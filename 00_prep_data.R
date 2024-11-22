################################################################################
#
# AGB Nov 2024
#
# A script that takes presence data as lat/long points from MP and fetches
# bioClim (present and future) and topo data at 30 sec res for
# a (user-defined) box around those points
#
# We create pseudo-absence data to match the presence data above an
# elevation threshold and make sure that absences aren't on the same cells
# as the presence data.
#
# We then export the presence/absence data and the bricks that we'll use for
# the modeling. These bricks use the bioclim variables as well as topo and
# tree cover data as predictors.
#
# Inputs:
# Chariclea_Final_Thin.csv
# Outputs:
#   presAbsSF.rds -- presAbs data plus predictors as class sf (mapping is easy)
#   presAbsDF.rds -- presAbs data plus predictors as stright data.frame
#   predictorsModern.rds -- the brick of modern climate and topo stuff
#   predictorsFuture.rds -- the brick of future climate and topo stuff
#
################################################################################
# Clear the R environment of all objects to start fresh
rm(list=ls())
# set a seed for reprodicibility of the random sampling
set.seed(3625)
# Load tidyverse for data manipulation (dplyr, ggplot2, etc.)
library(tidyverse)
# Load sf for handling and visualizing spatial data
library(sf)
# Load terra for working with raster (grid) data
library(terra)
# Load tidyterra to combine raster data processing with tidyverse workflows
library(tidyterra)
# Load tmap for creating thematic maps, supports both static and interactive maps
library(tmap)
# Load geodata to access global climate and geographical datasets
library(geodata)

# Load presence data (species observation) from CSV file
presence <- read_csv("Chariclea_Final_Thin.csv")

# Convert 'presence' data to an sf object with spatial coordinates (longitude, latitude)
presenceSF <- st_as_sf(presence, coords = c("Longitude","Latitude"), crs = 4326)

# Set tmap to interactive mode ('view') for creating a quick interactive map
tmap_mode("view")
# Define the shape layer (presenceSF) for tmap and plot points as symbols on the map
tm_shape(presenceSF) +
  tm_symbols()

# Fetch and prepare climate data to overlay with species presence data
# we will make a data directory called "climateData" if it doesn't exist.
# note that this happens at the root level of the project.
data_path <- "./spatialData"
if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
  message("Directory created at: ", data_path)
} else {
  message("Directory already exists at: ", data_path)
}

# Now we get the bioClim variables from world clim. Note that this
# is a big ask the first time you do it since the data are global at
# 30 sec res. Many GB of data.

# But after the data are initially
# downloaded they live on your machine and don't need to be downloaded again
# provided you are in the same project
# After it's downloaded we will clip it to to a more reasonable area
#
# more info on variables here:
# https://www.worldclim.org/data/bioclim.html
bioClim <- worldclim_global(var = "bio",
                            res = 0.5,
                            path = data_path)
# note the class (SpatRaster) and that this is a 19 layer cube
bioClim

# Here are the layer names
# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (×100)
# BIO4 = Temperature Seasonality (standard deviation ×100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter

bioClimNames <- c("AnnualMeanTmp", "MeanDiurnalRange", "Isothermality", "TmpSeasonality",
                  "MaxTmpWarmestMonth", "MinTmpColdestMonth", "TmpAnnualRange",
                  "MeanTmpWettestQtr", "MeanTmpDriestQtr", "MeanTmpWarmestQtr",
                  "MeanTmpColdestQtr", "AnnualPrcp","PrcpWettestMonth",
                  "PrcpDriestMonth", "PrcpSeasonality", "PrcpWettestQtr",
                  "PrcpDriestQtr", "PrcpWarmestQtr", "PrcpColdestQtr")

# add names to bioClim
names(bioClim) <- bioClimNames

# Now we get the future climate data from cmip6.
# you'll want to think about time, model, and ssp choice
# there are a zillion models, four scenarios, and time to
# consider.
cmip6 <- cmip6_world(model = "HadGEM3-GC31-LL",
                     ssp = "245",
                     time = "2061-2080",
                     var = "bioc",
                     res = 0.5,
                     path = data_path)
# add in the names
names(cmip6) <- bioClimNames

# Let's grab the elev data too. We will use it as a predictor and to
# constrain the psuedo absence point generation. We don't want ridiculous
# absence points in the lowlands
elev <- elevation_global(res = 0.5,
                         path = data_path)
names(elev) <- "elev"

# we can get slope and aspect as predictors as well. Be aware of the perils of
# aspect as a predictor though -- 360==0 so typically aspect is transformed to
# a categorical variable (N,NE,E,SE,S,SW,W,NW) or in the N Hemi to be
# "southness" -- kind of a poorman's radiation proxy. I'll do both.

slope <- terrain(elev, v = "slope", unit = "degrees")  # Slope in degrees
aspect <- terrain(elev, v = "aspect", unit = "degrees") # Aspect in degrees

# Define breaks and labels for cardinal directions
# Aspect in degrees: 0 = North, 90 = East, 180 = South, 270 = West
categories <- data.frame(
  from = c(0, 22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5),
  to   = c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 360),
  value = 1:9,
  categoricalDirection = c("North", "Northeast", "East", "Southeast", "South",
            "Southwest", "West", "Northwest", "North")
)

# Create a categorical SpatRaster from aspect
aspect_cat <- classify(aspect, rcl = categories[, 1:3], right = FALSE,others=9)
levels(aspect_cat) <- categories[,3:4]

# Transform aspect to proxy solar radiation 1 is S, 0 are EW, and -1 is N
# Convert degrees to radians: radians = degrees * pi / 180
transformed_aspect <- cos(aspect * pi / 180)
names(transformed_aspect) <- "southness"

# let's put all the topographic data together. I'll not going to include raw
# aspect here because it's a dangerous predictor variable.
# Use aspect_cat and/or transformed_aspect as predictors
topo <- c(elev,slope,aspect,aspect_cat,transformed_aspect)

# Get landcover (it's native as 30 sec values). With `geodata` we can get pct
# of "trees", "grassland", "shrubs", "cropland", "built", "bare", "snow",
# "water", "wetland", "mangroves", "moss". I'll use percentage of tree
# cover which will work best there I think. These are continuous data which are
# handy as well.
pctTree <- landcover(var = "trees",
                     path = data_path)

# We don't want to model the entire world, right?
# Let's get the extent of the data and expand it by 5 degrees for clipping
# the climate data to something reasonable.
# Note that 5 deg might or might not be a good number
presenceSF_bbox <- st_bbox(presenceSF)
presenceSF_bbox_expanded <- presenceSF_bbox
presenceSF_bbox_expanded[1] <- presenceSF_bbox_expanded[1] - 5
presenceSF_bbox_expanded[2] <- presenceSF_bbox_expanded[2] - 5
presenceSF_bbox_expanded[3] <- presenceSF_bbox_expanded[3] + 5
presenceSF_bbox_expanded[4] <- presenceSF_bbox_expanded[4] + 5

# Convert 'presence' data to an sf object with spatial coordinates (longitude, latitude)
presenceSF_bbox_expandedSF <- st_as_sfc(presenceSF_bbox_expanded)

# take a look -- static map this time
ggplot() +
  geom_sf(data=presenceSF_bbox_expandedSF) +
  geom_sf(data=presenceSF)

# now we can clip the raster data to that box
bioClim <- crop(x = bioClim, y = presenceSF_bbox_expandedSF)
topo <- crop(x = topo, y = presenceSF_bbox_expandedSF)
pctTree <- crop(x = pctTree, y = presenceSF_bbox_expandedSF)
cmip6 <- crop(x = cmip6, y = presenceSF_bbox_expandedSF)

# note the dimensions now -- much smaller
bioClim

# and take a look -- use elevation as base
ggplot() +
  geom_spatraster(data=topo$elev) +
  geom_sf(data=presenceSF) +
  scale_fill_hypso_c()

# Let's add bioclim, topo, pctTree data together to make the
# brick that we will use to build the model
predictorsModern <- c(bioClim,topo,pctTree)
dim(predictorsModern)

# and we will make a brick for future climate to use later. Note that this
# is the cmip6 data plus the topo data and pctTree as above.
predictorsFuture <- c(cmip6,topo,pctTree)
dim(predictorsFuture)
# note that the dimensions match

# with our climate data done, we now need to generate the
# pseudo absence data. Let's do this in a way that constrains the points to
# be 1. high elevation and 2. not on top of existing presence data

# Let's get a minimum elevation
presenceSF$elev <- extract(elev,presenceSF)$elev
# constrain to >5% of obecserved elevations to avoid funnies in the data
minElev <- quantile(presenceSF$elev, probs = 0.05)
minElev # about 1100m here

# now create a mask of areas in the box that are > minElev
absenceMask <- ifel(elev > minElev, elev, NA)
# We will clip this so that the absence data are in areas near presence data
absenceMask <- crop(absenceMask,presenceSF_bbox)
# And mask out points that have observations.
# This is a little cryptic, but basically we are getting the cell IDs where
# there are presence observations. There are simpler ways (or less confusing)
# ways of doing this probably.
# Get the cells in absenceMask where there are observations in presenceSF
presenceSF_cells <- extract(absenceMask, presenceSF, cells=T)$cell
# and set cells in absenceMask to NA
values(absenceMask)[presenceSF_cells] <- NA
# Now make a sf object that is in the elev range and excludes places where
# there are actual observations. Generate an equal number of pseudo absences
pseudoAbsenceSF <- spatSample(absenceMask,size = nrow(presenceSF),
                              na.rm=TRUE, as.points=TRUE)
# annoyingly, this is class spatVector -- make into sf
pseudoAbsenceSF <- as_sf(pseudoAbsenceSF)

# Combine the pseudoAbsenceSF data and presenceSF data to the make one sf
# object coded as pres/abs by adding a new column
presenceSF$present <- "present"
pseudoAbsenceSF$present <- "absent"
# and combining -- note we will drop elevation from these data.frames
# we used elev here to get the right threshold for generating the data
# but we don't need it in the data frame now.
presAbsSF <- rbind(presenceSF %>% select(-elev),
                   pseudoAbsenceSF %>% select(-elev))
presAbsSF$present <- as.factor(presAbsSF$present)
# note that we have the same number of present and absent values
table(presAbsSF$present)

# let's take a look
tm_shape(presAbsSF) +
  tm_symbols(col = "present",style="cat",alpha=0.5)

# now that we have somewhat realistic presence and absence data we can
# extract the climate data from the and add it to presAbsSF
presAbsSF <- as_sf(extract(predictorsModern,presAbsSF,bind=TRUE))
presAbsSF # look at all those lovely predictors
# And convert to a plain data.frame
presAbsDF <- st_drop_geometry(presAbsSF)

# we are now ready to move on to model development.
# let's save what I think we'll need
saveRDS(object = presAbsSF, file = "presAbsSF.rds")
saveRDS(object = presAbsDF, file = "presAbsDF.rds")
# we will jack up the compression here to get the files <100MB so they go to
# the github repo smoothly. bzip2 gets them small but takes longer
saveRDS(object = predictorsModern, file = "predictorsModern.rds", compress = "bzip2")
saveRDS(object = predictorsFuture, file = "predictorsFuture.rds", compress = "bzip2")

