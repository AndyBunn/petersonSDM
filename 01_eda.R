rm(list=ls())
# Load tidyverse for data manipulation (dplyr, ggplot2, etc.)
library(tidyverse)
library(ggridges)
# load the pres/abs data for some exploratory work
presAbsDF <- readRDS(file = "presAbsDF.rds")
names(presAbsDF)
ggplot(data=presAbsDF,
       mapping = aes(x = AnnualMeanTmp, y = SpeciesStatus,
                     fill = SpeciesStatus,
                     height = after_stat(density))) +
  geom_density_ridges(stat = "density", alpha=0.5) +
  scale_fill_viridis_d() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() + theme(legend.position = "none")

# let's look at all the means by SpeciesStatus
# set number of bootstrap means we want
m <- 1000
nSamps <- nrows(presAbsDF)
# initiate list to hold means
foo <- list()

for (i in 1:m){
  # Step 1: sample with replacement -- get equal samples for pres/abs
  rows2get <- sample(1:nSamps,size=nSamps,replace=T)
  # Step 2: get means of the samples
  aSampEst <- mean(aSamp)
  cSampEst <- mean(cSamp)
  gSampEst <- mean(gSamp)
  # Step 3: store means by appending to the tibble
  foo[[i]] <- tibble(AdelieMeans = aSampEst,
                     ChinstrapMeans = cSampEst,
                     GentooMeans = gSampEst)
}

# convert the list to a single tibble
penBoot <- bind_rows(foo)
