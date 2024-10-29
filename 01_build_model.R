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
# Load predicts for doing k-fold
library(predicts)

# load data
presAbsSF <- readRDS(file = "presAbsSF.rds")
presAbsDF <- readRDS(file = "presAbsDF.rds")
predictorsModern <- readRDS(file = "predictorsModern.rds")
predictorsFuture <- readRDS(file = "predictorsFuture.rds")

## STOP. Here there be dragons


# now we can get a training/test split
fold <- folds(x = presAbsDF,
              k = 5,
              by = presAbsDF$present)
# our first (simple) split can just be using fold==1 to test the
# model and all the other folds to train it
testing <- presAbsDF[fold == 1, ]
training <- presAbsDF[fold != 1, ]

# and we can build a model -- e.g., a glm with binom
simpleModel <- glm(present ~ ., data = presAbsDF, family = binomial())
# and get predictions from it -- note that this is built from just
# the training data which is a small set of the whole data set


simpleModelFits <- predict(predictorsModern, simpleModel, type = "response")
# this is SpatRaster showing the prob of spp being present
simpleModelFits
ggplot() + geom_histogram(data=simpleModelFits, mapping = aes(x=lyr1))
# let's make it categorical
simpleModelFitsCategorical <- as.factor(ifel(simpleModelFits > 0.5, TRUE, FALSE))
# and do a simple plot
ggplot() + geom_spatraster(data=simpleModelFitsCategorical)

