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
# Load randomForest for doing a rf prediction
library(randomForest)

# load data
presAbsSF <- readRDS(file = "presAbsSF.rds")
presAbsDF <- readRDS(file = "presAbsDF.rds")
predictorsModern <- readRDS(file = "predictorsModern.rds")
predictorsFuture <- readRDS(file = "predictorsFuture.rds")

# There are typically a few steps to approaching modeling.
# The first usually consists on deciding about the model type
# e.g., a glm or a ml approach. Sometimes this involves selecting
# predictors or evaluating a model for a balance between underfitting
# and over fitting. I'll leave that distinction up to you of course.
# In this case we are predicting a binary response. That is a
# classification exercise as opposed to a regression exercise. There are
# many kinds of models to predict classes and I'd steer you to the caret
# package for ways of doing classification (pres/abs) in semi automated
# ways. But until then...

# When we build the model we aim to use various subsets of data to train
# the model and then predict it over subsets of the data that the model
# hasn't seen before. This is testing.

# the approach we can use here is to use a certain number (k) of folds of the
# data. Randomly we assign each each row in the data to a fold. Each fold
# serves as training data and all the other folds are used to test the model
# We record the efficacy of the model and repeat so that each row gets a chance
# to be both training and testing data. Let's make the folds. We can do this
# "by hand" with the sample function or use any number of other functions in
# ml or regression packages to help us. I'll show a way here using the folds
# function predicts package.

# create five folds in the data
fold <- folds(x = presAbsDF,
              k = 5,
              by = presAbsDF$present)

# our first (simple) split can just be using fold==1 to test the
# model and all the other folds to train it

## Fold 1
testing <- presAbsDF[fold == 1,]
training <- presAbsDF[fold != 1,]

# note that all or data are accounted for
length(presAbsDF$present)
length(training$present) + length(testing$present)
table(training$present)
table(testing$present)

# and we can build a model -- e.g., a randomForest model
simpleModel <- randomForest(present ~ ., data = training)
simpleModel # note the table that shows the accuracy of the predictions
# we can also see which variables are important to the model:
varImpPlot(simpleModel)
partialPlot(simpleModel, as.data.frame(training), elev)

# how well does the model do on the withheld data?
fitsFold1 <- predict(simpleModel,testing)
# let's make table similar to that made by randomForest
# make df and mutate in the error
accuracyFold1 <- table(fitsFold1,testing$present)

# and get predictions from it -- note that this is built from just
# the training data which is a small set of the whole data set

simpleModelFits <- predict(predictorsModern, simpleModel, type = "response")
# this is SpatRaster showing the prob of spp being present
simpleModelFits
# and do a simple plot
ggplot() + geom_spatraster(data=simpleModelFits)

