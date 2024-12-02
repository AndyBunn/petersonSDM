rm(list=ls())
# Load tidyverse for data manipulation (dplyr, ggplot2, etc.)
library(tidyverse)
# Load randomForest for doing variable importance
library(randomForest)
# Load caret for doing simple xvalidation
library(caret)
# Load glmnet for GLM with regularization
library(glmnet)
# Load sf for handling and visualizing spatial data
library(sf)
# Load terra for working with raster (grid) data
library(terra)
# Load tidyterra to combine raster data processing with tidyverse workflows
library(tidyterra)

# load data
presAbsDF <- readRDS(file = "presAbsDF.rds")


# Define control for RFE
control <- rfeControl(functions = rfFuncs,  # Random Forest functions
                      method = "cv",        # Cross-validation
                      number = 10,          # 10-fold CV
                      repeats = 10)         # repeat this 10 times


# Use recursive feature elimination (RFE) to select important variables
# based on a machine learning algorithm (e.g., Random Forest).
# set a seed for reproducibility of the random sampling
set.seed(3625)
rfeResults <- rfe(x = presAbsDF[,-1], # use all predictors
                  y = presAbsDF$SpeciesStatus,
                  sizes = c(3,5,7,10,13,15,18),  # Number of predictors to test
                  rfeControl = control)
# This tells us that the variables that have the
# best crossvalidated classification accuracy (using kappa)
rfeResults
# these are they
selected_vars <- predictors(rfeResults)
selected_vars
# I found with repeated runs that the accuracy and kappa stay about the same
# (~0.72, ~0.45), the number of useful predictors varies from 5 to as many as 13
# but the top five (always?) include:
# Slope, TreePct, MeanTmpWarmestQtr, MeanTmpDriestQtr, AnnualMeanTmp


# we will use glmnet to do the predictions -- can discuss choice of algorithms
# and tuning at some point.
# Here we can use a simple train/test split.
trainIndex <- createDataPartition(presAbsDF$SpeciesStatus, p = 0.7, list = FALSE)
trainData <- presAbsDF[trainIndex, ]
testData <- presAbsDF[-trainIndex, ]

trainDataSelectedVars <- trainData[, c("SpeciesStatus", selected_vars)]
testDataSelectedVars <- testData[, c("SpeciesStatus", selected_vars)]

train_control <- trainControl(method = "cv",
                              number = 10,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)  # Use AUC

# Fit the GLMNet model
glmnet_model <- train(
  SpeciesStatus ~ .,  # Formula for caret
  data = trainDataSelectedVars,
  method = "glmnet",     # GLM with regularization
  trControl = train_control,
  metric = "ROC"         # Optimize for AUC
)

# Predict probabilities on the test set
predictions <- predict(glmnet_model, newdata = testDataSelectedVars, type = "prob")

# Compute ROC and AUC
library(pROC)
roc_curve <- roc(as.numeric(testDataSelectedVars$SpeciesStatus) - 1, predictions[, "present"])
auc_value <- auc(roc_curve)
# Print and plot results
auc_value
plot(roc_curve, main = "ROC Curve for GLMNet Model")

# Extract model coefficients
coef(glmnet_model$finalModel, glmnet_model$bestTune$lambda)

# Variable importance
importance <- varImp(glmnet_model, scale = TRUE)
plot(importance)


# Predict using the fitted glmnet model
presAbsSF <- readRDS(file = "presAbsSF.rds")
predictorsModern <- readRDS(file = "predictorsModern.rds")
predictorsFuture <- readRDS(file = "predictorsFuture.rds")

predictorsModernSelectedVars <- predictorsModern %>%
  select(all_of(selected_vars))

# First a binary prediction (discrete)
presenceAbsence <- predict(
  object = predictorsModernSelectedVars,
  na.rm=TRUE,
  model = glmnet_model)

# Then a probability of "present" (continuous)
presenceProbability <- predict(
  object = predictorsModernSelectedVars,
  na.rm=TRUE,
  model = glmnet_model,
  type = "prob",      # Predict probabilities
  index = 2           # Select the class "1" (presence) probabilities
)

ggplot() +
  geom_spatraster(data=presenceAbsence) +
  scale_fill_viridis_d(name = "Prediction") +
  theme_minimal()

ggplot() +
  geom_spatraster(data=presenceProbability) +
  scale_fill_viridis_c(name = "p",na.value = "transparent") +
  labs(title = "Presence") +
  theme_minimal()




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

