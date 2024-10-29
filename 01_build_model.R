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


simpleModelFits <- predict(bioClim, simpleModel, type = "response")
# this is SpatRaster showing the prob of spp being present
simpleModelFits
ggplot() + geom_histogram(data=simpleModelFits, mapping = aes(x=lyr1))
# let's make it categorical
simpleModelFitsCategorical <- as.factor(ifel(simpleModelFits > 0.5, TRUE, FALSE))
# and do a simple plot
ggplot() + geom_spatraster(data=simpleModelFitsCategorical)

# now recall we built that model using
