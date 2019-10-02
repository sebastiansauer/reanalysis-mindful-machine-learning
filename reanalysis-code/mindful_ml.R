# CODE BY FLORIAN PARGENT







# reanalyze data from:

# Sauer, S., Buettner, R., Heidenreich, T., Lemke, J., Berg, C., & Kurz, C. (2018). 
# Mindful machine learning: Using machine learning algorithms to predict the practice of mindfulness. 
# European Journal of Psychological Assessment, 34(1), 6-13.
# http://dx.doi.org/10.1027/1015-5759/a000312

# data was downloaded from:
# https://econtent.hogrefe.com/doi/suppl/10.1027/1015-5759/a000312/suppl_file/1015-5759_a000312_esm5.csv

# load data



dat = read.csv("original-data/1015-5759_a000312_esm5.csv")
# remove X1 variable
dat = dat[, -1]
# code practice as factor
dat$practice = factor(dat$practice)


# analyze data with mlr

# for a tutorial see:
# https://mlr.mlr-org.com/articles/tutorial/benchmark_experiments.html

library(mlr)
task = makeClassifTask(data = dat, target = "practice", positive = "1")
glm = makeLearner("classif.logreg")
rf = makeLearner("classif.randomForest")
rdesc = makeResampleDesc("RepCV", folds = 10, reps = 10, predict = "both")
mes = list(
  # test performance measures
  tpr, tnr, kappa, 
  # training performance measures
  setAggregation(tpr, train.mean), setAggregation(tnr, train.mean), setAggregation(kappa, train.mean)
  )
set.seed(42)
bm = benchmark(learners = list(glm, rf), tasks = task, resamplings = rdesc, measures = mes)
bm
# performance reported in the paper seems to be training performance!


# analyze data with caret:

# DISCLAIMER: I don't use caret myself so there might be some errors here.

library(caret)
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(42)
rfFit = train(practice ~ ., data = dat, method = "rf", trControl = fitControl)
rfFit
# performance seems roughly comparable to mlr

# perhaps training/insample performance was accidentally computed like this?
predictions = predict(rfFit, dat) # make predictions on the complete dataset
res = cbind(dat, predictions)
confusionMatrix = confusionMatrix(res$predictions, res$practice) # compute insample performance
confusionMatrix
# this looks like the values reported in the paper