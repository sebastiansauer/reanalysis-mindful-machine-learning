library(caret)
set.seed(998)


dat2 <- dat

folds <- createMultiFolds(dat2$practice, k = 10, times = 10) ## 10 rep 10-fold CV for performance evaluation

inner_res <- trainControl(method = "cv", number = 10) ## 10-fold CV for hyperparameter tuning

mod_list <- lapply(folds, function(x) { # train models for each of the 100 training sets
  train(practice ~ ., data = dat2[x,], 
        method = "rf", 
        trControl = inner_res,
        verbose = FALSE)
})

pred_list <- mapply(function(x, y) { # compute predictions on each of the 100 test sets
  predict(x, dat2[-y,])}, 
  x = mod_list, y = folds)

perf_sens_spec_list <- mapply(function(x, y) { # calculate performance measures I
  confusionMatrix(x, dat2[-y, "practice", drop = TRUE])$byClass[c(1,2)]}, 
  x = pred_list, y = folds)

perf_accuracy_kappa_list <- mapply(function(x, y) { # calculate performance measures II
  confusionMatrix(x, dat2[-y, "practice", drop = TRUE])$overall[c(1,2)]}, 
  x = pred_list, y = folds)

# average performance across the 100 test sets
mean(perf_sens_spec_list["Sensitivity", ])
mean(perf_sens_spec_list["Specificity", ])
mean(perf_accuracy_kappa_list["Accuracy", ])
mean(perf_accuracy_kappa_list["Kappa", ])