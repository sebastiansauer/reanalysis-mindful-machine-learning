##################
# This is the main source file for the paper "Mindful Machine Learning"
# Authors: Kurz, C., Sauer, S.
# Last Update: 2015-05-11
##################




##################
# Init the analysis for the paper "MML"
# Authors: Kurz, C., Sauer, S.
# Last Update: 2015-04-10
##################


MMLInit <- function(){
  
  library(caret)
  library(leaps)
  library(MASS)
  library(ada)
  library(kernlab)
  library(randomForest)
  library(ggplot2)
  library(reshape2)
  library(leaps)
  library(psych)
  library(effsize)
  library(boot)
  library(circlize)
  
  df.file.path <- paste0(path.data,main.df, separator = "")
  load(df.file.path)
  
  # read only items (independet variables) 
  # plus mindfulness practice (yes/no --> dependent variable)
  dat.fmi <- df[,4:18]
  
  # dat.fmi2 uses the DV as a string variable, 
  # whereas dat.fmi uses the DV as a numeric variable (0 vs 1)
  dat.fmi2 <- df[,3:18]
  dat.fmi2 <- dat.fmi2[,-2] # exlude "practice" variable
  # dat.fmi3 includes the sample variable (1 vs. 2)
  dat.fmi3 <- df[,c(1,3:18)]
  dat.fmi3 <- dat.fmi3[,-3] # exlude "practice" variable
  names(dat.fmi3)
  
  # create training sample and test sample 
  set.seed(42)
  inTraining <- createDataPartition(dat.fmi2$meditators, p = 0.8, list = FALSE)
  training <- dat.fmi2[inTraining, ]
  testing <- dat.fmi2[-inTraining, ]
  
  
  # 10 fold cross validation
  # 10 samples (k), 10 times repeated (n)
  fitControl <- trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    ## repeated ten times
    repeats = 10)
  
  
}




##################
# the machine learning algorithms for the paper "MML"
# Authors: Kurz, C., Sauer, S.
# Last Update: 2015-04-10
##################


MMLRun <- function(mml.df){
  
  # MACHINE LEARNING ALGORITHM
  # logistic regression
  set.seed(42)
  glmFit <- train(meditators ~ ., 
                  data = mml.df, 
                  method = "glm", 
                  trControl = fitControl, 
                  family = binomial)
  glmFit
  
  
  
  
  
  glmPred <- predict(glmFit, mml.df)
  glmConf <- confusionMatrix(glmPred, mml.df$meditators)
  glmConf$overall[2]
  
  conf.all[1, ] <- c("glm", glmConf$byClass[c(1,2)], glmConf$overall[2])
  print(conf.all[1, ])
  
  
  
  # same as above but use "sample" as predictor
  set.seed(42)
  glmFit2 <- train(meditators ~ ., 
                   data = dat.fmi3, 
                   method = "glm", 
                   trControl = fitControl, 
                   family = binomial)
  glmFit2
  str(glmFit2)
  
  glmPred2 <- predict(glmFit2, dat.fmi3)
  glmConf2 <- confusionMatrix(glmPred2, dat.fmi3$meditators)
  str(glmConf2)
  glmConf2$overall
  
  
  #unique(dat.fmi3$meditators)
  set.seed(42)
  glmFit3 <- glm(dat.fmi3$meditators ~ ., family=binomial(logit), data = dat.fmi3)
  summary(glmFit3)
  
  # MACHINE LEARNING ALGORITHM
  # boosting (general boosting machine/ Stochastic Gradient Boosting)
  
  # try out different parameters to identify best fit
  set.seed(42)
  gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                          n.trees = (1:30)*50,
                          shrinkage = 0.1)
  nrow(gbmGrid)
  
  
  set.seed(42)
  gbmFit <- train(meditators ~ ., data = mml.df,
                  method = "gbm",
                  trControl = fitControl,
                  verbose = FALSE,
                  tuneGrid = gbmGrid)
  gbmFit
  
  gbmPred <- predict(gbmFit, mml.df)
  gbmConf <- confusionMatrix(gbmPred, mml.df$meditators)
  gbmConf
  conf.all[2, ] <- c("gbm",gbmConf$byClass[c(1,2)], gbmConf$overall[2])
  print(conf.all[2, ])
  
  
  
  
  # MACHINE LEARNING ALGORITHM
  # quadratic discriminant analysis (qda)
  set.seed(42)
  qdaFit <- train(meditators ~ ., data = mml.df,
                  method = "qda",
                  trControl = fitControl,
                  verbose = FALSE)
  qdaFit
  # accuracy: .66
  qdaPred <- predict(qdaFit, mml.df)
  qdaConf <- confusionMatrix(qdaPred, mml.df$meditators)
  #qdaConf
  conf.all[3, ] <- c("qda",qdaConf$byClass[c(1,2)], qdaConf$overall[2])
  conf.all[3, ]
  
  
  
  # MACHINE LEARNING ALGORITHM
  # support vector machines with linear kernel
  set.seed(42)
  svmLinearFit <- train(meditators ~ ., data = mml.df,
                        method = "svmLinear",
                        trControl = fitControl,
                        verbose = FALSE)
  svmLinearFit
  # accuracy: .721
  
  svmLinearPred <- predict(svmLinearFit, mml.df)
  svmLinearConf <- confusionMatrix(svmLinearPred, mml.df$meditators)
  #svmLinearConf
  conf.all[4, ] <- c("svmLinear",svmLinearConf$byClass[c(1,2)], svmLinearConf$overall[2])
  conf.all[4, ]
  
  # support vector machines with polynomial kernel
  set.seed(42)
  svmPolyFit <- train(meditators ~ ., data = mml.df,
                      method = "svmPoly",
                      trControl = fitControl,
                      verbose = FALSE)
  mean(svmPolyFit$resample$Accuracy)
  # mean resample accuracy: .737
  
  svmPolyPred <- predict(svmPolyFit, mml.df)
  svmPolyConf <- confusionMatrix(svmPolyPred, mml.df$meditators)
  svmPolyConf
  conf.all[5, ] <- c("svmPoly",svmPolyConf$byClass[c(1,2)], svmPolyConf$overall[2])
  conf.all[5, ]
  
  
  
  # MACHINE LEARNING ALGORITHM
  # random forest
  set.seed(42)
  rfFit <- train(meditators ~ ., data = mml.df,
                 method = "rf",
                 trControl = fitControl,
                 verbose = FALSE)
  rfFit
  # accuracy: 
  # mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
  # 2    0.726     0.411  0.0758       0.164   
  # 8    0.708     0.373  0.0778       0.167   
  # 14   0.699     0.357  0.0718       0.154   
  
  rfPred <- predict(rfFit, mml.df)
  rfConf <- confusionMatrix(rfPred, mml.df$meditators)
  #rfConf
  conf.all[6, ] <- c("rf",rfConf$byClass[c(1,2)], rfConf$overall[2])
  conf.all[6, ]
  
  
  # MACHINE LEARNING ALGORITHM
  # neural net
  set.seed(42)
  nnFit <- train(meditators ~ ., data = mml.df,
                 method = "nnet",
                 trControl = fitControl,
                 verbose = FALSE)
  nnFit
  str(nnFit)
  
  # max. accuracy: .723
  nnPred <- predict(nnFit, mml.df)
  nnConf <- confusionMatrix(nnPred, mml.df$meditators)
  nnConf
  conf.all[7, ] <- c("nn",nnConf$byClass[c(1,2)], nnConf$overall[2])
  conf.all[7, ]
  
  
  # MACHINE LEARNING ALGORITHM
  # ada boost
  # cave: tages ages...
  set.seed(42)
  adaFit <- train(meditators ~ ., data = mml.df,
                  method = "ada",
                  trControl = fitControl,
                  verbose = FALSE)
  adaFit
  # accuracy: .270
  adaPred <- predict(adaFit, mml.df)
  adaConf <- confusionMatrix(adaPred, mml.df$meditators)
  adaConf
  conf.all[8, ] <- c("ada",adaConf$byClass[c(1,2)], adaConf$overall[2])
  conf.all[8, ]
  
  
  
  # MACHINE LEARNING ALGORITHM
  # k nearest neighbours
  
  set.seed(42)
  knnFit <- train(meditators ~ ., data = mml.df,
                  method = "kknn",
                  trControl = fitControl,
                  verbose = FALSE)
  knnFit
  
  # accuracy: 0.652
  knnPred <- predict(knnFit, mml.df)
  knnConf <- confusionMatrix(knnPred, mml.df$meditators)
  conf.all[9, ] <- c("knn",knnConf$byClass[c(1,2)], knnConf$overall[2])
  conf.all[9, ]
  
  
  
  # MACHINE LEARNING ALGORITHM
  # extreme learning machine
  set.seed(42)
  elmFit <- train(meditators ~ ., data = mml.df,
                  method = "elm",
                  trControl = fitControl,
                  verbose = FALSE)
  elmFit
  
  # Accuracy 0.644
  
  elmPred <- predict(elmFit, mml.df)
  elmConf <- confusionMatrix(elmPred, mml.df$meditators)
  conf.all[10, ] <- c("elm",elmConf$byClass[c(1,2)], elmConf$overall[2])
  conf.all[10, ]
  
  #Accuracy : 0.587           
  
  
  # save accuracy
  setwd(path.output)
  save(conf.all, file = "conf.all.Rda")
  
  # save model objects
  modelobj <- list(glmFit, gbmFit, qdaFit, svmLinearFit, svmPolyFit, rfFit, nnFit,
                   knnFit, elmFit)
  save(modelobj, file = "modelobj.Rda")
  
  # compare model results
  #   resamps <- resamples(list(neuralnet = nnFit,
  #                             lssvmLinear = svmLinearFit,
  #                             lssvmPoly = svmPolyFit,
  #                             GLM = glmFit,
  #                             RF  = rfFit,
  #                             gbm = gbmFit1,
  #                             qda = qdaFit,
  #                             gbm2 = gbmFit1))
  #   resamps
  #   
  #   print(summary(resamps))
  #   
  #trellis.par.set(theme1)
  #bwplot(resamps, layout = c(3, 3))
  
  
  results <- list(conf.all, modelobj)
  
  return(results)
  
  
}








### initialize
# initialize variables, set paths
path.data = "~/Documents/Forschung/Achtsamkeit_Studien/Mindf_Machine_Learning/Data"

path.output = "~/Documents/Forschung/Achtsamkeit_Studien/Mindf_Machine_Learning/output"
  
path.functions = "~/Documents/Forschung/Achtsamkeit_Studien/Mindf_Machine_Learning/Data"
  

main.df = "mindfor_df.RDA" # that's were the data come from

conf.all <- data.frame(matrix(nrow = 10, ncol = 4))
names(conf.all) <- c("model", "sensitivity", "specificity", "kappa")


MMLInit() # load libraries etc.

model.coeffs <- MMLRun(dat.fmi2) # compute models and model coefficients
# note that all functions are stored in separate syntax files


(conf.all <- model.coeffs[[1]]) # save confindence statistics in this variable

setwd(path.output)
save(model.coeffs[[1]], file = "model.coeffs.Rda")



### plot coefficients for accuracy

conf.all.melt <- melt(conf.all, id = "model") # melt to plot
conf.all.melt$value <- as.numeric(conf.all.melt$value)
names(conf.all.melt)[2] <- "Coefficient" # more intuitive name

head(conf.all.melt)

p.conf <- ggplot(conf.all.melt, aes(x = model, y = value, group = Coefficient, 
                                    fill = Coefficient)) + 
  geom_bar(stat = "identity", position = position_dodge()) + coord_flip()+
  scale_y_continuous(limits=c(-.7, 1)) 

p.conf

setwd(path.output)
ggsave(p.conf, file = "p.conf.pdf", width = 9, height = 9) # save it
ggsave(p.conf, file = "p.conf.eps", width = 9, height = 9) # save it
ggsave(p.conf, file = "p.conf.tiff", width = 9, height = 9) # save it



### plot densities for each group for each item
fmi.melt <- melt(dat.fmi, id = "practice")
fmi.melt$practice <- as.factor(fmi.melt$practice)
str(fmi.melt)

p.dens.plot <- ggplot(fmi.melt, aes(x = value)) + 
  geom_density(aes(group = practice, colour = practice, fill = practice), alpha = .3) +
  facet_wrap( ~ variable, ncol = 7) +
  theme(legend.position="bottom")

p.dens.plot

setwd(path.output)
ggsave(p.dens.plot, file = "p.dens.plot.pdf", width = 9, height = 9)
ggsave(p.dens.plot, file = "p.dens.plot.eps", width = 9, height = 9)
ggsave(p.dens.plot, file = "p.dens.plot.tiff", width = 9, height = 9)



### plot densities for each group scale mean

#first create vector of group membership
group.vec <- seq(1:276)
group.vec[inTraining] <- "training"
group.vec[-inTraining] <- "testing"


# create vector of mean value per items
items.means <- sapply(dat.fmi[,2:15], function(x) mean(x, na.rm = TRUE))

# create vector of items means per person
persons.means <- rowMeans(dat.fmi[,2:15], na.rm = T)

meditation.group <- dat.fmi2[,1]

# each person mean is signed as coming from a meditator or from a non-meditator

str(fmi.melt)

p.dens <- ggplot(fmi.melt, aes(x = value)) +
          geom_density(aes(group = practice, fill = practice),alpha = .3) +
           theme(legend.position = "bottom")
p.dens

setwd(path.output)
ggsave(p.dens, file = "p.dens.pdf", width = 9, height = 9)
ggsave(p.dens, file = "p.dens.eps", width = 9, height = 9)
ggsave(p.dens, file = "p.dens.tiff", width = 9, height = 9)


### glm between groups (sample by meditation.group) - overall level

# compute mean for all group combinations
ddply(dat.mean, c("group.vec", "meditation.group"), summarise, 
      MML.mean = mean(persons.means))

# t-Test
# whole sample
t.test(data = dat.mean, persons.means ~ meditation.group)
cohen.d(data = dat.mean, person.means ~ meditation.group)


# testing sample
t.test(data = dat.mean[dat.mean$group.vec == "training",], persons.means ~ 
         meditation.group)
cohen.d(data = dat.mean[dat.mean$group.vec == "training",], persons.means ~ 
         meditation.group)

# training sample
t.test(data = dat.mean[dat.mean$group.vec == "testing",], persons.means ~ 
         meditation.group)
cohen.d(data = dat.mean[dat.mean$group.vec == "testing",], persons.means ~ 
          meditation.group)




# glm
MML.glm <- glm(data = dat.mean, persons.means ~ meditation.group + group.vec + 
                 meditation.group * group.vec)

summary(MML.glm)



### plot Cohen's d for each item


fmi.summary <- describeBy(dat.fmi[ ,2:15], dat.fmi[,1], mat = TRUE)
# names(fmi.summary)
# tapply(fmi.summary$mean, fmi.summary$group1, mean)

# str(fmi.summary)
# setwd(path.output)
# write.csv(fmi.summary, file = "fmi.summary.csv")
# save(fmi.summary, file = "fmi.summary.Rda")


fmi.cohend <- sapply(dat.fmi[,2:15], function(x) cohen.d(x, dat.fmi[,1])$estimate)
fmi.cohend
fmi.cohend.df <- data.frame(fmi.cohend)
str(fmi.cohend)
fmi.cohend.overall <- mean(fmi.cohend)
fmi.cohend.overall



library(ggplot2)
fmi.cohend.df$item <- 1:14
head(fmi.cohend.df)
p.cohend <- ggplot(fmi.cohend.df, aes(x = as.factor(item), y = fmi.cohend)) + 
  geom_point(size = 5, colour ="cornflowerblue") + xlab("item") + ylab("Cohen d") +
  geom_bar(stat = "identity", width = .05) +
  coord_flip()
p.cohend


setwd(path.output)
ggsave(p.cohend, file = "p.cohend.pdf", width = 9, height = 9)
ggsave(p.cohend, file = "p.cohend.eps", width = 9, height = 9)
ggsave(p.cohend, file = "p.cohend.tiff", width = 9, height = 9)


### plot differences in mean between groups for each item


# repeat the function FUN over serveral colums of the dataframe
# in order to calculate mean fmi level per item per group
fmi.group.mean <- aggregate(dat.fmi[ ,2:15], by = list(dat.fmi2[ ,1]),
                   FUN = mean, na.rm = T)

str(fmi.group.mean)
# The function "diff" calculates the differences between all consecutive values of a vectors

fmi.diff.mean <- sapply(fmi.group.mean[,2:15], diff)

fmi.diff$item <- 1:14
names(fmi.diff)[1] <- "difference"

p.mean.diff <- ggplot(fmi.diff, aes(x = as.factor(item), y = difference)) + geom_bar(stat = "identity") +
  coord_flip() + xlab("items") + ylab("difference between mindfulness practitioners and non-practitioners")

p.mean.diff

setwd(path.output)
ggsave(p.mean.diff, file = "p.mean.diff.pdf")
ggsave(p.mean.diff, file = "p.mean.diff.eps")
ggsave(p.mean.diff, file = "p.mean.diff.tiff")




### central tendency and dispersion of accuracy for all 10 models

overall.sens <- mean(as.numeric(conf.all$sensitivity), na.rm = T)
overall.spec <- mean(as.numeric(conf.all$specificity), na.rm = T)
overall.kappa <- mean(as.numeric(conf.all$kappa), na.rm = T)

overall.kappa

overall.sens.md <- median(as.numeric(conf.all$sensitivity), na.rm = T)
overall.spec.md <- median(as.numeric(conf.all$specificity), na.rm = T)
overall.kappa.md <- median(as.numeric(conf.all$kappa), na.rm = T)


overall.sens.md
overall.spec.md
overall.kappa.md


overall.sens.sd <- sd(as.numeric(conf.all$sensitivity), na.rm = T)
overall.spec.sd <- sd(as.numeric(conf.all$specificity), na.rm = T)

overall.kappa.sd <- sd(as.numeric(conf.all$kappa), na.rm = T)


# median absolute deviation
overall.sens.mad <- mad(as.numeric(conf.all$specificity))
overall.spec.mad <- mad(as.numeric(conf.all$sensitivity))

overall.sens.mad
overall.spec.mad


# range
range.sens <- range(as.numeric(conf.all$sensitivity))
range.spec <- range(as.numeric(conf.all$specificity))

range.sens[2] - range.sens[1]
range.spec[2] - range.spec[1]
                                   


# comparison of classical psychometric models with machine learning models

conf.comparison <- data.frame(type = factor(c("classical", "ML", "ML-wo.ada")),
                              sensitivity = c(0, 0, 0),
                              specificity = c(0, 0, 0),
                              kappa = c(0, 0, 0))

#sens
conf.comparison[1,2] <- mean(as.numeric(
  conf.all$sensitivity[conf.all$model %in% c("glm")]), na.rm = T)
conf.comparison[1,2]

conf.comparison[2,2] <- mean(as.numeric(
  conf.all$sensitivity[!(conf.all$model %in% c("glm"))]), na.rm = T)
conf.comparison[2,2]

# ...without ada, the model with poorest performance
conf.comparison[3,2] <- mean(as.numeric(
  conf.all$sensitivity[!(conf.all$model %in% c("glm", "ada"))]), na.rm = T)
conf.comparison[3,2]


#spec
conf.comparison[1,3] <- median(as.numeric(
  conf.all$specificity[conf.all$model %in% c("glm")]), na.rm = T)
conf.comparison[1,3]

conf.comparison[2,3] <- median(as.numeric(
  conf.all$specificity[!(conf.all$model %in% c("glm"))]), na.rm = T)
conf.comparison[2,3] 

# ...without ada, the model with poorest performance
conf.comparison[3,3]  <- median(as.numeric(
  conf.all$specificity[!(conf.all$model %in% c("glm", "ada"))]), na.rm = T)
conf.comparison[3,3] 



#kappa
conf.comparison[1,4] <- median(as.numeric(
  conf.all$kappa[conf.all$model %in% c("glm")]), na.rm = T)
conf.comparison[1,4]

conf.comparison[2,4] <- median(as.numeric(
  conf.all$kappa[!(conf.all$model %in% c("glm"))]), na.rm = T)
conf.comparison[2,4] 

# ...without ada, the model with poorest performance
conf.comparison[3,4]  <- median(as.numeric(
  conf.all$kappa[!(conf.all$model %in% c("glm", "ada"))]), na.rm = T)
conf.comparison[3,4] 

conf.comparison$mean.all <- colMeans(conf.comparison[2:4])
conf.comparison$mean.all


conf.comparison.melt <- melt(conf.comparison, id = "type")
conf.comparison.melt
comp.conf.plot <- ggplot(conf.comparison.melt, aes(x = variable, y = value)) + 
  geom_bar(stat = "identity") + facet_wrap(~ type)
comp.conf.plot

setwd(path.output)
ggsave(comp.conf.plot, file = "comp.conf.plot.jpg")
ggsave(comp.conf.plot, file = "comp.conf.plot.pdf")


### item importance of the RF model (model of best accuracy)
mean(rfFit$finalModel$importance)
median(rfFit$finalModel$importance)




### plot circos
# compute mean correlations across all 14 items
fmi.cor.mat <- cor(dat.fmi[,-1])
fmi.cor.mat

# grand mean of correlations
fmi.cor.mat[fmi.cor.mat == 1] <- NA
fmi.cor.mat
mean.cor <- mean(fmi.cor.mat, na.rm = T)
mean.cor <- mean(fmi.cor.mat, na.rm = T)
mean.cor
mean.cor.vect <- apply(fmi.cor.mat, 2, function(x) mean(x, na.rm = T))
mean.cor.wo13 <- mean(mean.cor.vect[-13])
mean.cor.wo13 
mean.cor.vect <- order(mean.cor.vect)
mean.cor.vect


fmi.cor.mat <- fmi.cor.mat * 100 # multiply with 100 is needed for circos plot
fmi.cor.mat

dummy <- fmi.cor.mat

fmi.cor.mat[fmi.cor.mat < 40] <- 0 # suppress correlations below .4
fmi.cor.mat[fmi.cor.mat > 99] <- 0 # suppress perfect correlation
fmi.cor.mat[fmi.cor.mat < 0] <- abs(fmi.cor.mat) # change negative correlation into positive
fmi.cor.mat


library(circlize)
circos.clear()
circos.par(start.degree = 90)
chordDiagram(fmi.cor.mat)
