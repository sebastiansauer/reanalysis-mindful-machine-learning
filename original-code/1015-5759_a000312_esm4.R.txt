##################
# This is the main source file for the paper "Mindful Machine Learning"
# Authors: Kurz, C., Sauer, S.
# Last Update: 2015-05-11
##################


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
