#data for the wine data sets can be found on the UCI Machine Learning Repository:
#http://archive.ics.uci.edu/ml/datasets/Wine+Quality

setwd("~/Documents/R files") #set working directory

library_loading <- c("caTools", "e1071", "kknn", "pROC")
lapply(library_loading, require, character.only = T) #load packages
#sampling, svm, knn, roc

#1

vinotinto <-read.csv("winequality-red.csv", sep = ";")
vinoblanco <- read.csv("winequality-white.csv", sep = ";") #import the 2 wine data sets

summary(vinotinto)
summary(vinoblanco) #look at basic stats

summary(is.na(vinotinto))
summary(is.na(vinoblanco)) #search for missing values

table(vinotinto$quality)
qualitynumbers1<- table(vinotinto$quality)
barplot(qualitynumbers1, main = "Red Wine by Quality", xlab = "Quality number") #visualize tinto
#naive model tinto = 0.43 (rounded). All records = class 5

table(vinoblanco$quality)
qualitynumbers2<- table(vinoblanco$quality)
barplot(qualitynumbers2, main = "White Wine by Quality", xlab = "Quality number") #visualize blanco
#naive model blanco = 0.45 (rounded). All records = class 6

#2 classification
# thus the dv = quality must be a factor

vinotinto$quality <- as.factor(vinotinto$quality)
vinoblanco$quality <- as.factor(vinoblanco$quality) #make quality factor type

set.seed(703)
sample = sample.split(vinotinto, SplitRatio = .80)
traintinto = subset(vinotinto, sample == TRUE)
testtinto = subset(vinotinto, sample == FALSE) #create train and test tinto

set.seed(202)
sample = sample.split(vinoblanco, SplitRatio = .80)
trainblanco = subset(vinoblanco, sample == TRUE)
testblanco = subset(vinoblanco, sample == FALSE) #create train and test blanco

#svm tinto

svm.tinto <- svm(quality ~., data = traintinto, type = "C-classification", kernel = "polynomial") #svm model tinto
svm.t.results <- predict(svm.tinto, testtinto[,-12]) #svm prediction tinto

table.tinto1 <- table(pred = svm.t.results, true = testtinto[,12]) #comparison matrix tinto
table.tinto1

auc(testtinto[,12], as.numeric(svm.t.results)) #roc accuracy for tinto

#svm blanco

svm.blanco <- svm(quality ~., data = trainblanco, type = "C-classification", kernel = "linear") #svm model blanco
svm.b.results <- predict(svm.blanco, testblanco[,-12]) #svm prediction blanco

table.blanco1 <- table(pred = svm.b.results, true = testblanco[,12]) #comparison matrix blanco
table.blanco1

auc(testblanco[,12], as.numeric(svm.b.results)) #roc accuracy for blanco

#knn tinto

knn.tinto <- kknn(quality ~.,traintinto, testtinto, k= 9, kernel = "inv") 
summary(knn.tinto)

table.tinto2 <- table(pred = predict(knn.tinto,testtinto),true = testtinto$quality)
table.tinto2 #comparison matrix

accuracy.tinto <- (sum(diag(table.tinto2)))/sum(table.tinto2)
accuracy.tinto #accuracy %

#knn blanco

knn.blanco <- kknn(quality ~.,trainblanco, testblanco, k= 11, kernel = "inv") 
summary(knn.blanco)

table.blanco2 <- table(pred = predict(knn.blanco,testblanco),true = testblanco$quality)
table.blanco2 #comparison matrix

accuracy.blanco <- (sum(diag(table.blanco2)))/sum(table.blanco2)
accuracy.blanco #accuracy %
