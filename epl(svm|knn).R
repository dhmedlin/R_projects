#Business Understanding

#English Premier League 2011/12 Goal Analysis
#The following analysis was performed on data provided by Opta on the 2011 - 2012 English Premier League (EPL) season. 
#This link will take you to the data: https://datahub.io/dataset/uk-premier-league-match-by-match-2011-2012.  
#Each row of data represents statistics on a single player from one game of the 2011 - 2012 EPL season. 
#201 different statistics were collected for each player for every game they played in during the season.  
#The other 9 fields were data about the player, the team they played on, the team they were playing against, and the date of the game.

#Data Understanding 

setwd("~/Documents/R files") #set working directory

epl <- read.csv("epl.11-12.csv", na.strings = c("")) #import the data and make blanks = NA

summary(is.na(epl)) #search for NA's in the data 

summary(epl$Goals) #look at the distribution of goals

table(epl$Goals) #table of goals

goalnumbers<- table(epl$Goals)
barplot(goalnumbers, main = "Goals Scored per Game", xlab = "Number of Goals Scored", ylab = "Number of Records")

#Data Preparation

epl.mod <- epl #create a duplicate of epl

attach(epl.mod) 

class(epl.mod$Goals) #look up goals class type

epl.mod$Goals <- as.factor(epl.mod$Goals) #change goals to factor type

class(epl.mod$Goals) #confirm the previous line of code worked

epl.mod$Player.ID <- NULL
epl.mod$Date <- NULL
epl.mod$Player.Surname <- NULL
epl.mod$Player.Forename <- NULL
epl.mod$Team <- NULL
epl.mod$Team.Id  <- NULL
epl.mod$Opposition  <- NULL
epl.mod$Opposition.id  <- NULL
epl.mod$Venue  <- NULL
epl.mod$First.Goal  <- NULL
epl.mod$Winning.Goal  <- NULL
epl.mod$Penalties.Taken  <- NULL
epl.mod$Penalty.Goals  <- NULL
epl.mod$Penalties.Saved  <- NULL
epl.mod$Penalties.Off.Target  <- NULL
epl.mod$Penalties.Not.Scored  <- NULL
epl.mod$Direct.Free.kick.Goals  <- NULL
epl.mod$Goals.from.Inside.Box  <- NULL
epl.mod$Goals.from.Outside.Box  <- NULL
epl.mod$Headed.Goals  <- NULL
epl.mod$Left.Foot.Goals  <- NULL
epl.mod$Right.Foot.Goals  <- NULL
epl.mod$Other.Goals  <- NULL
epl.mod$Goals.Open.Play  <- NULL
epl.mod$Goals.from.Corners  <- NULL
epl.mod$Goals.from.Throws  <- NULL
epl.mod$Goals.from.Direct.Free.Kick  <- NULL
epl.mod$Goals.from.Set.Play  <- NULL
epl.mod$Goals.from.penalties  <- NULL
epl.mod$Attempts.from.Penalties.on.target  <- NULL
epl.mod$Attempts.from.Penalties.off.target  <- NULL
epl.mod$Goals.as.a.substitute  <- NULL #drop columns

#Modeling

library(caTools) #load package for sampling
library(e1071) #load package for svm
library(kknn) #load package for knn
library(pROC) #load package for roc

set.seed(703)
sample = sample.split(epl.mod, SplitRatio = .80)
train.epl.mod = subset(epl.mod, sample == TRUE)
test.epl.mod = subset(epl.mod, sample == FALSE) #create train and test data frames

#classification problem
#two chosen algorithms = knn and svm

knn.epl.mod <- kknn(Goals ~.,train.epl.mod, test.epl.mod, k= 1, kernel = "rectangular") #knn model

summary(knn.epl.mod)

table.knn <- table(pred = predict(knn.epl.mod,test.epl.mod),true = test.epl.mod$Goals)
table.knn #comparison matrix to visualize the data

accuracy.knn <- (sum(diag(table.knn)))/sum(table.knn)
accuracy.knn #the accuracy of the KNN model


svm.epl.mod <- svm(Goals ~., data = train.epl.mod, type = "C-classification", kernel = "polynomial") #svm model
svm.output <- predict(svm.epl.mod, test.epl.mod[,-7]) #svm prediction

table.svm <- table(pred = svm.output, true = test.epl.mod[,7]) #comparison matrix
table.svm

auc(test.epl.mod[,7], as.numeric(svm.output)) #roc accuracy
