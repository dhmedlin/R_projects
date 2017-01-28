library_loading <- c("Hmisc", "missForest", "caTools", "mice", "dplyr")
lapply(library_loading, require, character.only = T) #load packages

data("iris") 
iris #classic iris dataset

Species.column <- iris$Species #create a copy of the Species column

iris.missing <- prodNA(iris, noNA = 0.1) #create missing values

iris.missing$Species <- Species.column #add data back to the Species column

summary(iris.missing) #check for NA's

set.seed(101)
sample = sample.split(iris.missing, SplitRatio = .70)
train = subset(iris.missing, sample == TRUE)
test = subset(iris.missing, sample == FALSE) #create train and test

test$Species <- c("") #remove data from the species column

summary(test) #check for NA's

full <- rbind(train, test) #row bind train and test

#imputation

ini <- mice(full, maxit=0, pri=F)
pred <- ini$pred
pred["Species",] <- 0
meth1 <- c("pmm", "pmm", "pmm", "pmm", "")
imp <- mice(full, meth=meth1, pred=pred, pri=F)
imp$imp$Species 
#impute all columns except Species but keep Species as a imputer variable

summary(imp)

#next look at the distribution of the imputed data vs the original data
#as well as pooling prior to modeling
