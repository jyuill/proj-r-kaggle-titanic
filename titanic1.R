### Intro to Kaggle via Titanic dataset, pt 1
## based on Data Science Dojo tutorial
## https://www.youtube.com/watch?v=Zx2TguRHrJE


## Get training data
titanic.train <- read.csv("input/train.csv", stringsAsFactors = FALSE)
str(titanic.train)
summary(titanic.train)
head(titanic.train)

library(ggplot2)

plot1 <- ggplot(titanic.train, aes(x=Sex))+geom_bar(stat="count")
facet1 <- facet_grid(.~Survived)
facet2 <- facet_grid(Pclass~.~Survived)

plot1
plot1+facet1
plot1+facet1+facet2
plot1+facet2

## Get test data
titanic.test <- read.csv("input/test.csv", stringsAsFactors = FALSE)
str(titanic.test)
summary(titanic.test)
head(titanic.test)

### Combine data for cleaning
## check medians
median(titanic.train$Age, na.rm=TRUE)
median(titanic.test$Age, na.rm=TRUE)
## create new column
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
## check number of columns
ncol(titanic.train)
ncol(titanic.test) ## one col less
## check column names
names(titanic.train)
names(titanic.test)

## add Survived col to test
titanic.test$Survived <- NA

## new dataset combined
titanic.full <- rbind(titanic.train, titanic.test)
418+891 ## check number of rows
nrow(titanic.full)
## create table to show number of rows in each category for IsTrainSet
table(titanic.full$IsTrainSet)

## check cols for missing values (NA)
summary(titanic.full) ## Age, Fare; strings with missing/empty values may not show up

## fix Embarked
table(titanic.full$Embarked) ## indicates 2 items with no category
table(is.na(titanic.full$Embarked)) ## doesn't show as NA
table(is.null(titanic.full$Embarked)) ## doesn't show as NULL
table(titanic.full$Embarked=='') ## shows up as ''
## finding rows with missing values
titanic.full[titanic.full$Embarked=='',]
## check distribution of values again to see which is most common
table(titanic.full$Embarked) ## S is most common (mode)
## replace missing values with S
titanic.full[titanic.full$Embarked=='',"Embarked"] <- "S"
table(titanic.full$Embarked) ## check: no blanks, 2 more 'S'

## fix Age
## check/clean age
table(is.na(titanic.full$Age)) ## lots missing
## replace with median > very primitive approach, but works
age.replace <- median(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.replace

## fix Fare
table(is.na(titanic.full$Fare)) ## lots missing
## replace with median > very primitive approach, but works
fare.replace <- median(titanic.full$Fare, na.rm=TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.replace

## categorical casting
str(titanic.full)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
str(titanic.full)
## may also want to cast Sibsp and Parch as.ordinal

### Split data back out to train and test sets
## clean training set
titanic.train2 <- titanic.full[titanic.full$IsTrainSet==TRUE,]
## clean test set -> using dplyr to achieve same as above
library(dplyr)
titanic.test2 <- titanic.full %>% filter(IsTrainSet==FALSE)

## Survived as factor
## convert Survived on training data only - no NAs
## makes it clear that this is classification problem, not regression, etc
titanic.train2$Survived <- as.factor(titanic.train2$Survived)

### BUILD PREDICTIVE MODEL
## select predictors
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

library(randomForest)
titanic.model <- randomForest(formula(survived.formula), data=titanic.train2, ntree=500, mtry=3, nodesize=0.01*nrow(titanic.test2))

## list of features used in model -> not sure how this is used
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

## make prediction based on the model applied to test data
Survived <- predict(titanic.model, newdata=titanic.test2)
## see results of the model
Survived

## get passenger ids from test set
PassengerId <- titanic.test2$PassengerId
## create data frame with passenger ids
output.df <- as.data.frame(PassengerId)
## add survived column from prediction above - assumes same order as passenger ids in test2
output.df$Survived <- Survived 

## save file
write.csv(output.df, "output/prediction1.csv", row.names = FALSE)
