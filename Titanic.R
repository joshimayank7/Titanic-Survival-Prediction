rm(list=ls())
require(dplyr)
require(Boruta)
require(regexr)
require(DescTools)
require(plyr)
require(MASS)
require(mice)
require(rpivotTable)
path = "C:\\Users\\Bp_mayank j joshi\\Desktop\\Kaggle\\Titanic\\"
setwd(path)

## loading total and test data

train <- read.csv("train.csv",header=T, na.strings=c(""," ","NA"))
md.pattern(train)
test<- read.csv("test.csv",header=T, na.strings=c(""," ","NA"))
sub_test <- test
md.pattern(test)

## creating HISTOGRAM of missing data

library(VIM)
aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing train_new","Pattern"))
aggr_plot_2 <- aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, ylab=c("Histogram of missing train_new","Pattern"))

# train = rbind.data.frame(train)

## DATA PREPARATION

train <- subset(train,select = -c(PassengerId,Cabin))
colnames(train)
train <- train[c(3:10,2,1)]
cols <- colnames(train)
cols
cols <- cols[1:9]
test <- subset(test,select = cols)

summary(train)
summary(test)
## creating a feature named SALUTATION

name_survival <- subset(train,select = c(Name,Survived))

x<- regexpr("\\.[^\\.]*$", train$Name)
y <- regexpr("\\,[^\\,]*$", train$Name)

train$salutation <- substr(train$Name,y+1,x)
uniq <- unique(train$salutation)


# y = data.frame(count(train$Name[grep("Miss.", train$Name)]))
# sum(y$freq)
survived_salutation <- subset(train, select = c(salutation,Survived))
tabl <- count(survived_salutation)
survived_salutation$salutation <- as.factor(survived_salutation$salutation)

level_3 <- ddply(survived_salutation, .(salutation), numcolwise(sum))  
cnt <- count(survived_salutation$salutation)
colnames(cnt) <- c("salutation","count")
m<-merge.data.frame(cnt,level_3,all.x=TRUE)

m$survival_rate = m$Survived/m$count

two_third <- 2/3
one_third <- 1/3



high<- m[(m$survival_rate>two_third),]
high$importance <- "HIGH"

medium <- m[(m$survival_rate<two_third & m$survival_rate>one_third),]
medium$importance <- "MEDIUM"

low<- m[(m$survival_rate<one_third),]
low$importance <- "LOW"

mapping_file <- rbind.data.frame(high,medium,low)
mapping_file <- mapping_file[,c(1,5)]

train$importance <- ""

## vlookup on mapping_file and train
mn <- c("salutation","importance")
train[mn] <- lapply(mn, function(x) mapping_file[[x]][match(train$salutation, mapping_file$salutation)])

## feature engineering on test data

s<- regexpr("\\.[^\\.]*$", test$Name)
t <- regexpr("\\,[^\\,]*$", test$Name)

test$salutation <- substr(test$Name,t+1,s)
test$importance <- ""
test[mn] <- lapply(mn, function(x) mapping_file[[x]][match(test$salutation, mapping_file$salutation)])


table(complete.cases(train$importance))

test[which(is.na(test$importance)),11] <- "HIGH"


## imputing missing AGES on the basis of SALUTATION

salu_age_train <- subset(train,select = c(salutation,Age))
salu_age_test <- subset(test,select = c(salutation,Age))

salu_age_total <- rbind.data.frame(salu_age_test[,c(1,2)],salu_age_train[,c(1,2)])

level_avg <- aggregate( Age ~ salutation, salu_age_total, mean )
level_med <- aggregate( Age ~ salutation, salu_age_total, median )

level_avg_med <- cbind.data.frame(level_avg,level_med[,2])
colnames(level_avg_med) <- c("salutation","mean","median")

train_age_na_rows <-  which(is.na(train$Age)) 
test_age_na_rows <- which(is.na(test$Age))

ab <- c("salutation","Age")
train[train_age_na_rows,ab] <- lapply(ab, function(x) level_avg[[x]][match(train$salutation[train_age_na_rows], level_avg$salutation)])
test[test_age_na_rows,ab] <- lapply(ab, function(x) level_avg[[x]][match(test$salutation[test_age_na_rows], level_avg$salutation)])

train <- train[,c(2:9,12,10)]
test <- test[,c(2:9,11)]

## get names of those columns which have NA values

colnames(train)[colSums(is.na(train)) > 0]
colnames(test)[colSums(is.na(test)) > 0]

## removing rows having NA values in EMBARKED column in TRAIN data

train <- na.omit(train)
## removing rows having NA values in FARE and importance column  in TEST data

test[which(is.na(test$Fare)),6] <- mean(test$Fare,na.rm = T)




## creating Family Size using SibSp and Parch

train$Family_Size <- train$SibSp + train$Parch
test$Family_Size <- test$SibSp + test$Parch

train <- subset(train,select = -c(SibSp,Parch))
test <- subset(test,select = -c(SibSp,Parch))

train = train[,c(1:7,9,8)]

str(train)

# ## finding out the percentage (%) of data missing
# 
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(train,2,pMiss)
# apply(test,2,pMiss)

## using MICE for imputation of NA values in AGE column

# md.pattern(train)

# tempData <- mice(train_new,m=5,maxit=10,meth='pmm',seed=500)
# methods(mice)
# summary(tempData)


aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing train_new","Pattern"))
aggr_plot_2 <- aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(test), cex.axis=.7, gap=3, ylab=c("Histogram of missing test_new","Pattern"))

summary(train)

## Using BARUTA to find out the most significant Features affecting the Survival of Passengers

library(Boruta)

set.seed(321)
boruta.train <- Boruta(Survived ~ . , data = train, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels), at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

boruta.df <- attStats(boruta.train)
class(boruta.df)
print(boruta.df)

## model fitting 
train_data <- subset(train,select = -Ticket)
train_data$Survived <- as.factor(train_data$Survived)
test_data <- subset(test,select = -Ticket)

to_be_trained <- train_data[1:800,]

to_be_tested <- train_data[801:889,]


###____________________________________________LOGISTIC REGRESSION____________________________________
##_________________________________________with CV ( cross validation )

library(caret)
# 
# glmModel <- train(Survived~. , train_data, method = 'glm', family = binomial, trControl = trainControl(method = 'cv', number = 5))
# summary(glmModel)
# PredTrain = predict(glmModel, newdata=to_be_tested, type="raw") 
# PredTrain <- as.data.frame(PredTrain)
# to_be_tested$Predicted = PredTrain[,1]
# confusionMatrix(to_be_tested$Predicted,to_be_tested$Survived)
# 
# accuracy = mean(to_be_tested$Predicted==to_be_tested$Survived)


##___________________________RANDOM FOREST_____________________________
library(randomForest)
train_data$importance<- as.factor(train_data$importance)

titanic.survival.train.rf = randomForest(Survived ~ ., data=train_data , ntree=6000, importance=TRUE)
titanic.survival.train.rf
plot(titanic.survival.train.rf)
importance(titanic.survival.train.rf)
to_be_tested$importance<- as.factor(to_be_tested$importance)
PredTrain = predict(titanic.survival.train.rf, newdata=to_be_tested[1:8], type="response") 
PredTrain <- as.data.frame(PredTrain)
to_be_tested$Predicted = PredTrain[,1]


confusionMatrix(to_be_tested$Predicted,to_be_tested$Survived)

accuracy = mean(to_be_tested$Predicted==to_be_tested$Survived)


# ##___________________________CONDITIONAL TREE_____________________________
# require(partykit)
# titanic.survival.train.ctree = ctree(Survived ~., data=train_data)
# titanic.survival.train.ctree
# plot(titanic.survival.train.ctree)
# PredTrain = predict(titanic.survival.train.ctree, newdata=to_be_tested[1:8], type="response") 
# PredTrain <- as.data.frame(PredTrain)
# to_be_tested$Predicted = PredTrain[,1]
# confusionMatrix(to_be_tested$Predicted,to_be_tested$Survived)
# accuracy = mean(to_be_tested$Predicted==to_be_tested$Survived)

#_____________________Predict Test
test_data$importance <- as.factor(test_data$importance)
PredTest = predict(titanic.survival.train.rf , newdata=test_data, type="response")

submission <- cbind.data.frame(sub_test$PassengerId,PredTest)
colnames(submission) <- c("PassengerId","Survived")
filename = 'Titanic Predictions.csv'

write.csv(submission,filename)



