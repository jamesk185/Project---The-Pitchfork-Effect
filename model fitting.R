library(caret)
library(tidyverse)

rm(list = ls())

p4knewdata <- readRDS("./p4knewdata.rds")
training <- readRDS("./p4ktraining.rds")
testing <- readRDS("./p4ktesting.rds")
finaltest <- readRDS("./p4kfinaltest.rds")

corTable <- readRDS("./corTable.rds")
corTable <- corTable[-grep("PrevTwoScore", rownames(corTable)),,drop=FALSE]
interestedvars2 <- paste(rownames(corTable), collapse="+")
interestedvars2

## run step function to see which predictors should be included in the model
step(lm(score~PrevScoreAvg+PrevScore+PrevBNMTotal+GenreExperimental+PrevAuthorSameTotal+PrevAuthorSame+PrevTotal+GenreJazz+GenreGlobal+GenreFolk+GenreCountry+GenreMetal+TimeSincePrev+GenreRandB+GenrePop+GenreRap+GenreElectronic+GenreRock,
        data=training),
     direction = "both")

stepfit <- lm(formula = score ~ PrevScoreAvg + PrevScore + PrevBNMTotal + 
                GenreExperimental + PrevAuthorSameTotal + GenreRandB + GenreRap + 
                GenreElectronic + GenreRock, data = training)
steppred <- predict(stepfit, testing)
mean(abs(steppred-testing$score))

## linear regression models predicting score
PREVSCOREONLYfit <- lm(score~PrevScore, data=training)
PREVSCOREONLYpred <- predict(PREVSCOREONLYfit, testing)
mean(abs(PREVSCOREONLYpred-testing$score))

PREVSCOREAVGONLYfit <- lm(score~PrevScoreAvg, data=training)
PREVSCOREAVGONLYpred <- predict(PREVSCOREAVGONLYfit, testing)
mean(abs(PREVSCOREAVGONLYpred-testing$score))

TWOPREVSCOREfit <- lm(score~PrevScoreAvg+PrevScore, data=training)
TWOPREVSCOREpred <- predict(TWOPREVSCOREfit, testing)
mean(abs(TWOPREVSCOREpred-testing$score))


## machine learning models predicting score
set.seed(211009)
control <- trainControl(method="cv", number=3, verboseIter=F)
training2 <- training %>% select(-artist, -album, -genre, -date, -author, -role, -bnm, -label, -release_year)

TREEfit <- train(score~., method="rpart", data=training2, tuneLength = 50, trControl=control)
TREEpred <- predict(TREEfit, testing)
mean(abs(TREEpred-testing$score))

## WARNING may take a while to run
RFfit <- train(score~., method="rf", data=training2, trControl=control)
RFpred <- predict(RFfit, testing)
mean(abs(RFpred-testing$score))

GBMfit <- train(score~., method="gbm", data=training2, trControl=control, verbose=FALSE)
GBMpred <- predict(GBMfit, testing)
mean(abs(GBMpred-testing$score))

