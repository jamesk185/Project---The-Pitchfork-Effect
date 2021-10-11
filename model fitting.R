library(caret)

rm(list = ls())

p4knewdata <- readRDS("./p4knewdata.rds")
training <- readRDS("./p4ktraining.rds")
testing <- readRDS("./p4ktesting.rds")
finaltest <- readRDS("./p4kfinaltest.rds")

PREVSCOREONLYfit <- lm(score~PrevScore, data=training)
PREVSCOREONLYpred <- predict(PREVSCOREONLYfit, testing)
mean(abs(PREVSCOREONLYpred-testing$score))

PREVSCOREAVGONLYfit <- lm(score~PrevScoreAvg, data=training)
PREVSCOREAVGONLYpred <- predict(PREVSCOREAVGONLYfit, testing)
mean(abs(PREVSCOREAVGONLYpred-testing$score))

TWOPREVSCOREfit <- lm(score~PrevScore+PrevScoreAvg, data=training)
TWOPREVSCOREpred <- predict(TWOPREVSCOREfit, testing)
mean(abs(TWOPREVSCOREpred-testing$score))

set.seed(211009)
control <- trainControl(method="cv", number=3, verboseIter=F)

TREEfit <- train(score~PrevScore+PrevBNMTotal+PrevScoreAvg+PrevTotal+TimeSincePrev+PrevAuthorSame+PrevAuthorSameTotal,
                 method="rpart", data=training, tuneLength = 50, trControl=control)
TREEpred <- predict(TREEfit, testing)
mean(abs(TREEpred-testing$score))

RFfit <- train(score~PrevScore+PrevBNMTotal+PrevScoreAvg+PrevTotal+TimeSincePrev+PrevAuthorSame+PrevAuthorSameTotal,
               method="rf", data=training, trControl=control)
RFpred <- predict(RFfit, testing)
mean(abs(RFpred-testing$score))

GBMfit <- train(score~PrevScore+PrevBNMTotal+PrevScoreAvg+PrevTotal+TimeSincePrev+PrevAuthorSame+PrevAuthorSameTotal,
                method="gbm", data=training, trControl=control, verbose=FALSE)
GBMpred <- predict(GBMfit, testing)
mean(abs(GBMpred-testing$score))

