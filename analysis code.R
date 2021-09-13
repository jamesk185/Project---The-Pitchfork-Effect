library(caret)

TEST <- p4kdata9 %>% filter(date < "2019-02-01", date > "2018-01-01")
testing <- p4kdata9 %>% filter(date <= "2018-01-01", date > "2014-01-01")
training <- p4kdata9 %>% filter(date <= "2014-01-01", date > "2004-01-01")
nrow(training)/(nrow(training)+nrow(testing))

training2 <- na.omit(training)
testing2 <- na.omit(testing)
TEST2 <- na.omit(TEST)

any(!is.finite(training2$PrevTwoScoreChange))
training2 <- training2[-c(511,2130),]

nrow(training2)/(nrow(training2)+nrow(testing2))




cor(training2$score, training2$PrevTotal)
cor(training2$score, training2$PrevBNMTotal)
cor(training2$score, training2$PrevScore)
cor(training2$score, training2$PrevScoreAvg)
cor(training2$score, training2$PrevAuthorSame)
cor(training2$score, training2$PrevAuthorSameTotal)
cor(training2$score, training2$TimeSincePrev)
cor(training2$score, training2$PrevTwoScoreChange)
cor(training2$score, training2$PrevTwoScoreAppreciated)

x <- lm(score~PrevScoreAvg+PrevScore+PrevAuthorSame+PrevTwoScoreAppreciated, data=training2)
y <- predict(x, testing2)
mean(abs(y - testing2$score))

control <- trainControl(method="cv", number=3, verboseIter=F)
RFfit <- train(score~PrevScoreAvg+PrevScore+PrevAuthorSame+PrevTwoScoreAppreciated, method="rf", data=training2, trControl=control)
RFpred <- predict(RFfit, testing2)
mean(abs(RFpred - testing2$score))

TREEfit <- train(score~PrevScoreAvg+PrevScore+PrevAuthorSame+PrevTwoScoreAppreciated, method="rpart", data=training2, tuneLength = 50, trControl=control)
TREEpred <- predict(TREEfit, testing2)
mean(abs(TREEpred - testing2$score))

GBMfit <- train(score~PrevScoreAvg+PrevScore+PrevAuthorSame+PrevTwoScoreAppreciated, method="gbm", data=training2, trControl=control, verbose=FALSE)
GBMpred <- predict(GBMfit, testing2)
mean(abs(GBMpred - testing2$score))

f <- TEST2[TEST2$artist=="Animal Collective",]
predict(GBMfit, f)
predict(x, f)

g <- TEST2[TEST2$album=="Age Of",]
predict(GBMfit, g)
predict(RFfit, g)
predict(TREEfit, g)
predict(x, g)

