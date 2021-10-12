library(tidyverse)
library(gridExtra)

## removing albums by various artists
p4knewdata <- readRDS("./p4knewdata.rds")
p4knewdata <- p4knewdata[p4knewdata$artist!="Various Artists",]
saveRDS(p4knewdata, "./p4knewdata.rds")

p4knewdata <- readRDS("./p4knewdata.rds")

## creating a training set, testing set and set for a final test
## two variables removed as they have two many na values
TEST <- p4knewdata %>% filter(date < "2019-02-01", date > "2018-01-01") %>% select(-PrevTwoScoreAppreciated, -PrevTwoScoreChange)
testing <- p4knewdata %>% filter(date <= "2018-01-01", date > "2014-01-01") %>% select(-PrevTwoScoreAppreciated, -PrevTwoScoreChange)
training <- p4knewdata %>% filter(date <= "2014-01-01", date > "2004-01-01") %>% select(-PrevTwoScoreAppreciated, -PrevTwoScoreChange)
nrow(training)/(nrow(training)+nrow(testing))

## removing na values
training <- na.omit(training)
testing <- na.omit(testing)
TEST <- na.omit(TEST)
nrow(training)/(nrow(training)+nrow(testing))

saveRDS(training, "./p4ktraining.rds")
saveRDS(testing, "./p4ktesting.rds")
saveRDS(TEST, "./p4kfinaltest.rds")


rm(list = ls())

p4knewdata <- readRDS("./p4knewdata.rds")
training <- readRDS("./p4ktraining.rds")
testing <- readRDS("./p4ktesting.rds")
finaltest <- readRDS("./p4kfinaltest.rds")

## how variable the scores are in general
var(p4knewdata$score)

mean(p4knewdata$score)
## how variable the mean is
var(p4knewdata$score)/length(p4knewdata$score)

## 95% of scores fall in this range
quantile(p4knewdata$score, probs=c(0.025,0.975))

## confirming what percentage of scores fall within two standard deviations of the mean
upper <- mean(p4knewdata$score)+2*sd(p4knewdata$score)
lower <- mean(p4knewdata$score)-2*sd(p4knewdata$score)
inbounds <- p4knewdata %>% filter(score > lower, score < upper) %>% nrow
inbounds/nrow(p4knewdata)

## a density plot of the distribution of the scores to confirm normality with vertical lines two sds either side of mean
p4knewdata %>% ggplot(aes(x=score)) +
  geom_histogram(aes(y=..density..), binwidth=.5, color="black", fill="white") +
  geom_density(alpha=.2, fill="#BB1111", bw=0.2) + 
  geom_vline(xintercept=lower) +
  geom_vline(xintercept=upper)

## the prevtwoscorechange variable has infinite values
## remove them so that the correlation can be computed
p4knewdata[which(is.infinite(p4knewdata$PrevTwoScoreChange)),]
p4knewdata2 <- p4knewdata[-which(is.infinite(p4knewdata$PrevTwoScoreChange)),]

## a vector of the names of the variables we are interested in 
interestedvars <- p4knewdata %>% select(-score, -artist, -album, -genre, -date, -author, -role, -bnm, -label, -release_year) %>% names()

## a table of the correlations between score and each other variable respectively
corTable <- NULL
for(x in interestedvars){
  y <- p4knewdata2 %>% select(score, x) %>% na.omit() %>% cor()
  corTable <- rbind(corTable, y)
}
corTable <- corTable[-seq(1, 2*length(interestedvars), by = 2),-2]
corTable <- corTable %>% as.data.frame() %>% arrange(desc(.))
colnames(corTable) <- "Correlation"
corTable

saveRDS(corTable, "./corTable.rds")
  
## scatter plots of each variable against score with regression lines
g1 <- p4knewdata %>% ggplot(aes(x=score, y=PrevTotal)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g2 <- p4knewdata %>%
  ggplot(aes(x=score, y=PrevBNMTotal)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g3 <- p4knewdata %>% ggplot(aes(x=score, y=PrevScore)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g4 <- p4knewdata %>% ggplot(aes(x=score, y=PrevScoreAvg)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g5 <- p4knewdata %>% mutate(PrevAuthorSame = as.numeric(PrevAuthorSame)) %>%
  ggplot(aes(x=score, y=PrevAuthorSame)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g6 <- p4knewdata %>% ggplot(aes(x=score, y=PrevAuthorSameTotal)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g7 <- p4knewdata %>% ggplot(aes(x=score, y=TimeSincePrev)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g8 <- p4knewdata %>% ggplot(aes(x=score, y=PrevTwoScoreChange)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

g9 <- p4knewdata %>% mutate(PrevTwoScoreAppreciated = as.numeric(PrevTwoScoreAppreciated)) %>%
  ggplot(aes(x=score, y=PrevTwoScoreAppreciated)) +
  geom_point(position=position_jitter(h=0.1, w=0.1), alpha=.1, color="#BB1111") +
  geom_smooth(method='lm', col="black")

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, nrow = 3)

