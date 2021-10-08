library(caret)
library(tidyverse)

p4knewdata <- readRDS("./p4knewdata.rds")

TEST <- p4knewdata %>% filter(date < "2019-02-01", date > "2018-01-01")
testing <- p4knewdata %>% filter(date <= "2018-01-01", date > "2014-01-01")
training <- p4knewdata %>% filter(date <= "2014-01-01", date > "2004-01-01")
nrow(training)/(nrow(training)+nrow(testing))

training <- na.omit(training)
testing <- na.omit(testing)
TEST <- na.omit(TEST)
nrow(training)/(nrow(training)+nrow(testing))

training <- training[-which(!is.finite(training$PrevTwoScoreChange)),]

saveRDS(training, "./p4ktraining.rds")
saveRDS(testing, "./p4ktesting.rds")
saveRDS(TEST, "./p4kfinaltest.rds")


rm(list = ls())

p4knewdata <- readRDS("./p4knewdata.rds")
training <- readRDS("./p4ktraining.rds")
testing <- readRDS("./p4ktesting.rds")
finaltest <- readRDS("./p4kfinaltest.rds")


