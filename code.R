rm(list = ls())

library(tidyverse)

URL <- "https://dl.dropboxusercontent.com/s/cqf7cgxh91eoeyn/pitchfork.csv"
if(!file.exists("./pitchfork.csv")){download.file(URL, "./pitchfork.csv")}
p4kdata <- read.csv("./pitchfork.csv")
p4kdata <- p4kdata %>% select(-link, -review)
p4kdata$artist <- as.character(p4kdata$artist)
p4kdata$date <- as.Date(p4kdata$date, format = "%B %d %Y")

artistnames <- unique(p4kdata$artist)


p4kdata2 <- data.frame(NULL)
PrevTotal <- function(name){
  artist <<- p4kdata[p4kdata$artist==name,]
  y <<- NULL
  for(x in 1:nrow(artist)){
    z <- sum(artist$date < artist$date[x])
    y <- c(y,z)
  }
  oneartist <<- cbind(artist, PrevTotal = y)
  p4kdata2 <<- rbind(p4kdata2, oneartist)
}





PrevTotal("Andrew Bird")
p4kdata2

PrevTotal("Aphex Twin")
p4kdata2

sapply(artistnames, PrevTotal)
p4kdata2

with(artistnames, PrevTotal())







