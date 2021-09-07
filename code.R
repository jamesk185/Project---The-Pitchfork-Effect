rm(list = ls())

library(tidyverse)

URL <- "https://dl.dropboxusercontent.com/s/cqf7cgxh91eoeyn/pitchfork.csv"
if(!file.exists("./pitchfork.csv")){download.file(URL, "./pitchfork.csv")}
p4kdata <- read.csv("./pitchfork.csv")

p4kdata <- p4kdata %>% select(-link, -review)
p4kdata$artist <- as.character(p4kdata$artist)
p4kdata$date <- as.Date(p4kdata$date, format = "%B %d %Y")
p4kdata <- p4kdata[order(desc(p4kdata$date)),]


artistnames <- unique(p4kdata$artist)

PrevTotalfn <- function(name){
  artist <<- p4kdata[p4kdata$artist==name,]
  y <<- NULL
  for(x in 1:nrow(artist)){
    z <- sum(artist$date < artist$date[x])
    y <- c(y,z)
  }
  oneartist <<- cbind(artist, PrevTotal = y)
}

p4kdata2 <- lapply(artistnames, PrevTotalfn)
p4kdata2 <- bind_rows(p4kdata2)
head(p4kdata2, n=30)


PrevBNMTotalfn <- function(name){
  artist <<- p4kdata2[p4kdata2$artist==name,]
  y <<- NULL
  for(x in 1:nrow(artist)){
    a <- artist[artist$date < artist$date[x],]
    b <- sum(a$bnm)
    y <- c(y,b)
  }
  oneartist <<- cbind(artist, PrevBNMTotal = y)
}

p4kdata3 <- lapply(artistnames, PrevBNMTotalfn)
p4kdata3 <- bind_rows(p4kdata3)
head(p4kdata3, n=30)



