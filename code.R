rm(list = ls())

library(tidyverse)

URL <- "https://dl.dropboxusercontent.com/s/cqf7cgxh91eoeyn/pitchfork.csv"
if(!file.exists("./pitchfork.csv")){download.file(URL, "./pitchfork.csv")}
p4kdata <- read.csv("./pitchfork.csv")

p4kdata <- p4kdata %>% select(-link, -review)
p4kdata$artist <- as.character(p4kdata$artist)
p4kdata$date <- as.Date(p4kdata$date, format = "%B %d %Y")
p4kdata <- p4kdata[order(desc(p4kdata$date)),]
p4kdata$author <- p4kdata$author %>% 
  gsub("\\s+|\\.|-|Ã©", "", .) 

p4kdata$author[p4kdata$author=="DrAndyBeta"] <- "AndyBeta"
p4kdata$author[p4kdata$author=="DrewGaerig"] <- "AndrewGaerig"
p4kdata$author[p4kdata$author=="StephenMDeusner"] <- "StephenDeusner"
p4kdata$author[p4kdata$author=="StephenMDuesner"] <- "StephenDeusner"
p4kdata$author[p4kdata$author=="MarkRichardSan"] <- "MarkRichardson"
p4kdata$author[p4kdata$author=="SavyReyesKulkarni"] <- "SabyReyesKulkarni"
p4kdata$author[p4kdata$author=="PaulAThompson"] <- "PaulThompson"
p4kdata$author[p4kdata$author=="SeanFennessy"] <- "SeanFennessey"
p4kdata$author[p4kdata$author=="AlexLindhart"] <- "AlexLinhardt"
p4kdata$author[p4kdata$author=="CoryDByrom"] <- "CoryByrom"
p4kdata$author[p4kdata$author=="MarcusJMoore"] <- "MarcusMoore"
p4kdata$author[p4kdata$author=="JeremyDLarson"] <- "JeremyLarson"

approxnames <- lapply(unique(p4kdata$author), agrep, unique(p4kdata$author), value = TRUE)
commonnames <- approxnames[lengths(approxnames) > 1]
unlist(commonnames)


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


PrevScorefn <- function(name){
  artist <<- p4kdata3[p4kdata3$artist==name,]
  y <<- NULL
  if(nrow(artist)==1){
    y <- NA
    oneartist <- cbind(artist, PrevScore = y)
  } else {
    for(x in 1:(nrow(artist)-1)){
      a <- artist[x+1,]
      b <- a$score
      y <- c(y,b)
    }
    y <- c(y, NA)
    oneartist <<- cbind(artist, PrevScore = y)
  }
}

p4kdata4 <- lapply(artistnames, PrevScorefn)
p4kdata4 <- bind_rows(p4kdata4)
head(p4kdata4, n=30)


PrevScoreAvgfn <- function(name){
  artist <<- p4kdata4[p4kdata4$artist==name,]
  y <<- NULL
  if(nrow(artist)==1){
    y <- NA
    oneartist <- cbind(artist, PrevScoreAvg = y)
  } else {
    for(x in 1:(nrow(artist)-1)){
      a <- artist[(x+1):nrow(artist),]
      b <- mean(a$score)
      y <- c(y,b)
    }
    y <- c(y, NA)
    oneartist <<- cbind(artist, PrevScoreAvg = y)
  }
}

p4kdata5 <- lapply(artistnames, PrevScoreAvgfn)  
p4kdata5 <- bind_rows(p4kdata5)  
head(p4kdata5, n=30)


PrevAuthorSamefn <- function(name){
  artist <<- p4kdata5[p4kdata5$artist==name,]
  y <<- NULL
  if(nrow(artist)==1){
    y <- FALSE
    oneartist <- cbind(artist, PrevAuthorSame = y)
  } else {
    for(x in 1:(nrow(artist)-1)){
      b <- identical(artist$author[x], artist$author[x+1])
      y <- c(y,b)
    }
    y <- c(y, FALSE)
    oneartist <<- cbind(artist, PrevAuthorSame = y)
  }
}

p4kdata6 <- lapply(artistnames, PrevAuthorSamefn)  
p4kdata6 <- bind_rows(p4kdata6)  
head(p4kdata6, n=30)

