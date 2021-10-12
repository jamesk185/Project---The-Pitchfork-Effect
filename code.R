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
p4kdata$role <- p4kdata$role %>% 
  gsub("\\s$", "", .) %>%
  as.factor(.)
p4kdata$genre[p4kdata$genre==""] <- NA

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

p4kdata$artist[p4kdata$artist==""] <- "NA"


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
    oneartist <- cbind(artist, PrevScore = y)
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
    oneartist <- cbind(artist, PrevScoreAvg = y)
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
    oneartist <- cbind(artist, PrevAuthorSame = y)
  }
}

p4kdata6 <- lapply(artistnames, PrevAuthorSamefn)  
p4kdata6 <- bind_rows(p4kdata6)  
head(p4kdata6, n=30)


PrevAuthorSameTotalfn <- function(name){
  artist <<- p4kdata6[p4kdata6$artist==name,]
  y <<- NULL
  if(nrow(artist)==1){
    y <- 0
    oneartist <- cbind(artist, PrevAuthorSameTotal = y)
  } else {
    for(x in 1:(nrow(artist)-1)){
      b <- sum(artist$author[x]==artist$author[(x+1):nrow(artist)])
      y <- c(y,b)
    }
    y <- c(y, 0)
    oneartist <- cbind(artist, PrevAuthorSameTotal = y)
  }
}

p4kdata7 <- lapply(artistnames, PrevAuthorSameTotalfn)  
p4kdata7 <- bind_rows(p4kdata7)  
head(p4kdata7, n=30)


TimeSincePrevfn <- function(name){
  artist <<- p4kdata7[p4kdata7$artist==name,]
  y <<- NULL
  if(nrow(artist)==1){
    y <- NA
    oneartist <- cbind(artist, TimeSincePrev = y)
  } else {
    for(x in 1:(nrow(artist)-1)){
      b <- artist$date[x]-artist$date[x+1]
      y <- c(y,b)
    }
    y <- c(y, NA)
    oneartist <- cbind(artist, TimeSincePrev = y)
  }
}

p4kdata8 <- lapply(artistnames, TimeSincePrevfn)
p4kdata8 <- bind_rows(p4kdata8)
head(p4kdata8, n=30)


PrevTwoScoreChangefn <- function(name){
  artist <<- p4kdata8[p4kdata8$artist==name,]
  y <<- NULL
  if(nrow(artist) <= 2){
    y <- NA
    oneartist <- cbind(artist, PrevTwoScoreChange = y)
  } else {
    for(x in 1:(nrow(artist)-2)){
      b <- artist$score[x+1]/artist$score[x+2]
      y <- c(y,b)
    }
    y <- c(y, NA, NA)
    oneartist <- cbind(artist, PrevTwoScoreChange = y)
  }
}

p4kdata9 <- lapply(artistnames, PrevTwoScoreChangefn)
p4kdata9 <- bind_rows(p4kdata9)
head(p4kdata9, n=30)


p4kdata9 <- p4kdata9 %>% mutate(PrevTwoScoreAppreciated = PrevTwoScoreChange > 1)
head(p4kdata9, n=30)


p4kdata9 <- p4kdata9 %>% mutate(GenreElectronic = grepl("Electronic", p4kdata9$genre),
                                GenreExperimental = grepl("Experimental", p4kdata9$genre),
                                GenreFolk = grepl("Folk", p4kdata9$genre),
                                GenreCountry = grepl("Country", p4kdata9$genre),
                                GenreGlobal = grepl("Global", p4kdata9$genre),
                                GenreRock = grepl("Rock", p4kdata9$genre),
                                GenreMetal = grepl("Metal", p4kdata9$genre),
                                GenreRandB = grepl("R&B", p4kdata9$genre),
                                GenrePop = grepl("Pop", p4kdata9$genre),
                                GenreRap = grepl("Rap", p4kdata9$genre),
                                GenreJazz = grepl("Jazz", p4kdata9$genre))
head(p4kdata9, n=30)


saveRDS(p4kdata9, "./p4knewdata.rds")

