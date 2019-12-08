## Jixing Jacey Man HW6 CS544
## Part 1
library(stringr)
file <-"http://kalathur.com/cs544/data/lincoln.txt"
words <-scan(file, what=character())

#a 
grep("[[:punct:]]", levels(file), value = TRUE)
words[str_detect(words,"[:punct:]")]

#b
newwords <- words1 <- str_replace(words,"[:punct:]","")
newwords
## I used the name "newwords" to better differentiate the dataset 

#c
x <- str_length(newwords)
table(x)
hist(x,main = "Frequency of Length")
# I think histgram is the best plot type for this distribution

#d
## from the above str_length function, i already know the longest word is 11
## letter, now I can use this to find all the words that are 11 character long
longestword <- str_extract(newwords, "[a-z]{11,11}")
longestword[!is.na(longestword)]

#e
newwords[str_detect(newwords,"^p")]
#f
newwords[str_detect(newwords,"r$")]
#g
newwords[str_detect(newwords,"(^p.*r$)")]
## regular expression used to find word begin with p and end with r

## part 2
library(tidyverse)
library(dplyr)
setwd("~/Desktop/")
mydata <- read.csv(file = 'usa_daily_avg_temps.csv')
head(mydata)

#a
xz <- data.frame(mydata)
usaDailyTemps <- as_tibble(xz)
summary(usaDailyTemps)

#b
usaDailyTemps %>%
  filter((avgtemp) > 0) %>%
  group_by(year) %>%
  summarise(count = n(),
            temp=max(avgtemp))

#bar plot for year/avgtemp
temp <- tapply(xz$avgtemp,xz$year,max)
barplot(temp,xlab="Year",ylab="Avg Temp",col = "cyan",las=2)

#c
usaDailyTemps %>%
  filter((avgtemp) > 0) %>%
  group_by(state) %>%
  summarise(count = n(),
            temp=max(avgtemp))
#bar plot for state/avgtemp
temps <- tapply(xz$avgtemp,xz$state,max)
barplot(temps,xlab="State",ylab="Avg Temp",col = "cyan",las=2)

#d
bostonDailyTemps <- filter(usaDailyTemps,city == "Boston")
bostonDailyTemps
#e
bt <- bostonDailyTemps %>%
  filter((avgtemp) > 0) %>%
  group_by(city,month) %>%
  summarise(count = n(),
            temp=mean(avgtemp))

#bar plot for month/avgtemp for boston
barplot(bt$temp,names.arg = c(1:12),xlab="Month",ylab="Boston Avg Temp",col = "cyan",las=2)



