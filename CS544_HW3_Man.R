### CS 544 HW3 Jacey Jixing Man

## part 1
library(UsingR)
primes
x <- diff(primes)
table(diff(primes))
barplot(table(diff(primes)))

## part 2
coins
##a
x <- table(coins)
x
colSums(x) ## this one shows how many coins are in each denomination
addmargins(x) ## this shows the table with how many coins in each denomination

##b

df <- unname(colSums(x))
df ## this gets the number of each coin
colnames(x[])
z <- as.numeric(colnames(x[]))
z ## this get each type coin value

df*z ## mutiple them together get value of each coin type
o = rbind(z,df*z)
o ## this makes the table view of value of each coin type

## c
apply(coins,2,sum) ## this is the total value of all coin add together
margin.table(x) ## this is the number of total coin

## d 
k <- table(coins)
h <- barplot(k,xlab = "year", ylab = "value")
h

## 3
library(UsingR)
x <- south

#a
stem(x)
## the stem plot shows the majority of the numbber are tenths, 10,16 etc.
#b
f <- fivenum(x)
outlier <- c((f[2] - 1.5*(f[4]) - f[2]) , f[4]+1.5*(f[4]-f[2]))
outlier
# c
boxplot(x,horizontal = TRUE, xaxt = "n")
axis(side = 1, at = fivenum(x),labels = TRUE)

## 4
##a
z <- pi2000
z
x <- table(z)
y <- as.data.frame.array(x)
## b
addmargins(x)
prop.table(x)
## c
barplot(prop.table(x))
## d ### 
x <- hist(pi2000)
x$breaks
x2 <- hist(pi2000,breaks = 13)
x2 ## the first bar is different is because there are more 0 and 1 then other numbers


## 5
## a
x <- cbind(c(25,20),c(10,40),c(15,30))
## b & c
rownames(x) <- c("Men","Women")
colnames(x) <- c("NFL","NBA","NHL")
x
## d
dimnames(x) <-list("Gender" = rownames(x), "Sports" = colnames(x))
x
## e
apply(x,1,sum)
margin.table(x,1)
applt(x,2,sum)
margin.table(x,2)
mqrgin.table(x)
## f
addmargins(x)
## g
prop.table(x,margin=2)
## the data shows there are more women (higher percentage) in NBA playing basketball
## then women in other sports

## h

mosaicplot(x,color=c("green","red"),legend.text = TRUE,
           main = "Gender in Sports",)

##part 6
## a
x <- midsize
names(midsize)
x
pairs(midsize)

## b
#1 each of the car plot against the years, it shows that as
# time goes by, each car increase in price (2,3,4 col in the first roll)

#2 taurus's price incresed the most, since the line of its
#year/price graph (1st row, col 4) is the most steep

#3 Taurus's price increas the most around afer 1998, the
# line for the increase (4th row, col 1) shows a big jump of price

#4 price of all 3 cars are colorated with each other, and time
# since all of the graph are almost a line. 

## part 7
## a
y <- as.data.frame(MLBattend)
x = subset(y,wins)

x1 = subset(y,franchise == c("BAL") , select = c(franchise, wins))
x2 = subset(y,franchise == c("BOS") , select = c(franchise, wins))
x3 = subset(y,franchise == c("DET") , select = c(franchise, wins))
x4 = subset(y,franchise == c("LA") , select = c(franchise, wins))
x5 = subset(y,franchise == c("PHI") , select = c(franchise, wins))
n1 <- as.numeric(x1[,2])
n2 <- as.numeric(x2[,2])
n3 <- as.numeric(x3[,2])
n4 <- as.numeric(x4[,2])
n5 <- as.numeric(x5[,2])

## b
x6 <- data.frame(BAL = n1, BOS= n2,DET = n3,LA = n4,PHI = n5)
x6
## c
boxplot(x6)

## d
#1.BAL has the most wide range of scores
#2. BOS has the most stable performance, it's score range is the smallest
#3.BAL is also the city with the highest socres
#4. There are two outliers for BOS, two really low scores out of ordinary match perfomrace
#5. LA also has one outlier performace out of the all other match performaces


## part 8

house <- read.csv('http://kalathur.com/house.csv', stringsAsFactors = FALSE)
senate <- read.csv('http://kalathur.com/senate.csv', stringsAsFactors = FALSE)

h <- data.frame(house)
s <- data.frame(senate)
s
##a
table(h$Party)
table(s$Party)
## b
head(sort(table(h$State),decreasing = TRUE),n =10)
## c
xy <- data.frame(table(h$State))
xy2 <- as.numeric(xy[,2])
xy2
boxplot(xy2,horizontal = TRUE)

## d

aggregate(h[,2],list(h$Party),mean)
aggregate(s[,2],list(s$Party),mean)



