### Jixing (Jacey) Man CS 544 HW5

##part 1
library(sampling)
library(prob)
attach(mtcars)
options(digits=4)


## a
x <- c(1:20)

hist(x,prob = TRUE)

## b
set.seed(100)
samples <- urnsamples(x, 2)

xbar1 <- (samples$X1 + samples$X2)/2
hist(xbar1, prob = TRUE)
set.seed(100)
## c
samples <- urnsamples(x,5)

xbar2 <- (samples$X1 + samples$X2)/5
hist(xbar2,prob = TRUE)

set.seed(100)
par(mfrow=c(1,3))
hist(x,prob = TRUE)
hist(xbar1, prob = TRUE)
hist(xbar2,prob = TRUE)
set.seed(100)

## d
set.seed(100)
m11 <- mean(x)
sd11 <- sd(x)
m22 <- mean(xbar1)
sd22 <- sd(xbar1)
m33 <- mean(xbar2)
sd33 <- sd(xbar2)
set.seed(100)

comparemsd1 <- data.frame(M11 = m11,SD11 = sd11, M22 = m22,SD22 = sd22,M33 = m33,SD33=sd33)
comparemsd1
## Input result
##   M11  SD11  M22  SD22 M33  SD33
##  10.5 5.916 5.25 2.562 2.1 1.025
## the larger the sample size, the smaller the standard deviation, and the smaller the mean

## part 2
#a 
par(mfrow = c(1,1))
gdata <- read.csv(url("http://kalathur.com/cs544/data/queries.csv"))
head(gdata, n=2)

xgdata <- gdata$queries
set.seed(100)
hist(gdata$queries,prob = TRUE)
mean(xgdata)
sd(xgdata)

# b
set.seed(100)
x <- gdata$queries

x.sample <- sample(x, size = 1000, 
                   replace = TRUE)

set.seed(100)
samples <- 1000
sample.size <- 5
xbar <- numeric(samples)
for (i in 1:samples) {
  xbar[i] <- mean(sample(x, size = sample.size, 
                         replace = TRUE))
}
mean(xbar)
sd(xbar)

hist(xbar, prob = TRUE,main = "Sample size 5")
set.seed(100)
## part c
set.seed(100)
samples <- 1000
sample.size <- 20
xbar2 <- numeric(samples)
for (i in 1:samples) {
  xbar2[i] <- mean(sample(x, size = sample.size, 
                         replace = TRUE))
}
mean(xbar2)
sd(xbar2)
hist(xbar2, prob = TRUE,main = "Sample size 20")
set.seed(100)
## part d

m1 <- mean(xgdata) 
sd1 <- sd(xgdata) 
m2 <- mean(xbar) 
sd2 <- sd(xbar) 
m3 <- mean(xbar2)
sd3 <- sd(xbar2)

comparemsd <- data.frame(M1 = m1,SD1 = sd1, M2 = m2,SD2 = sd2,M3 = m3,SD3=sd3)
comparemsd
## the mean of all 3 distrubution is pretty similar, but the standard deviation is pretty different, this is due to 
## the difference in sample size, the larger the sample size, the smaller the standard deviation, eg; sample 5 compare to sample 20

## part 3
## a
set.seed(100)
samples <- 1000

r <- 5; p <- 1/2
x.sample <- rnbinom(samples, size = r, prob = p)

par(mfrow=c(1,1))
barplot(prop.table(table(x.sample)),
        xlab = "x", ylab = "Proportion")

## b & c
par(mfrow=c(2,2))

for (size in c(10, 20, 30, 40)) {
  for (i in 1:5000) {
    xbar[i] <- mean((rnbinom(5000, size = size, prob = 1/2)))
  }
  
 
  hist(xbar, prob = TRUE ,main = paste("Sample Size =", size ))
## c 
  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

par(mfrow = c(1,1))

# Sample Size =  10  Mean =  10  SD =  0.06342 
# Sample Size =  20  Mean =  20  SD =  0.08719 
# Sample Size =  30  Mean =  30  SD =  0.1083 
# Sample Size =  40  Mean =  40  SD =  0.1291 
# in the case of negative binomial, the larger the sample size, the larger the mean and stdev

set.seed(100)

## part 4
library(sampling)
data(MU284)
mdata <- data.frame(MU284)


# a
set.seed(123)
s <- srswor(20, nrow(MU284))
s
## simple radom sapling without replacement, 20 sample size
sample.2 <- MU284[s != 0, ]
set.seed(123)
table(sample.2$REG)

head(sample.2[c(10,1:9,11)])
## frequencies for each region
table(MU284$REG) 
enData <- as.vector(table(MU284$REG)) ## REG data from entire table
table(sample.2$REG)/enData
set.seed(123)
## percentages of region to the entire dataset
set.seed(123)
# b
set.seed(113)
N <- nrow(MU284)
n <- 20
k <- ceiling(N/n)
k
r <- sample(k,1)
r
s <- seq(r, by = k, length = n)
s ## this generate value 290 which is larger than 284
## so it would return a NA value
s2 <- replace(s,c(20),284) ## replace the last value
s2
sample.3 <- MU284[s2,]
sample.3
head(sample.3[c(10,1:9,11)])
table(sample.3$REG)
table(sample.3$REG)/enData
set.seed(113)

# c
set.seed(113)
svar <- inclusionprobabilities(MU284$S82,20)
length(svar)
sum(svar)
s <- UPsystematic(svar)
sample.4 <- MU284[s != 0, ]
sample.4
head(sample.4[c(10,1:9,11)])
table(sample.4$S82)
table(sample.4$REG)/enData
set.seed(113)


# d
set.seed(113)
order.index <- order(MU284$REG)
data <- MU284[order.index, ]
head(data[c(10,1:9,11)])

st <- strata(data,stratanames = c("REG"),
             size = c(10,1:9,11),
             method = "srswor")
sample.5 <- getdata(data,st)
table(sample.5$REG)
table(sample.5$REG)/enData
set.seed(113)
# e
s1 <- mean(sample.2$RMT85) ## mean for simple random sampling
s2 <- mean(sample.3$RMT85) ## mean for systematic sampling
s3 <- mean(sample.4$RMT85) ## using inclusion probabilities
s4 <- mean(sample.5$RMT85) ## stratified sample
s5 <- mean(MU284$RMT85) ## mean for entire data set
## The mean of the entire data set is some what in between the
## the value of other 4 samples
compare <- data.frame(sample2 = s1,sample3 = s2, sampleED = s5,sample4 = s3,sample5 = s4)
compare




