###CS554 HW2 Jacey(Jixing) Man

### Part 1 
### calculation without R 
#1. (0.4*0.7)/(0.4*0.7 + 0.5*0.4 + 0.1*0.2) = 0.56
#2. (0.5*0.4)/(0.4*0.7 + 0.5*0.4 + 0.1*0.2)  = 0.4
#3. (0.1*0.2)/(0.4*0.7 + 0.5*0.4 + 0.1*0.2)  = 0.04

## Bayes R code
bayes <- function (prior, likelihood) {
  numerators <- prior * likelihood
  return (numerators / sum(numerators))
}

prior <- c(0.4,0.5,0.1)
like <- c(0.7,0.4,0.2)

bayes(prior,like)

### PART 2
## a
## roll 2 dice with prob pack in R
library(prob)
roll <- rolldie(2, makespace = TRUE)
subset(roll , X1 - X2 > 0)
# the aboslute value for the difference of two rolls
# means whichever roll number minus the other 
# can not be negative, so in this case > 0

## b
roll <- rolldie(2, makespace = TRUE)
smaller <- subset(roll , X1 - X2 <= 2)

roll <- rolldie(2, makespace = TRUE)
larger <- subset(roll , X1 - X2 >= 3)

Prob(smaller)
Prob(larger)

## c 
roll <- rolldie(2, makespace = TRUE)
rollm <- addrv(roll, side = abs(X1 - X2))
## I assume this question is asking for the marginal
## of all possible difference between the two dice roll
## so I used aboslute value as the difference between the two die roll
marginal(rollm, vars = "side")

## d 
rollf <- function(x) {
    return (sum(x %% 2 == 0))
}
roll <- rolldie(2, makespace = TRUE)
roll <- addrv(roll,FUN = rollf, name = "U")
marginal(roll,vars = "U")

## part 3

evensum <- function (x) {
  holder = c()
  for (i in seq(1:length(x))) {
    if (x[i] %% 2 == 0) {
      holder = c(holder, x[i])
    }
  }
  return (sum(holder))
}
### the above code use the for loop and holder to return the sum of holder,
### the holder represent all the even number in a given vector

evensum2 <- function(x) {
  y <- x[which(x %% 2 == 0)] 
  z <- sum(y)
  return(z)
}
### the above function retuns sum of all even number without using loop, the "which"
### function is there to filter out even number in a given vector

## part 4
## a
dow <- read.csv('http://kalathur.com/dow.csv', stringsAsFactors = FALSE)
df <- dow
dow$DIFFS <- (c(dow[,3] <- 0, diff(dow$VALUE, lag = 1)))
dow <- dow[c("DATE","VALUE","DIFFS")]
dow

## b
close_higher <- sum(dow$DIFFS > 0)
close_higher

close_lower <- sum(dow$DIFFS < 0)
close_lower

## c
dow[dow$DIFFS >= 400, ]

## d
dow$STREAK <- ifelse(dow$DIFFS >= 100, "YES","NO")
dow
y <- rle(dow$STREAK)
y

max_run_length <- max(y$lengths[y$values == "YES"])
max_run_length

# I have trouble getting the right index of the "yes" streak start
# I kept getting 10, even the correct number is 36
which.max(y$lengths[dow$STREAK == "YES"])
y$values[which.max(y$lengths[y$values == "YES"])]

# Index of max run in rle output
max_run_index <- which.max(sum(y$lengths[y$values == "YES"]))
max_run_index
prev_values_length <- ifelse(max_run_index == 1, 0, sum(y$lengths[1:(max_run_index-1)]))
prev_values_length
# starting index of max run
prev_values_length + 1
(prev_values_length + 1) : (prev_values_length + max_run_length)
dow[36:38, ]
## the correct output should be produced by the above code