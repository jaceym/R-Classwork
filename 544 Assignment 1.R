# part 1
## a
scores <- c(58,52,93,55,54,99,68,69,98,70)
length(scores)
scores[c(length(scores)/2 , length(scores)/2 + 1)]
### two sepration equation to get the middle number and the number after it
### when the list has total of odd numbers

## b
median(scores)
as.logical(scores < median(scores))

## c
as.numeric(scores)
x <- median(scores)
scores[c(scores < x)]
scores[c(scores > x)]
### giving "x" the value of median to use as condition to filter the score list

## d
scores[c(TRUE,FALSE)]

## e 
grade <- c("A","B","C","D","E","F","G","H","I","J")
paste(grade , scores ,sep ='=')
### a new list "grade" to store the alphabet value

## f
scores <- c(58,52,93,55,54,99,68,69,98,70)
scores.matrix <- matrix(scores,
           nrow = 5, ncol = 2)
scores.matrix

## g
score.matrix2 <- scores.matrix[c(1,5), ]
score.matrix2

## h

scores <- c(58,52,93,55,54,99,68,69,98,70)
scores.matrix <- matrix(scores,
                        nrow = 5, ncol = 2)
scores.matrix
quizCol <- c("Quiz 1" , "Quiz")
studentRow <- paste("Student", seq(1:5))
### quizCol to assign the number of column, this case is fixed to 2
### studentRow to assign the number of rows,this case is 5, the number is changeable
### for larger number of student

dimnames(scores.matrix) <- list(studentRow,quizCol)
scores.matrix

# Part 2
## a
monthname = month.name
Monthly_Average <- c(4.1,6.1,12.8,23.9,35.5,45.0,49.1,48.1,41.6,30.2,20.7,10.1)
Daily_Max_Average <- c(13.6,14.7,20.7,30.4,41.3,50.4,54.1,53.3,47.1,36.4,28.1,18.4)
Daily_Min_Average <- c(-4.1,-2.4,-5.0,17.4,29.8,39.5,44.0,43.0,36.1,24.0,13.3,1.7)
RecordHigh <- c(48,43,54,60,66,72,71,72,69,62,52,47)
RecordLow <- c(-47,-46,-38,-20,-2,8,24,20,9,-5,-20,-46)


weather.info <- data.frame(
  Month = monthname,
  Monthly_Avg = Monthly_Average,
  DailyMax_Avg = Daily_Max_Average,
  DailyMin_Avg = Daily_Min_Average,
  Record_High = RecordHigh,
  Record_Low = RecordLow)

weather.info

## b
summary(weather.info)
### the quesionts asked for the summary from all the columns, 
### so i just put the whole set into the summary function

## c 
weather.info[c("Month","Record_High","Record_Low")]

## d
rownumber <- nrow(weather.info)
weather.info[c(1,rownumber), ]
### a seprate function to get the total number of rows from the table and
### use to represant the last row

## e 
largerDMax <- (Daily_Max_Average > 40)
weather.info[c(largerDMax), ]
### seprate function to get condition for daily max value larger than 40
### to represent daily max average

## f
data.orig <- weather.info
weather.info$Record_Deviation <- 
  weather.info$Record_High - weather.info$Record_Low
weather.info
data.orig