
# Loading dataset 

grades<- read.csv('grades.csv')

# Structure of dataset

str(grades)
## Percentage of .zip files in the dataset
library(stringr)


# Counting ".zip" in the submission column 

sum(str_count(grades$submission, ".zip"))

############################################################
#Q1.

# Percentage of .zip files

percentage <- round((sum(str_count(grades$submission, ".zip"))/nrow(grades))*100,2)

#95.45 percentage
############################################################

# converting to submit_time column to date time format

head(grades)
str(grades)

#grades$submit_time <- as.Date(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")

#or 
 
grades$submit_time <- as.POSIXlt(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")

#grades$submit_date <- NULL

# Extract hour, minute, second

grades$hours <- format(grades$submit_time, "%H")
grades$minute <- format(grades$submit_time, "%M")
grades$second <- format(grades$submit_time, "%S")
grades$date <- as.Date(grades$submit_time)
############################################################
# Q2.
# Is penalized

grades$penalized <- ifelse(grades$date > "2017-01-03", 1, 0)

# Sum of total penalized submission
sum(grades$penalized)

#44
############################################################
# some analysis!

#Q3
# On which date did the most students submit the assignment?
library(ggplot2)

ggplot(grades, aes(factor(date))) + geom_histogram(stat = 'count')

#2017-01-03

#or
table(grades$date)

#or
which(data.frame(table(grades$date))$Freq==max(data.frame(table(grades$date))$Freq))
data.frame(table(grades$date))[10,1]

############################################################

#Q4 & Q5
#In which hour of the day did most students submit the solution?

ggplot(grades, aes(hours)) + geom_histogram(stat = 'count')

#or
which(data.frame(table(grades$hours))$Freq==max(data.frame(table(grades$hours))$Freq))
data.frame(table(grades$hours))[22,1]

# Q6.

#If you see the distribution of submissions by the minutes,
#do you find any pattern of distribution?

ggplot(grades,aes(minute)) + geom_histogram(stat = 'count')
