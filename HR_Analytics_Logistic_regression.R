#################====== HR ANALYTICS =====###################
#############################################################

##loading the libraries
library(dplyr) #setdiff
library(tidyr) #gather
library(ggplot2) #plotting
library(corrplot)
library(gridExtra) # plots
library(scales)
library(MASS) #stepAIC
library(car) #multicollinearity 
library(caret)
library(GGally)
library(cowplot)
library(caTools) #sample.split
library(e1071)
library(ROCR) #KS statistics

##loading the data 

employee_survey <- read.csv('employee_survey_data.csv', stringsAsFactors = F)
general<- read.csv('general_data.csv', stringsAsFactors = F)
manager_survey <- read.csv('manager_survey_data.csv', stringsAsFactors = F)
in_time <- read.csv('in_time.csv',stringsAsFactors = F)
out_time <- read.csv('out_time.csv',stringsAsFactors = F)

str(employee_survey)
str(general)
str(manager_survey)
str(in_time)

#duplicated values

table(duplicated(employee_survey$EmployeeID)) # no duplicates
table(duplicated(general$EmployeeID)) # no duplicates
table(duplicated(manager_survey$EmployeeID)) # no duplicates

# checking for any different employee ids
setdiff(employee_survey$EmployeeID,general$EmployeeID)
setdiff(employee_survey$EmployeeID,manager_survey$EmployeeID)

# merging the 3 data frames
master <- merge(employee_survey, general, by = 'EmployeeID')
master <- merge(master, manager_survey, by = 'EmployeeID')

str(master)
dim(master)

# missing values

sum(is.na(master))
sort(sapply(master, function(x) sum(is.na(x))))

# sort(colSums(is.na(master))) - Alternative way 

# 111/4410  2.5 percent of the data is missing best is to remove from the data set

master_1 <- na.omit(master)

# checking for duplicate value in intime and out time

table(duplicated(in_time))   # no duplicates
table(duplicated(out_time)) # no duplicates

str(in_time)
str(out_time)

# NA values 

#removing missing values 
in_time <- in_time[colSums(!is.na(in_time))>0]
out_time <- out_time[colSums(!is.na(out_time))>0]

#converting the variables to date format
head(in_time[,4])

in_time[,-1] <- sapply(in_time[,-1], function(x) as.POSIXlt(x,format = '%Y-%m-%d %H:%M:%S'))
in_time <- data.frame(in_time)
in_time$X <- seq.int(nrow(in_time))

out_time[,-1] <- sapply(out_time[,-1], function(x) as.POSIXlt(x,format = '%Y-%m-%d %H:%M:%S'))
out_time <- data.frame(out_time)
out_time$X <- seq.int(nrow(out_time))

# creating no of hours spent for each employee on each day

colnames <- names(in_time)[-1]

time_interval <- cbind(in_time[1], out_time[colnames] - in_time[colnames] )

time_interval <- sapply(time_interval, function(x) round(x,1))

# we have no of hours worked for 250 days for all 4410 members
##Deriving new columns from this data namely
##min hr ,
#max hr ,
#mean hr , 
#no of days Present, 
#no of days Absent, 
#Perc Absent, 
#long leave taken by each employee in office

time_interval_data <- time_interval[,-1]
dim(time_interval_data)
EmployeeID <- time_interval[,1]
Employeetime2 <- NULL
Employeetime1 <- data.frame(matrix(vector(mode = 'numeric'),nrow = 1))

for (i in 1:nrow(time_interval_data)){
  time <- time_interval_data[i,]
  Employeetime1$meanHR <- round(mean(time, na.rm = T),1)
  Employeetime1$minHR <- min(time, na.rm = T)
  Employeetime1$maxHR <- max(time, na.rm = T)
  Employeetime1$present <- sum(!is.na(time))
  Employeetime1$absent <- sum(is.na(time))
  Employeetime1$perctabs <- round(sum(is.na(time))/length(time)*100,1)
  is.na.rle <- rle(is.na(time))
  is.na.rle$values <- is.na.rle$values & is.na.rle$lengths >=2
  Employeetime1$longleave <- ifelse(sum(unlist(is.na.rle[2]))>=1, 'Yes', 'No')
  Employeetime2 <- rbind(Employeetime2,Employeetime1)
  
}

Employeetime2$EmployeeID <- EmployeeID
# merging the derived metrics to master data frame

master2 <- merge(master, Employeetime2, by = 'EmployeeID', all = F)

master <- master2
################################################################

### Data Preparation & Exploratory Data Analysis

# calculate average working time for each employee 
# i.e. average difference between out time and in time


colnames(master)
FACTOR_VARIABLES <- c("Attrition","BusinessTravel","Department","Education","EducationField",
                      "Gender","JobLevel","JobRole","MaritalStatus","Over18","StockOptionLevel",
                      "EnvironmentSatisfaction", "JobSatisfaction","WorkLifeBalance","JobInvolvement",        
                      "PerformanceRating","longleave")
cols <- colnames(master)

NUMERIC_VARIABLES <- cols [!(cols %in% FACTOR_VARIABLES)]

str(master)

# convert character variables to factor
master[,FACTOR_VARIABLES] <- as.data.frame(sapply(master[,FACTOR_VARIABLES],as.factor))

#converting all the numeric variables to numeric
master[,NUMERIC_VARIABLES] <- as.data.frame(lapply(master[,NUMERIC_VARIABLES],as.numeric))

sapply(master, class)

sum(is.na(master))

sapply(master, function(x) sum(is.na(x)))

# dropping na as the value is considerably low

final_data <- na.omit(master)
final_data <- final_data[,-1]

## visualising the attrition in numeric variables 
str(final_data)
#creating one graph to reiterate
ggplot(final_data,aes(x=Attrition, y = Age, fill = Attrition) ) + geom_boxplot(width = 0.2) + coord_flip()+
  theme(legend.position = 'none')

#looks good, creating graphs across all the numeric variables in 2 sets

final_data %>% gather(c(Age,DistanceFromHome,EmployeeCount,MonthlyIncome),key = 'var', value = 'value') %>%
  ggplot(aes(x = Attrition, y = value, fill= Attrition)) +
  geom_boxplot(width = 0.2) + coord_flip() + facet_wrap(~ var, scales = "free") + 
  theme(legend.position = 'none') + theme(axis.title.x = element_blank())

#NUMERIC_VARIABLES

final_data %>% gather(c(NumCompaniesWorked,PercentSalaryHike,StandardHours,TotalWorkingYears),key = 'var', 
                      value = 'value') %>%
  ggplot(aes(x = Attrition, y = value, fill= Attrition)) +
  geom_boxplot(width = 0.2) + coord_flip() + facet_wrap(~ var, scales = "free") + 
  theme(legend.position = 'none') + theme(axis.title.x = element_blank())

final_data %>% gather(c(TrainingTimesLastYear,YearsAtCompany,YearsSinceLastPromotion,YearsWithCurrManager),
                      key = 'var',value = 'value') %>%
  ggplot(aes(x = Attrition, y = value, fill= Attrition)) +
  geom_boxplot(width = 0.2) + coord_flip() + facet_wrap(~ var, scales = "free") + 
  theme(legend.position = 'none') + theme(axis.title.x = element_blank())

final_data %>% gather(c(meanHR,minHR,maxHR,present),key = 'var',value = 'value') %>% 
                      ggplot(aes(x = Attrition, y = value, fill= Attrition)) +
                      geom_boxplot(width = 0.2) + coord_flip() + facet_wrap(~ var, scales = "free") + 
                      theme(legend.position = 'none') + theme(axis.title.x = element_blank())

final_data %>% gather(c(absent,perctabs),key = 'var',value = 'value') %>% 
  ggplot(aes(x = Attrition, y = value, fill= Attrition)) +
  geom_boxplot(width = 0.2) + coord_flip() + facet_wrap(~ var, scales = "free") + 
  theme(legend.position = 'none') + theme(axis.title.x = element_blank())

#############################################################################

#### Distribution of Attrition in factor Variables
FACTOR_VARIABLES
bar_theme <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                   axis.title.x = element_blank(),legend.position = 'none')

# lets plot set of variables

plot_grid(ggplot(final_data, aes(x=BusinessTravel,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=Department,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h") 

plot_grid(ggplot(final_data, aes(x=Education,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=JobRole,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h")

plot_grid(ggplot(final_data, aes(x=MaritalStatus,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=StockOptionLevel,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h")

plot_grid(ggplot(final_data, aes(x=EnvironmentSatisfaction,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=JobSatisfaction,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h")

plot_grid(ggplot(final_data, aes(x=WorkLifeBalance,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=JobInvolvement,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h")

plot_grid(ggplot(final_data, aes(x=PerformanceRating,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion"),
          ggplot(final_data, aes(x=Over18,fill=Attrition))
          + geom_bar(position = position_fill())+ labs(y="Proportion")
          + bar_theme, align = "h")

###########################################################################################


################################################################
# Feature standardisation

# Normalising continuous variables
NUMERIC_VARIABLES

NUMERIC_VARIABLES2 <- lapply(final_data, class) == 'numeric'


# splitting numeric variables and categorical variables

attrition_num <- final_data[,NUMERIC_VARIABLES2]

#standardising the numeric variables

attrition_stand <- data.frame(sapply(attrition_num, function(x) scale(x)))

# removing standard hours and employee count

attrition_stand$EmployeeCount <- NULL
attrition_stand$StandardHours <- NULL

# creating categorical variable data frame
FACTOR_VARIABLES

attrition_categ <- final_data[,FACTOR_VARIABLES]

# removing over18 as it has one level
attrition_categ$Over18 <- NULL

#creating dummy variables
dummies <- data.frame(sapply(attrition_categ, function(x) 
                                  data.frame(model.matrix(~x-1,data = attrition_categ))[,-1]))

#checking attrition rate

attritionrate <- sum(dummies$Attrition)/nrow(dummies)
attritionrate

# combining standardised data with dummies 

attrition_final <- cbind(attrition_stand,dummies)

########################################################################
# splitting the data between train and test

set.seed(100)

train_indices <- sample.split(attrition_final$Attrition, SplitRatio = 0.7)

train <- attrition_final[train_indices,]
test <- attrition_final[!train_indices,]


########################################################################
# Logistic Regression: 

model_1 <- glm(Attrition ~., data = train, family = 'binomial')

summary(model_1)

##stepAIC 

model_2 <- stepAIC(model_1, direction = 'both')

summary(model_2)
## checking for multicollinearity 

sort(vif(model_2))

## removing minHR and EducationField.xLife.Sciences

model_3 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
      TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
      meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
      Education.x5 + EducationField.xMarketing + 
      EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
      JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
      JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
      JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
      StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
      EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
      JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
      WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
    data = train)

summary(model_3)
sort(vif(model_3))

#EducationField.xMarketing

model_4 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + 
                  EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)
summary(model_4)

#EducationField.xMedical

model_5 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + EducationField.xOther + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_5)
sort(vif(model_5))


#EducationField.xOther

model_6 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + EducationField.xTechnical.Degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_6)

#EducationField.xTechnical.Degree
model_7 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobLevel.x5 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_7)

#JobLevel.x5
model_8 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", 
                data = train)

summary(model_8)
sort(vif(model_8))

#MaritalStatus.xMarried
model_9 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  Education.x5 + JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_9)
sort(vif(model_9))

#Education.x5

model_10 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                  JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xManager + 
                  JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle + 
                  StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_10)
sort(vif(model_10))

#JobRole.xHuman.Resources
model_11 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                   JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_11)
sort(vif(model_11))

#StockOptionLevel.x1
model_12 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobLevel.x2 + JobRole.xManager + JobRole.xManufacturing.Director + 
                   JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle + 
                   EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_12)
sort(vif(model_12))

#JobRole.xManager
model_13 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobLevel.x2 + JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                   JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_13)
sort(vif(model_13))

#JobLevel.x2
model_14 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                   JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 + JobInvolvement.x3, family = "binomial", data = train)

summary(model_14)
sort(vif(model_14))

#JobInvolvement.x3
model_15 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                   JobRole.xSales.Executive + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_15)
sort(vif(model_15))

#JobRole.xSales.Executive
model_16 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director +
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_16)
sort(vif(model_16))

#JobRole.xResearch.Director
model_17 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                   meanHR  + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                   JobRole.xManufacturing.Director + MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_17)
sort(vif(model_17))

#BusinessTravel.xTravel_Rarely
model_18 <- glm( Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                   YearsSinceLastPromotion + YearsWithCurrManager + meanHR  + 
                   BusinessTravel.xTravel_Frequently + JobRole.xManufacturing.Director + 
                   MaritalStatus.xSingle + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                   EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                   JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                   WorkLifeBalance.x4 , family = "binomial", data = train)

summary(model_18)
sort(vif(model_18))

########################################################################
# Final Model With only significant variables in the model

final_model<- model_18

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred <- predict(final_model, type = 'response', newdata = test)

summary(test_pred)

# let's use the probability of 50 percent

test_pred_attrition <- factor(ifelse(test_pred >=0.50,'Yes','No'))
test_actual_attrition <- factor(ifelse(test$Attrition ==1, 'Yes', 'No'))

test_conf <- confusionMatrix(test_pred_attrition,test_actual_attrition, positive = 'Yes')
test_conf

# sensitivity is too low we need to use a different cut off value 
# Let's find out the optimal probalility cutoff 
# First let's create a function to find the accuracy, sensitivity and specificity
# for a given cutoff

perform_fn <- function(cutoff){
  prediction_attrition <- factor(ifelse(test_pred >= cutoff,'Yes','No'))
  conf <- confusionMatrix(prediction_attrition,test_actual_attrition, positive = 'Yes')
  acc <- conf$overall[1]
  sen <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sen,spec,acc)))
  colnames(out) <- c('Sensitivity','Specificity','Accuracy')
  return(out)
}

# Creating cutoff values from 0.01 to 0.80 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

s = seq(0.01,0.80,length =100)

OUT = matrix(0,100,3)

for (i in 1:100){
  OUT[i,] <- perform_fn(s[i])
}

plot(s,OUT[,1], xlab = 'Cut Off', ylab = 'Value', cex.lab = 1.5, cex.axis = 1.5, ylim = c(0,1),
     type = 'l', lwd = 2, col = 2)
lines(s, OUT[,2], type = 'l', col = 'blue', lwd = 2)
lines(s, OUT[,3], type = 'l', col = 'darkgreen', lwd = 2)
legend(x=.4,y=.6,col = c(2,'blue','darkgreen'), lwd = c(2,2,2),c('Sensitivity','Specificity','Accuracy'))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# cut off we found is 0.185

test_cutoff_attrition <- factor(ifelse(test_pred >=0.185,'Yes','No'))
conf_final <- confusionMatrix(test_cutoff_attrition,test_actual_attrition,positive = 'Yes')

conf_final

acc <- conf_final$overall[1]
sen <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

test_cutoff_attrition <- ifelse(test_cutoff_attrition =='Yes',1,0)
test_actual_attrition <- ifelse(test_actual_attrition == 'Yes',1,0)

##################################################################################################
### Exporting Data for Excel Analysis (KS, Gain, Lift etc.) ######

myanalysis <- matrix(nrow = length(test_pred), ncol = 2)

myanalysis[,1] <- test_pred
myanalysis[,2] <- test_actual_attrition
colnames(myanalysis) <- c('predict_prob','actual_attrition')
write.csv(myanalysis, file = 'myanalysis.csv')

##################################################################################################
### KS -statistic - Test Data ######

#on testing data 

pred_object_test <- prediction(test_cutoff_attrition,test_actual_attrition)

performance_measure_test <- performance(pred_object_test,'tpr','fpr')

ks_table_test <- attr(performance_measure_test,'y.values')[[1]]-
                 (attr(performance_measure_test,'x.values')[[1]])

max(ks_table_test)


# Loading dplyr package 
####################################################################
# Lift & Gain Chart 

# Loading dplyr package 

library(dplyr)

lift <- function(labels, predicted_prob, groups = 10){
  if(is.factor(labels)) labels <- as.integer(as.character(labels))
  if(is.factor(predicted_prob)) labels <- as.integer(as.character(predicted_prob))
  helper <- data.frame(cbind(labels, predicted_prob))
  helper[,'bucket'] <- ntile(-helper[,'predicted_prob'],groups)
  gaintable = helper %>% group_by(bucket) %>% 
    summarise_at(vars(labels),funs(total = n(),totalresp =sum(.,na.rm=T))) %>%
    mutate(Cumresp = cumsum(totalresp),Gain = Cumresp/sum(totalresp)*100, 
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

Attrition_decile <- lift(test_actual_attrition, test_pred, groups = 10)


#Gain Chart
Gain <- c(0,Attrition_decile$Gain)

Deciles <- c(0,Attrition_decile$bucket)

plot(y=Gain, x= Deciles,type = 'l', lwd = 2, xlab= 'Bucket', ylab='Gain', main = 'Gain Chart')

Randomgain <- seq(0,100,10)

lines(y =Randomgain ,x= Deciles,type = 'l', lwd = 2, col = 'red'  )

Perfectgain <- vector(mode = 'numeric', length = 11)

for (i in 2:11){
  Perfectgain[i] <- 100*min(1,129*(i-1)/209)}

lines(y = Perfectgain ,x= Deciles,type = 'l', lwd = 2, col = 'blue'  )

legend('bottomright',col = c('blue','black','red'), lwd =c(2,2,2), 
       c('Perfect Model','Actual Model','Random Model'), cex = 0.7)


# plotting lift chart 

lift <- Gain/Randomgain
Random_lift <- Randomgain/Randomgain

plot(y = lift, x= Deciles, type = 'l', ylim = c(0,4),lwd = 2, xlab = 'Bucket', ylab = 'Lift',
     main = 'Lift Chart'  )
lines(y = Random_lift, x= Deciles, type = 'l', col = 'red', lwd = 2)

legend('topright', col = c('black', 'red'), lwd = c(2,2), c('Actual Model Lift', 'Random Model Lift'), cex = 0.7)
