# Emails discriminator spam or ham ####

# 1. Data Understanding: 
# https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.names
# Number of Instances: 4601
# Number of Attributes: 58 (57 continuous, 1 nominal class label)

#2. Data Preparation: 
##Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(caTools)
# loading data 

emails <- read.csv('Spam.csv', stringsAsFactors = F)


# structure
str(emails)

dim(emails)

#summary of the data
summary(emails)

#factorize the spam column 

emails$spam <- as.factor(emails$spam)

#missing values 

sapply(emails, function(x) sum(is.na(x)))  # no missing values

# splitting the data 

set.seed(100)

indices <- sample.split(emails$spam , SplitRatio = 0.7)

train <- emails[indices,]
test <- emails[!(indices),]


# SVM
####################################################################
#  Model Building
#  Non-Linear Model - Kernels

# Cross Validation
# Tunning Non-linear model

# RBF Kernel 

model_rbf <- ksvm(spam ~.,data = train, scale = FALSE, kernel = 'rbfdot') 

# predicting the model results 

predict_rbf <- predict(model_rbf, test)

#confusion matrix 
confusionMatrix(predict_rbf, test$spam)

#Accuracy    : 0.9261
#Sensitivity : 0.9593         
#Specificity : 0.8750

#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

# Making grid of "sigma" and C values. 

grid <- expand.grid(sigma = seq(0.01,0.05, by = 0.01), C = seq(1,5, by= 1))

metric <- 'Accuracy'

traincontrol <- trainControl(method = 'cv', number = 5)
# Performing 5-fold cross validation

fit.svm.radial <- train(spam~., data = train, method = 'svmRadial', metric = metric, 
                        tuneGrid = grid, trControl = traincontrol)

# Printing cross validation result

print(fit.svm.radial)

#best tune at sigma = 0.01 and C = 5 Accuracy = 0.9316962

#plotting model results

plot(fit.svm.radial)

######################################################################
# Checking overfitting - Non-Linear - SVM
######################################################################

# Validating the model results on test data

predict_nonlinear <- predict(fit.svm.radial, test)

#confusion matrix 
confusionMatrix(predict_nonlinear, test$spam)

#Accuracy    : 0.9348
#Sensitivity : 0.9545          
#Specificity : 0.9044

#########################################################################