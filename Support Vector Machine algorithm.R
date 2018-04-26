
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

emails <- read.delim('Spam.txt', sep = ',', stringsAsFactors = F, header = F)

# loading the colnames 

colnames(emails) <- c("word_freq_make", "word_freq_address", "word_freq_all", "word_freq_3d",
                      "word_freq_our", "word_freq_over", "word_freq_remove", "word_freq_internet",
                      "word_freq_order", "word_freq_mail", "word_freq_receive", "word_freq_will",
                      "word_freq_people", "word_freq_report", "word_freq_addresses", "word_freq_free",
                      "word_freq_business", "word_freq_email", "word_freq_you", "word_freq_credit",
                      "word_freq_your", "word_freq_font", "word_freq_000", "word_freq_money",
                      "word_freq_hp", "word_freq_hpl", "word_freq_george", "word_freq_650",
                      "word_freq_lab", "word_freq_labs", "word_freq_telnet", "word_freq_857",
                      "word_freq_data", "word_freq_415", "word_freq_85", "word_freq_technology",
                      "word_freq_1999", "word_freq_parts", "word_freq_pm", "word_freq_direct",
                      "word_freq_cs", "word_freq_meeting", "word_freq_original","word_freq_project",
                      "word_freq_re", "word_freq_edu", "word_freq_table", "word_freq_conference",
                      "char_freq_;", "char_freq_(", "char_freq_[", "char_freq_!", "char_freq_$",
                      "char_freq_hash", "capital_run_length_average", "capital_run_length_longest",
                      "capital_run_length_total", "spam")

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

# model building 

#--------------------------------------------------------------------
# 4.1 Linear model - SVM  at Cost(C) = 1
#####################################################################

# Model with C =1

model_1 <- ksvm(spam ~. , data = train , scale = FALSE, C=1)

# predicting model results
predict_1 <- predict(model_1, test)

#confusion matrix 
confusionMatrix(predict_1, test$spam)

#Sensitivity : 0.9593         
#Specificity : 0.8750
#Accuracy    : 0.9261

# Model with C =10

model_2 <- ksvm(spam ~., data = train, scale = FALSE, C=10)

#predicting model results
predict_2 <- predict(model_2, test)

#confusionmatrix
confusionMatrix(predict_2, test$spam)

#Sensitivity : 0.9581         
#Specificity : 0.8842
#Accuracy    : 0.929

#####################################################################

#####################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 
######################################################################

# We will use the train function from caret package to perform crossvalidation

traincontrol <- trainControl(method = 'cv', number = 5)

# Number - Number of folds 
# Method - cross validation

metric <- 'Accuracy'

set.seed(100)

# making a grid of c values

grid <- expand.grid(C = seq(1,5,by = 1))

# Performing 5-fold cross validation

fit.svm <- train(spam~., data = train, method = 'svmLinear',  metric = metric, tuneGrid = grid, 
                 trControl = traincontrol )

#printing cross validation results

print(fit.svm)


# plotting 'fit.svm' results

plot(fit.svm)

# best tune at C = 2
#Accuracy = 0.9270335

###############################################################################

# Valdiating the model after cross validation on test data

predict_linear_svm <- predict(fit.svm, test)

#confusion matrix 

confusionMatrix(predict_linear_svm, test$spam)

#Accuracy    : 0.9304
#Sensitivity : 0.9545          
#Specificity : 0.8934
