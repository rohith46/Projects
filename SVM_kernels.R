############################ SVM Letter Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

# 1. Business Understanding: 

#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the 26 capital letters in the English alphabets

#####################################################################################

# 2. Data Understanding: 
# https://archive.ics.uci.edu/ml/datasets/letter+recognition
# Number of Instances: 20,000
# Number of Attributes: 17 

#3. Data Preparation: 


#Loading Neccessary libraries
library(kernlab)
library(readr)
library(caret)

letters <- read_csv('letter-recognition.csv')

#dimensions and structure
dim(letters)
str(letters)

head(letters)
summary(letters)

# missing values

sapply(letters, function(x) sum(is.na(x))) #no missing values

# converting to factor
letters$letter <- as.factor(letters$letter)

# splitting the data 
set.seed(100)

train_indices <- sample(1:nrow(letters), 0.7*nrow(letters))
train <- letters[train_indices,]
test <- letters[-train_indices,]

# creating a model

#using linear kernel

Model_linear <- ksvm(letter~., data = train, scale = FALSE, kernel = 'vanilladot')

predict_svm_linear <- predict(Model_linear, test)

#confusion matrix

confusionMatrix(predict_svm_linear, test$letter)
#Accuracy : 0.8475

# using RBF kernel

model_non_linear <- ksvm(letter~., data = train, scale = FALSE, kernel = 'rbfdot')

predict_svm_nonlinear <- predict(model_non_linear, test)

#confusion matrix
confusionMatrix(predict_svm_nonlinear, test$letter)
#Accuracy : 0.9323

############   Hyperparameter tuning and Cross Validation #####################
# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method = 'cv', number = 5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- 'Accuracy'

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

grid <- expand.grid(sigma = c(0.025,0.05, by=0.01), C = c(0.1,0.5,1,2))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

library(parallel)
install.packages("doParallel")
library(doParallel)
cl = makeCluster(detectCores())
registerDoParallel(cl)

fit.svm <- train(letter~., data= train, method = 'svmRadial',tuneGrid = grid, 
                 metric = metric, trControl = trainControl )

stopCluster(cl)

print(fit.svm)

plot(fit.svm)

predict_fit_svm <- predict(fit.svm, test)

#confusionmatrix 
confusionMatrix(predict_fit_svm,test$letter)

#Accuracy : 0.9487 
