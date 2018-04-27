########## digit recogniser ################

#loading required libraries 

library(kernlab) # svm and kernel model building
library(caret) # confusion matrix and preprocessing 
library(parallel) # parallel process
library(doParallel) # parallel process
library(readr) # loading the date
library(ggplot2) # plotting
library(DescTools)#### Check the distrubution of data

data_train <- read_csv('mnist_train.csv', col_names = FALSE, na = 'NA')

data_test <- read_csv('mnist_test.csv', col_names = FALSE, na = 'NA')

#dimensions and structure

dim(data_train)
str(data_train)

#missing values
sapply(data_train, function(x) sum(is.na(x)))
sum(is.na(data_train)) #no missing values

sapply(data_test, function(x) sum(is.na(x)))
sum(is.na(data_test)) #no missing values

colnames(data_train)[1] <- 'digit'
colnames(data_test)[1] <- 'digit'

summary(factor(data_train$digit))
summary(factor(data_test$digit))

# distribution check

ggplot(data_train, aes(x= factor(digit), fill = factor(digit)))+ geom_bar()+ coord_flip()
ggplot(data_test, aes(x= factor(digit), fill = factor(digit)))+ geom_bar()+ coord_flip()

# merging the two data sets for data quality and dimensionality reduction 
#processing 785 variables is not 

data <- rbind(data_train, data_test)

dim(data) # 70000 rows and 785 columns

summary(data[,1:100])

# on an average the range is from 0 to 255

colnames(data)

# missing values 
sum(is.na(data))

# creating a data set with out target variable 

data_1 <- data[,-1]

# distribution of target variable 

Desc(factor(data$digit))

### in 785 Variable to do the Analysis is heavy 
### So reduce the dimension will go for PCA Analysis (or) Factor Analysis

##############################################################
### Factor Analysis using "factanal()" 
### Looks like Factor Analysis not working this data set due to more coloums..
### 1. trying to check the Correlaction to decide number of Factors it's generates "NA"
### 2. And try to Run the factor Analysis, it's data having some "NA" values
# Here Factor Analysis works based on the Correlation and Correlation generates "NA"
### 3. Before Factor Analysis, try to scale the data we will get "NAN" in data.
### 4. After removing the zero variance feature, it's Getting some system memory error, 
###  5.we try to do the analysis based on the error, that say's Factor Analysis not works properly on more the 100 Variable
##############################################################

#### Principal Component Analysis (PCA) using Caret Package
#### Before going to PCA need to remove the Zero Variance features
### check the Variance of features & "near zero variance"
### For this step we eliminate the feature which are in "Zerovariance"

var_obj = nearZeroVar(data_1,saveMetrics = TRUE, allowParallel = T)

var_obj$freqRatio
var_obj$percentUnique
var_obj$zeroVar

table(var_obj$zeroVar)
# 65 variables has zero variance and can be removed

data_2 <- data_1[,var_obj$zeroVar == FALSE]

# implementing PCA analysis 

pca_obj_1 <- preProcess(data_2, method = c('pca'), thresh = 0.95)

pca_obj_1 # PCA needed 332 components to capture 95 percent of the variance

pca_obj_2 <- preProcess(data_2, method = c('pca'), thresh = 0.85)

pca_obj_2 #PCA needed 186 components to capture 85 percent of the variance

pca_obj_3 <- preProcess(data_2, method = c('pca'), thresh = 0.75)
pca_obj_3 # PCA needed 121 components to capture 75 percent of the variance

pca_obj_4 <- preProcess(data_2, method = c('pca'), thresh = 0.65)
pca_obj_4 #PCA needed 80 components to capture 65 percent of the variance

pca_obj_5 <- preProcess(data_2, method = c('pca'), thresh = 0.60)
pca_obj_5 #PCA needed 64 components to capture 60 percent of the variance

#### PCA With Cumalative Variance as 0.55%
pca_obj_6 = preProcess(mnist_Merging_2,method = c("pca"),thresh = 0.55)
pca_obj_6 ###PCA needed 51 components to capture 55 percent of the variance

### PCA -- 0.95 Variance ---332 components  58% (332)  Variables reduced if we capture 95% of Variables
### PCA -- 0.85 Variance ---186 components  76% (186)  Variables reduced if we capture 85% of Variables
### PCA -- 0.75 Variance ---121 components  85% (121)  Variables reduced if we capture 75% of Variables
### PCA -- 0.65 Variance ---80 components   90% (80)   Variables reduced if we capture  65% of Variables
### PCA -- 0.60 Variance ---64 components   92% (64)   Variables reduced if we capture the 60%  of Variables
### PCA -- 0.55 Variance ---51 components   93% (55)   Variables reduced if we capture the 55% of Variables


#### looks like 65% and 60% Variance has not much difference 
#### Captured the minimam 60% Variance for predicting PCA, it will get 64 PCA

data_pca_60 <- predict(pca_obj_5, data_2)
str(data_pca_60)
summary(data_pca_60)

#adding target variable 
data_pca_60$digit <- data$digit

# as factor 
data_pca_60$digit <- as.factor(data_pca_60$digit)
str(data_pca_60)

#splitting the data as per the original rows
data_train_1 <- data_pca_60[1:60000,]
data_test_1 <- data_pca_60[-(1:60000),]

### Model building using Support vector machines
#### Devide the data into training & validation
set.seed(100)
trainindices <- sample(1:nrow(data_train_1),0.7*nrow(data_train_1))

train <- data_train_1[trainindices,]
validation <- data_train_1[-trainindices,]

#### build the basic SVM model
#### if we are not given C value it's takes default 1 ##########################

model_1_ksvm <- ksvm(digit~., data = train )

model_1_ksvm

predict_1_ksvm <- predict(model_1_ksvm, data_test_1)

#confusion matrix
confusionMatrix(predict_1_ksvm, data_test_1$digit)

# Accuracy : 0.964

# non-linear kernel method rbfdot

model_2_rbf <- ksvm(digit~., data = train, kernel = 'rbfdot')

predict_2_rbf <- predict(model_2_rbf, data_test_1)

#confusion matrix
confusionMatrix(predict_1_ksvm, data_test_1$digit)

#Accuracy : 0.964 # not much of a difference in converting to non-linearity 

#register cluster for parallel processing to reduce the model running time
### Parallelisam will work, it's captured all the momery and build the models Parallely

cl = makeCluster(detectCores())
registerDoParallel(cl)

############   Hyperparameter tuning and Cross Validation #####################
# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method = 'cv', number = 5)

metric <- 'Accuracy'

grid <- expand.grid(sigma = c(0.01,0.025,0.05), C = c(1,2,5))

fit.svm <- train(digit~., data = train, method = 'svmRadial',tuneGrid = grid, metric = metric, 
                 trControl = trainControl )

stopCluster(cl)

print(fit.svm)

plot(fit.svm)

predict_3_fitsvm <- predict(fit.svm, data_test_1)

#confusion matrix 
confusionMatrix(predict_3_fitsvm, data_test_1$digit)

#Accuracy : 0.9592
