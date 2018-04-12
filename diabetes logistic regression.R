library(caTools)

# loading the data 

diabetes<- read.csv('pima_indian_diabetes.csv', stringsAsFactors = F)

#missing values
sum(is.na(diabetes))

str(diabetes)

#scaling continuos data 

diabetes$No_Times_Pregnant <- scale(diabetes$No_Times_Pregnant)

diabetes[,c(2:7)]<- sapply(diabetes[,c(2:7)], function(x) scale(x))

# split data 

set.seed(100)

indices <- sample.split(diabetes$Diabetes, SplitRatio = 0.7)

train <- diabetes[indices,]
test <- diabetes[!(indices),]

#creating a model

model_1 <- glm(Diabetes ~., family = 'binomial', data = train)

summary(model_1)

library(MASS)
model_2 <- stepAIC(model_1, direction = 'both')

summary(model_2)

library(car)

vif(model_2)


#applying on test data 

test_value <- predict(model_2,type = 'response', test[,-8])

summary(test_value)

test$test_prob <- test_value


