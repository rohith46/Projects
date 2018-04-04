#loading data 

housing <- read.csv('Housing.csv')

str(housing)

# dummary variables

summary(factor(housing$mainroad))
str(housing$mainroad)

# changing categorical to 1's and 0's
levels(housing$mainroad) <- c(1,0)

housing$mainroad <- as.numeric(levels(housing$mainroad))[housing$mainroad]

summary(housing$mainroad)
sum(housing$mainroad)

# doing it for guest room

levels(housing$guestroom) <- c(1,0)
housing$guestroom <- as.numeric(levels(housing$guestroom))[housing$guestroom]

# creating function for converting categorical variables to numeric

A <- function(x){
  levels(x)<- c(1,0)
  x <- as.numeric(levels(x))[x]
}

#Repeating the same function to the columns with these categorical variables 
for (i in c(8,9,10,12)){
  housing[,i] <- sapply(housing[,i], A)
}

# 3 level categorical variables
summary(factor(housing$furnishingstatus))

dummy_1 <- data.frame(model.matrix(~furnishingstatus, data = housing ))

summary(dummy_1)

# removing the dummy variable

dummy_1 <- dummy_1[,-1]

# combining dummy variable to mainset
housing_1 <- cbind(housing[,-13], dummy_1)


# derived metrics area per bedroom , bathroom-bedroom ratio , parking spots per bedrooms

housing_1$areaperbedroom <- housing_1$area/housing_1$bedrooms

housing_1$bbratio <- housing_1$bathrooms/housing_1$bedrooms

housing_1$parkbedratio <- housing_1$parking/housing_1$bedrooms

# setting random 
set.seed(100)

# splitting data to train and test data

trainindices <- sample(1:nrow(housing_1), 0.7*nrow(housing_1))

train <- housing_1[trainindices,]

test <- housing_1[-trainindices,]

# creating model 1 with all variables

model_1 <- lm(price~., data = train)
summary(model_1)


# using step AIC method

install.packages('MASS')
library(MASS)

step <- stepAIC(model_1, direction = 'both')

step

#building 2nd model

model_2 <- lm(price~. -furnishingstatussemi.furnished -areaperbedroom -bbratio -bedrooms, data = train)

summary(model_2)

vif(model_2)

# parking bedratio removing 

model_3 <- lm(price~. -furnishingstatussemi.furnished -areaperbedroom -bbratio -bedrooms -parkbedratio, data = train)

summary(model_3)

vif(model_3)

#basement removal

model_4 <- lm(price~. -furnishingstatussemi.furnished -areaperbedroom -bbratio -bedrooms -parkbedratio -basement, data = train)

summary(model_4)

vif(model_4)

#mainroad

model_5 <- lm(price~. -furnishingstatussemi.furnished -areaperbedroom -bbratio -bedrooms -parkbedratio -basement -mainroad, data = train)

summary(model_5)

vif(model_5)

#hotwaterheating

model_6 <- lm(price~. -furnishingstatussemi.furnished -areaperbedroom -bbratio -bedrooms -parkbedratio -basement -mainroad -hotwaterheating, data = train)

summary(model_6)

vif(model_6)

# predicting values 

Predict_1 <- predict(model_6, test[,-1])

test$test_price <- Predict_1

# finding correlation
r<- cor(test$price, test$test_price)

rsquared <- r^2

rsquared
