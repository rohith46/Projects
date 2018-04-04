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

train.housing <- housing_1[trainindices,]

test.housing <- housing_1[-trainindices,]


# creating model

model <- lm(price~., data = train.housing)

summary(model)

corrs <- cor(housing_1)

View(corrs)
write.csv(corrs, file = 'correlation_housing.csv')

# calculating VIF package needed : car 

install.packages('car')

library(car)

vif(model)


# second model without bbratio 

model_2 <- lm(price~.-bbratio, data = train.housing)

summary(model_2)


vif(model_2)

# third model without parkbedratio and bbratio 

model_3 <- lm(price ~. -bbratio -parkbedratio, data = train.housing)

summary(model_3)

vif(model_3)
 

# removing areaperbedroom 

model_4 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom, data = train.housing)

summary(model_4)

vif(model_4)

#removing furnishingstatussemi.furnished

model_5 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom -furnishingstatussemi.furnished, data = train.housing)

summary(model_5)

vif(model_5)


# removing bedrooms

model_6 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom -furnishingstatussemi.furnished -bedrooms, data = train.housing)

summary(model_6)

vif(model_6)

#basement
model_7 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom -furnishingstatussemi.furnished -bedrooms -basement, data = train.housing)

summary(model_7)

vif(model_7)

# removing mainroad

model_8 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom -furnishingstatussemi.furnished -bedrooms -basement -mainroad, data = train.housing)

summary(model_8)

vif(model_8)

# removing hotwaterheating

model_9 <- lm(price ~. -bbratio -parkbedratio -areaperbedroom -furnishingstatussemi.furnished -bedrooms -basement -mainroad -hotwaterheating, data = train.housing)

summary(model_9)

vif(model_9)


# predicting prices

Predict_1 <- predict(model_8, test.housing[-1])


test.housing$test_price <- Predict_1

# correlation

r <- cor(test.housing$price,test.housing$test_price)

rsquared <- r^2

rsquared
