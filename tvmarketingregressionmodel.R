#load the data

advertising <- read.csv('tvmarketing.csv')

# view the structure of the data
str(advertising)

# plotting

plot(advertising$TV, advertising$Sales)

#setting random seed to keep them consistent

set.seed(100)

# training data indices 70 percent

trainindices <- sample(1:nrow(advertising), 0.7*nrow(advertising))

# subsetting for training
train.advertising <- advertising[trainindices,]

test <- advertising[-trainindices,]

# creating model

model <- lm(Sales~TV, data = train.advertising)

summary(model)

#predicting values

predict_1<- predict(model, test[-2])

# append the predicted values 

test$test_sales <- predict_1

r <- cor(test$Sales , test$test_sales)

rsquared <- r^2
rsquared
# rsquared of test data is 0.59 and rsquared from original data in model is 0.6233 
# this is fairly a good model
