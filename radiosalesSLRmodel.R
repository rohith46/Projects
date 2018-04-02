# radio marketing

# laoding data 

radio <- read.csv('radiomarketing.csv')

# no na values 
sum(is.na(radio))

colnames(radio)

set.seed(100)


# plotting 

plot(radio$Radio, radio$Sales)

# subsetting 

trainindices <- sample(1:nrow(radio), 0.7*nrow(radio))
train.radio <- radio[trainindices,]

test <- radio[-trainindices,]

model <- lm(Sales~Radio, data = train.radio)

summary(model)

Predict1 <- predict(model, test[1])

test$test_sales <- Predict1

r <- cor(test$Sales,test$test_sales)

rsquared <- r^2


##Next, append the predicted results with the test data set 

# to compare the actual prices with the predicted ones


plot(test$Sales, col = 'blue', type = 'l')
lines(test$test_sales, col = 'red', type = 'l')
