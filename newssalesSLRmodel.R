# loadng data 

newsdata <- read.csv('newspapermarketing.csv')

colnames(newsdata)

set.seed(100)
# plotting 
plot(newsdata$Newspaper, newsdata$Sales)

# subsetting 

trainindices <- sample(1:nrow(newsdata), 0.7*nrow(newsdata))

train.newsdata <- newsdata[trainindices,]
test.newsdata <- newsdata[-trainindices,]


r <- cor(newsdata$Newspaper,newsdata$Sales)
r^2
# creating a model

model <- lm(Sales~Newspaper, data = train.newsdata)

summary(model)


Predict1 <- predict(model, test.newsdata[-2])

test.newsdata$test_sales <- Predict1

r <- cor(test.newsdata$test_sales,test.newsdata$Sales)

rsquared <- r^2

plot(test.newsdata$Sales, col = 'blue', type = 'l')
lines(test.newsdata$test_sales, col = 'red', type = 'l')
