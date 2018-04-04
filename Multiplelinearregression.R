# loading data 

advertising <- read.csv('advertising.csv')

#structure of advertising 
str(advertising)


# creating multiple linear regression 
model1 <- lm(Sales~., data = advertising)

#summary
summary(model1)

#modeling just for news paper
news <- advertising[,c(3,4)]

news_model <- lm(Sales~. , news)

summary(news_model)

# finding correlations of all variables 

corrs <- cor(advertising[,-4])

View(round(corrs,2))
# Newspaper has very high p value i.e variable is insignificant 

 
#creating model without newspaper

model2 <- lm(Sales~.-Newspaper, data = advertising)

summary(model2)

