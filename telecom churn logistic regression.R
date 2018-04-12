#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

# Install and Load the required packages
#install.packages("MASS") -AIC 
#install.packages("car") - VIF
#install.packages("e1071") 
#install.packages("caret", dependencies = c("Depends", "Suggests")) - confusion matrix
#install.packages("cowplot") -- plotting grids
#install.packages("GGally") -- correlation between numeric variables
#install.packages('ggplot2') data visualization
#install.packages('caTools') -- sample splitting

library(ggplot2) 
library(cowplot)
library(GGally)
library(MASS)
library(caTools)
library(caret)
library(e1071)

# loading the 3 files
churn <- read.csv('churn_data.csv', stringsAsFactors = F)
customer <- read.csv('customer_data.csv', stringsAsFactors = F)
internet <- read.csv('internet_data.csv', stringsAsFactors = F)

str(churn)
str(customer)
str(internet)

length(unique(tolower(customer$customerID)))
length(unique(tolower(churn$customerID)))
length(unique(tolower(internet$customerID)))

setdiff(customer$customerID,churn$customerID)
setdiff(customer$customerID,internet$customerID)

telecom <- merge(customer, churn, by = 'customerID', all = F)
telecom <- merge(telecom, internet,by = 'customerID', all = F )

test <-merge(churn,customer,  by = 'customerID', all = F)
test <-merge(test,internet, by = 'customerID', all = F)
str(telecom) 

View(telecom) #master file 

#analysis by looking at data 

# tenure, monthly charges, Totalcharges are continous 

#senior citizen can be changed to categorical 

# plotting EDA

#setting theme 

bar_theme <- theme(axis.text.x = element_text(angle = 90,hjust = 1, vjust = 0.5))
 #                  legend.position = 'none')
ggplot(telecom, aes(gender, fill = Churn)) + geom_bar() + bar_theme

plot_grid(ggplot(telecom, aes(gender, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(factor(SeniorCitizen), fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(Partner, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(Dependents, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(PhoneService, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(Contract, fill = Churn)) + geom_bar() + bar_theme, align = 'h')

plot_grid(ggplot(telecom, aes(PaperlessBilling, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(PaymentMethod, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(MultipleLines, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(InternetService, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(OnlineSecurity, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(OnlineBackup, fill = Churn)) + geom_bar() + bar_theme, align = 'h')

plot_grid(ggplot(telecom, aes(DeviceProtection, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(TechSupport, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(StreamingTV, fill = Churn)) + geom_bar() + bar_theme,
          ggplot(telecom, aes(StreamingMovies, fill = Churn)) + geom_bar() + bar_theme, align = 'h')

#histogram and boxplots for continous variables

theme_box <- theme(axis.line = element_blank(),axis.text = element_blank(),axis.title = element_blank(),
                   axis.ticks = element_blank())

plot_grid(ggplot(telecom, aes(tenure))+geom_histogram(binwidth = 10), 
          ggplot(telecom, aes(x ='', y = tenure))+geom_boxplot(width = 0.1)+coord_flip()+theme_box,
          align = 'v', ncol = 1 )

plot_grid(ggplot(telecom, aes(MonthlyCharges))+ geom_histogram(binwidth = 20),
          ggplot(telecom, aes(x="",y=MonthlyCharges))+ geom_boxplot(width=0.1)+coord_flip()+theme_box, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(TotalCharges))+ geom_histogram(),
          ggplot(telecom, aes(x="",y=TotalCharges))+ geom_boxplot(width=0.1)+coord_flip()+theme_box, 
          align = "v",ncol = 1)


# boxplot relative to churn 
theme_box_y <- theme(axis.line.y = element_blank(),axis.text.y = element_blank(),
                     axis.title.y = element_blank(),axis.ticks.y = element_blank(), 
                     legend.position = 'none')

plot_grid(ggplot(telecom, aes(x= Churn, y = tenure, fill = Churn))+geom_boxplot(width = 0.2)+
            coord_flip()+ theme(legend.position = 'none'),
          ggplot(telecom, aes(x= Churn, y = MonthlyCharges, fill = Churn))+geom_boxplot(width = 0.2)+
            coord_flip()+theme_box_y,
          ggplot(telecom, aes(x= Churn, y = TotalCharges, fill = Churn))+geom_boxplot(width = 0.2)+
            coord_flip()+theme_box_y, align = 'v', nrow  = 1)
# Shorter tenure sees more telecom

#correlation

ggpairs(telecom[,c('tenure','MonthlyCharges','TotalCharges')])

#As expected, tenure and TotalCharges are highly correlated (corr 0.83)

# data preparation 

telecom$SeniorCitizen <- ifelse(telecom$SeniorCitizen  == 1, 'Yes','No')


# outlier treatment


sapply(telecom[,c('tenure','MonthlyCharges','TotalCharges')],
       function(x) quantile(x,seq(0,1,.01),na.rm = T))


# so no outliers 

sapply(telecom, function(x) sum(is.na(x)))

View(subset(telecom, is.na(telecom$TotalCharges)))

#ratio of na values 

sum(is.na(telecom$TotalCharges))/nrow(telecom)
# 0.15 percent so dropping them

telecom <- telecom[!is.na(telecom$TotalCharges),]


# normalising continous values 

telecom$tenure <- scale(telecom$tenure)
telecom$MonthlyCharges <- scale(telecom$MonthlyCharges)
telecom$TotalCharges <- scale(telecom$TotalCharges)

View(telecom)


# converting churn yes no to 1 and 0 
telecom$Churn <- ifelse(telecom$Churn == 'Yes', 1, 0)

#churn_rate
churn_rate <- sum(telecom$Churn)/nrow(telecom)

churn_rate
# 26.57 percent of churn rate

# data frame with categorical variables

telecom_categ <- telecom[,-c(1,6,11,12,13)]

str(telecom_categ)

# converting to factors

telecom_categ <- data.frame(sapply(telecom_categ, function(x) factor(x)))

str(telecom_categ)

#creating dummy variables

dummies <- data.frame(sapply(telecom_categ, function(x) 
             data.frame(model.matrix(~x-1,data = telecom_categ))[,-1]))

# all categorical values are turned into binomial values 1's and 0's


# final data set merging

telecom_final <- cbind(telecom[,c(6,11,12,13)],dummies)


# splitting the data now

set.seed(100)

indices <- sample.split(telecom_final$Churn, SplitRatio = 0.7)

train <- telecom_final[indices,]
test <- telecom_final[!(indices),]


#################
##logistic Regression

model_1 <- glm(formula = Churn ~. , data = train, family = 'binomial' )

summary(model_1)

# stepAIC to eliminate the unsignificant variables 

model_2 <- stepAIC(model_1, direction = 'both')

summary(model_2)


# multicollinearity 

library(car)

vif(model_2)


# removing totalcharges instead of monthly charges because of significance

# excluding totalcharges 

model_3 <- glm(formula = Churn ~ tenure + MonthlyCharges + SeniorCitizen + PhoneService +
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + MultipleLines.xYes + InternetService.xFiber.optic +
                 InternetService.xNo + OnlineBackup.xYes + DeviceProtection.xYes + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)

summary(model_3)

vif(model_3)

#dropping monthly charges high vif and previous model too 

model_4 <- glm(formula = Churn ~ tenure + SeniorCitizen + PhoneService +
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + MultipleLines.xYes + InternetService.xFiber.optic +
                 InternetService.xNo + OnlineBackup.xYes + DeviceProtection.xYes + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)

summary(model_4)
vif(model_4)

# dropping OnlineBackup.xYes
model_5 <- glm(formula = Churn ~ tenure + SeniorCitizen + PhoneService +
                 Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                 PaymentMethod.xElectronic.check + MultipleLines.xYes + InternetService.xFiber.optic +
                 InternetService.xNo + DeviceProtection.xYes + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)

summary(model_5)

# dropping DeviceProtection.xYes
model_6 <- glm(formula = Churn ~ tenure + SeniorCitizen + PhoneService + Contract.xOne.year +
                 Contract.xTwo.year + PaperlessBilling + PaymentMethod.xElectronic.check + 
                 MultipleLines.xYes + InternetService.xFiber.optic +InternetService.xNo + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)

summary(model_6)


# dropping senior citizen
model_7 <- glm(formula = Churn ~ tenure + PhoneService + Contract.xOne.year +
                 Contract.xTwo.year + PaperlessBilling + PaymentMethod.xElectronic.check + 
                 MultipleLines.xYes + InternetService.xFiber.optic +InternetService.xNo + 
                 StreamingTV.xYes + StreamingMovies.xYes, family = "binomial", data = train)

summary(model_7)

#dropping StreamingTV.xYes

model_8 <- glm(formula = Churn ~ tenure + PhoneService + Contract.xOne.year + Contract.xTwo.year +
                 PaperlessBilling + PaymentMethod.xElectronic.check + MultipleLines.xYes + 
                 InternetService.xFiber.optic + InternetService.xNo + StreamingMovies.xYes,
                 family = "binomial", data = train)

summary(model_8)

final_model <- model_8
# vif are quite low and significance are also good . We can conclude the model is good

# 10 significant variables 

# model evaluation on test data 

#predicted probabilities of churn in test data



test_pred <- predict(final_model, type = 'response',newdata = test[,-4])

summary(test_pred)

test$prob <- test_pred

View(test)


#probability cutoff by 50 percent

test_pred_churn <- factor(ifelse(test$prob >= 0.5, 'Yes', 'No'))
test_actual_churn <- factor(ifelse(test$Churn == 1, 'Yes', 'No'))

# creating a table 
table(test_actual_churn,test_pred_churn)


###################################
#probability cut off by 40 percent

test_pred_churn <- factor(ifelse(test$prob >= 0.4, 'Yes', 'No'))

test_conf <- confusionMatrix(test_pred_churn, test_actual_churn, positive = 'Yes')

test_conf


# creating a function to find the optimal cut off

perform_fn <- function(cutoff){
  predicted_churn <- factor(ifelse(test$prob >= cutoff, 'Yes', 'No'))
  conf <- confusionMatrix(predicted_churn,test_actual_churn, positive = 'Yes')
  acc <- conf$overall[1]
  sen <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sen,spec,acc)))
  colnames(out) <- c('sensitivity','specificity','accuracy')
  return(out)
}

# predicted values are between 0.003 to 0.8121

summary(test_pred)

s = seq(0.01,0.80, length= 100)

OUT = matrix(0,100,3)

for (i in 1:100){
  OUT[i,] <- perform_fn(s[i])
}


# plotting all together

plot(s,OUT[,1],xlab = 'CutOff', ylab = 'Value', type = 'l', cex.lab = 1.5, cex.axis = 1.5, ylim = c(0,1)
     ,lwd = 2,col = 2, axes = FALSE)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
box()
lines(s,OUT[,2],col = 3,lwd = 2)
lines(s,OUT[,3],col = 4, lwd = 2)
legend(0,0.50,col = c(2,3,4), lwd = c(2,2,2),c('sensitivity','specificity','accuracy'))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# keeping cutoff value as 0.313

test_cutoff_churn <- factor(ifelse(test$prob >= 0.313, 'Yes', 'No'))

conf_final <- confusionMatrix(test_cutoff_churn,test_actual_churn,positive = 'Yes')

conf_final

Accuracy <- conf_final$overall[1]
Sensitivity <- conf_final$byClass[1]
Specificity <- conf_final$byClass[2]

Accuracy
Sensitivity
Specificity

View(test)

###################
#K-S statistics test data 


test_cutoff_churn <- ifelse(test_cutoff_churn == 'Yes', 1, 0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)

#install.packages('ROCR')
library(ROCR)

predict_object_test <- prediction(test_cutoff_churn,test_actual_churn)

performance_measure_test <- performance(predict_object_test, 'tpr','fpr')

ks_table_test <- (attr(performance_measure_test, 'y.values')[[1]]) -
                   (attr(performance_measure_test, 'x.values')[[1]])

max(ks_table_test)


# lifting and gain chart
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)
