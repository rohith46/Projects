
# EDA case study for analysizing the driving pattern for loan defa

library(data.table)
library(dplyr)
library(tidyr)
library(DT)
library(lubridate)
library(stringr)
library(ggplot2)
library(corrplot)
library(leaflet)
library(Hmisc)
library(scales)
library(gdata)

# laoding data 

loan <- read.csv('loan.csv', header = T, stringsAsFactors = F)
dictionary <- read.xls('Data_Dictionary.xlsx')
str(loan)

head(loan)

#Customers Information: 

# emp_title 
# emp_length
# home_ownership
# annual_inc
# verification_status
# addr_state
# zip_code
# title
# purpose
# desc
# url

## Loan Characteristics Information

# loan amount
# funded amount
# funded amount invested
# interest rate
# loan status
# loan grade
# loan sub-grade
# dti
# loan issue date
# loan term
# installment

## Credit information: Customer Behaviour variables 

#  delinq_2yrs
#  earliest_cr_line
#  inq_last_6mths
#  open_acc
#  pub_rec
#  revol_bal
#  revol_util 
#  total_acc
#  out_prncp 
#  out_prncp_inv
# total_pymnt"             
# total_pymnt_inv
# total_rec_prncp
# total_rec_int 
# total_rec_late_fee 
# recoveries             
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# application_type      

# behavior variables saving in a vector
Behavior_Var <- c('delinq_2yrs','earliest_cr_line','inq_last_6mths','open_acc','pub_rec','revol_bal','revol_util', 
                  'total_acc','out_prncp', 'out_prncp_inv','total_pymnt','total_pymnt_inv','total_rec_prncp','total_rec_int', 
                  'total_rec_late_fee','recoveries','collection_recovery_fee','last_pymnt_d','last_pymnt_amnt','next_pymnt_d',
                  'last_credit_pull_d','application_type')

# removing the behavior variables as we dont consider them to solve the problem in other words we dont get these information
#before approving the loan application

loan <- loan[,!(colnames(loan) %in% Behavior_Var)]

# Missing values

missing_values <- loan %>% summarise_all(funs(sum(is.na(.))/n()))


missing_values <- gather(missing_values,key = 'feature',value = 'percentage_of_missing')

ggplot(missing_values, aes(x=reorder(feature,-percentage_of_missing),
                           y = percentage_of_missing )) + geom_bar(stat = 'identity', fill = 'red') + coord_flip()


# choosing only columns with less than 15 percent 

good_features <- filter(missing_values,percentage_of_missing<0.15)


good_features <- (good_features$feature)


# removing all other columns except good_feautures

loan <- loan[,colnames(loan) %in% good_features]


#summarise the data 
summary(loan)


## analysizing which variables are not useful 
#zeros or same values -- pymnt_plan  initial_list_status  collections_12_mths_ex_med  policy_code acc_now_delinq 
#chargeoff_within_12_mths delinq_amnt pub_rec_bankruptcies   tax_liens 
#unvaluable variables "member_id","id" "url","desc","emp_title","zip_code","addr_state","title"

red_variables <- c ('pymnt_plan','initial_list_status','collections_12_mths_ex_med','policy_code', 'acc_now_delinq',
                     'chargeoff_within_12_mths', 'delinq_amnt', 'pub_rec_bankruptcies','tax_liens', "member_id","id",
                    "url","desc","emp_title","zip_code","addr_state","title")

# removing all unnecessary variables 
loan <- loan[,!(colnames(loan) %in% red_variables)]

# na values verification
loan %>% summarise_all(funs(sum(is.na(.))))

sum(is.na(loan))

##############################################################
#1. finding the default rate 

##############################################################

loan_current <- filter(loan, loan_status %in% c('Current'))

loan_current$loan_status <- factor(loan_current$loan_status)

#separating them out 

loan <- filter(loan, loan_status %in% c('Fully Paid','Charged Off'))

loan$loan_status <- factor(loan$loan_status)

# assigning 1 to charged off and 0 to fully paid

loan$loan_status <- ifelse(loan$loan_status == 'Charged Off',1,0)


# converting character to factors
loan[sapply(loan,is.character)] <- lapply(loan[sapply(loan,is.character)], as.factor)


# categorial univariate function

univariate_categorical <- function(dataset,var, var_name){
  dataset %>% ggplot(aes(x = as.factor(var))) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),
              stat = 'count',vjust = -0.5) + scale_y_continuous(labels = percent) +
    labs(title = var_name, y = 'Percent', x = var_name) +
    theme(axis.text.y = element_blank(),axis.ticks = element_blank(),axis.title.y = element_blank()
          ,axis.text.x = element_text(angle = 60, hjust = 1)
          )
}

# univariate analysis 

mean(loan$loan_status)

# overall default rate is 14.6%

univariate_categorical(loan, loan$loan_status, 'Default Distribution')

#2. type of products offered by lending club

univariate_categorical(loan, loan$purpose, 'Types of products offered by lending Club')


## not considering the products with less than 5 % and others category 

loan <- filter(loan, purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase"))

loan$purpose <- factor(loan$purpose)


# applications based on years 


loan$issue_d <- paste('01-',loan$issue_d,sep ='')
loan$issue_d <- as.Date(loan$issue_d,'%d-%B-%y')

#extracting year
loan$year <- as.factor(format(loan$issue_d, '%Y'))

univariate_categorical(loan,loan$year,"Yearly distribution for top 4 products")


# function to separate product wise 

product_wise <- function(dataset, var, var_name){
  dataset %>% ggplot(aes(x = as.factor(var)))+ 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))),
              stat = 'count',vjust = -0.5) + scale_y_continuous(labels = percent) + 
    labs(title = var_name,y = 'Percent',x = var_name) + 
    theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1)) + facet_wrap(~purpose)
}

## year wise 

product_wise(loan,loan$year, "Yearly product wise distribution of loans ")


## 4 products varied by loan terms 

product_wise(loan,loan$term,"Loan term throughout the 4 products")


## how grades are distributed

product_wise(loan, loan$grade,'Grades distribution in 4 products')


str(loan$int_rate)

# removing the % in int_rate
loan$int_rate <- extract_numeric(loan$int_rate)

#3. Interest rate 

describe(loan$int_rate)

# interest rate across each grade

tapply(loan$int_rate,loan$grade, mean)

# interest rate across type of loan

tapply(loan$int_rate, loan$purpose, mean)


#4 Home ownership distribution

product_wise(loan,loan$home_ownership,"Distribution of home ownership across 4 products")

#5 verification status distribution

product_wise(loan,loan$verification_status, 'Verification status distribution in top 4 products')


# Derived metrics 

#1. creating bins loan_amnt bins 

options('scipen' = 100, 'digits' = 4)

loan$bins_loan_amnt <- ifelse(loan$loan_amnt <= 5000,'small',
                              ifelse(loan$loan_amnt >5000 & loan$loan_amnt <= 15000, 'medium',
                                     ifelse(loan$loan_amnt > 15000 & loan$loan_amnt <= 25000, 'large','verylarge')))
#2. Funded amount invested 

describe(loan$funded_amnt_inv)

loan$bins_funded_amnt_inv <- ifelse(loan$funded_amnt_inv <= 5000,'small',
                              ifelse(loan$funded_amnt_inv >5000 & loan$funded_amnt_inv <= 15000, 'medium',
                                     ifelse(loan$funded_amnt_inv> 15000 & loan$funded_amnt_inv <= 25000, 'large','verylarge')))


#3. bins for interest rate 

loan$bins_int_rate <- ifelse(loan$int_rate <= 10, 'Lowrate',
                             ifelse(loan$int_rate >10 & loan$int_rate <= 15, 'Mediumrate', 'Highrate'))


#4. DTI 

describe(loan$dti)


loan$bins_dti <- ifelse(loan$dti <= 10, 'lowdti',
                        ifelse(loan$dti >10 & loan$dti <= 20, 'mediumdti','highdti'))


#5. Funded amount 

describe(loan$funded_amnt)

loan$bins_funded_amnt <- ifelse(loan$funded_amnt <= 5000,'small',
                                    ifelse(loan$funded_amnt >5000 & loan$funded_amnt <= 15000, 'medium',
                                           ifelse(loan$funded_amnt> 15000 & loan$funded_amnt <= 25000, 'large','verylarge')))

#6 installment

describe(loan$installment)

loan$bins_installment <- ifelse(loan$installment <= 200 ,'Lowinstallment',
                                ifelse(loan$installment >200 & loan$installment <=400,'Medium',
                                       ifelse(loan$installment >400 & loan$installment <=600, 'High', 'Veryhigh')))


#7 Annual income

describe(loan$annual_inc)

loan$bins_annual_income <- ifelse(loan$annual_inc <= 50000 ,'Lowincome',
                                ifelse(loan$installment >50000 & loan$installment <=100000,'Medium',
                                       ifelse(loan$installment >100000 & loan$installment <=150000, 'High', 'Veryhigh')))


# employment length 

loan$emp_length <- extract_numeric(loan$emp_length)

describe(loan$emp_length)


# 50 % of applicants are less than 4 years candidates and 24% are 10+ years of experience
# missing values 
sum(is.na(loan))
sum(is.na(loan$emp_length))
# missing values are from emp length 
sum(is.na(loan))/nrow(loan)*100
# 2.37 percent of employment details are missing 

dim(loan)
#28107    24
loan <- data.frame(na.omit(loan))
dim(loan)
#[1] 27439    24

# dividing the applicants based on employment 
# <= 1 fresher 1-3 junior 4-7 senior 7 and above expert

loan$emp_length <- as.factor(ifelse(loan$emp_length <=1, 'Fresher',
                                    ifelse(loan$emp_length>1 & loan$emp_length <=3,'Junior',
                                           ifelse(loan$emp_length>3 & loan$emp_length<=7, 'Senior','Expert'))))

# distribution on employment

product_wise(loan,loan$emp_length,'Employment distribution: top 4 products')


# laon approved largely for freshers largely in home and major purchases
# loan approved largely for experts in debt_consolidation and creditcard

#derived metrics are characters converting to factors

loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], as.factor)


# corelation of numerical values in loan 

continuous_var <- names(loan)[sapply(loan,class) !='factor']


continuous_data <- loan[,colnames(loan) %in% continous_var]

#removing loan status as it is discrete variable

continuous_data$loan_status <- NULL

#removing date variable 

continuous_data$issue_d <- NULL

corr <- cor(continous_data)

corrplot(corr, method = 'number')


# Loan amount, funded amount and funded amount invested are hightly correlated. 
# Which is expected, because funded amount can be less than or eqaul to the loan amount applied by the applicant. 
# And most of the time, full amount has been granted by the company, Thus these variables are highly correlated. 
# One good insight is that the loan amount and annual income are having 26% positive correlation which implies the 
# loan amount requested by applicant has been decided on the basis of his/her earning i.e annual income


# Till the univariate analysis, We have seen the distribution of the each and every variables of loan dataset.
# 75% of applicants applied laon for 36 months term period
# We saw that loan applicants are increasing year on year, more than 50% of loan applicants received loans in 2011
# 30% of applicants loans comes under grade B 
# Interest rate increases as a grade level increases from A to B
# 48 % of applicants are living in rented home whereas 44% applicants were mortagaged their home.  
# Most importantly, 47% of applicants applied loan for paying their other loans. 

##################################################################################################
# One thing is clear from the univariate analysis is that the
# loan attributes and customer characteristics would be changed based on the purpose of the loan.
##################################################################################################
# How the loan amount, funded amount, dti and interest rate distributed for top-4 loan products
#########################################################################################


continuous_dist <- function(dataset,con_var,var_name){
  con_summary <- tapply(con_var, loan$purpose, summary)
  p1 <- dataset %>% ggplot(aes(x= con_var)) + geom_line(stat = 'density', color = 'red') + facet_wrap(~purpose) +
    ggtitle(var_name)+xlab(var_name)
  return(list(con_summary,p1))
}

#1. loan_amnt # ignoring the rest amount as all the other variables are correlated to each.
continuous_dist(loan,loan$loan_amnt,"Loan Distribution")

#2.int rate
continuous_dist(loan,loan$int_rate, "Interest rate distribution")


#3. annual income distribution

continuous_dist(loan,loan$annual_inc, "Annual Income Distribution")


#4. DTI 
continuous_dist(loan,loan$dti,"DTI distribution")


############################
# Bi variate analysis

#categorical values

segmented_defaults <- function(dataset, cat_var, var_name){
  a <- aggregate(loan_status~cat_var, dataset, mean)
  b <- data.frame(prop.table(table(cat_var))*100)
  b[,2] <- paste(round(b[,2],2),'%',sep = '')
  colnames(a)[1] <- var_name
  colnames(b)[1] <- var_name
  agg_default <- merge(a,b, by= var_name)
  agg_default <- data.frame(agg_default)
  colnames(agg_default) <- c(var_name,'Default','count')
  agg_default[,2] <- round(agg_default[,2], 2)
  agg_default <- arrange(agg_default, desc(Default))
  
  p.plot <- ggplot(agg_default,aes(agg_default[,1], Default, label = count)) +
    geom_bar(stat = 'identity')+ theme(axis.text.x = element_text(angle = 60, hjust = 1) ) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
  return(list((agg_default[1,2]- agg_default[nrow(agg_default),2]),p.plot))
  
}


# separating categorical data sets 

categorical_var_segments <- names(loan)[sapply(loan,class) == 'factor']

categorical_data_segments <- loan[,(colnames(loan) %in% categorical_var_segments)]

categorical_data_segments$loan_status <- loan$loan_status


# so many sub grades lets get rid of them 

categorical_data_segments$sub_grade <- NULL


###################################################################################################
### Let's understanding the incremental gain within the categories of categorical variables ###
###################################################################################################
# Creating a dataframe which contains the incremental values.


Incremental_grain <- function(dataset,var_name){
  
  Increment <- data.frame(sapply(dataset, function(x) segmented_defaults(dataset, as.factor(x),'Plots')[[1]]))
  Increment <- cbind(variables = row.names(Increment),Increment)
  rownames(Increment) <- NULL
  colnames(Increment) <- c('Variables', var_name)
  Increment <- arrange(Increment,desc(Increment[,2]))
  Increment <- Increment[-which(Increment$Variables == 'loan_status'),]
  
  ggplot(Increment,aes(x = reorder(Variables,Increment[,2]),y = Increment[,2]))+
    geom_bar(position = 'dodge', stat = 'identity') + coord_flip() +
    ggtitle(var_name) +xlab('Variables')+ylab('Default rate')
  
}

# filtering credit card

creditcard <- filter(categorial_data_segments,categorial_data_segments$purpose == 'credit_card')

creditcard$purpose <- NULL

# creditcards plots 

creditcard_plots<- list()
for (i in 1:13){
  creditcard_plots[[i]] <- segmented_defaults(creditcard,creditcard[,i],colnames(creditcard)[i])
}


# for terms

creditcard_plots[[1]]
# Applicant having loan for 60 months term period defaults more than 30 months term period

# for grades

creditcard_plots[[2]]
# Grade contains 5% overall applicant among them 24% defaults 

# and so on, best is to see the maximum incremental gain within the category

Incremental_grain(creditcard,'Credit Card Incremental Difference')

# Top-5 Variables: Credit Card
# Grade
# Term
# Bin interest rate
# Year
# Home Ownership



# debt_consolidation 
###################################################################################

debt_consolidation <- filter(categorical_data_segments,purpose =="debt_consolidation")
debt_consolidation$purpose <- NULL

debt_consolidation_plots <- list()
for (i in 1:13){
  debt_consolidation_plots[[i]] <- segmented_defaults(debt_consolidation,debt_consolidation[,i],colnames(debt_consolidation)[i])
}


#term 
debt_consolidation_plots[[1]]
#27% default rate for 60 months term 

#grade

debt_consolidation_plots[[2]]

# Total approved applicant in Grades E-G comprises of 12% of total applicant but these applicant
# are very risky for the business


Incremental_grain(debt_consolidation,'Debt Consolidation Incremental Difference')

# Top-5 Variables: Debt Consolidation
# Grade
# Bin interest rate
# Home Ownership
# Term
# Bin loan amount

# Home Improvement 

home_improvement <- filter(categorial_data_segments,categorial_data_segments$purpose == 'home_improvement')

home_improvement$purpose <- NULL

# plots

home_improvement_plots <- list()
for (i in 1:13){
  home_improvement_plots[[i]] <- segmented_defaults(home_improvement,home_improvement[,i],colnames(home_improvement)[i])
}

# incremental difference
Incremental_grain(home_improvement,'Home Improvement Incremental Difference')
# Top-5 variables: Home Improvement
# Grade
# Bin interest rate
# Home Ownership
# Term
# Bin Annual Income


# major purchase 

major_purchase <- filter(categorial_data_segments,categorial_data_segments$purpose == 'major_purchase')
major_purchase$purpose <- NULL

# plots 

major_purchase_plots <- list()
for (i in 1:13){
  major_purchase_plots[[i]] <- segmented_defaults(major_purchase, major_purchase[,i],colnames(major_purchase)[i])
}


# incremental difference

Incremental_grain(major_purchase,'Major Purchase Incremental Difference')
# Top-5 Major purchase loan variables: 
# Grade
# Bin interest rate
# Term
# Home Ownership
# Year
