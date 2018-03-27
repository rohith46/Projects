library(ggplot)
library(lubridate)
library(tidyr)
library(dplyr)
library(tm)

#Loading Data 

loan <- read.csv('loan.csv', stringsAsFactors = F)
dictonary <- read.xls('Data_Dictionary.xlsx')


#duplicated values 
table(duplicated(loan))

# na values 
sapply(loan, function(x) sum(is.na(x)))
sum(is.na(loan))

# searching for any pattern

NA_values <- which(colSums(sapply(loan,is.na)) > 0)

sort(colSums(sapply(loan[NA_values],is.na)),decreasing = T)


aggr_plot <- aggr(loan, col = c('navy blue', 'green'), numbers = TRUE, 
                  sortvars = TRUE, labels = names(data), cex.axis = 0.7, gap = 3, 
                  ylab = c('Histogram of missing data % of total','pattern'))

#removing the na values columns around 50 percent is missing 

loan <- loan[colSums(!is.na(loan))>0]

dim(loan)

colSums(is.na(loan))

# plotting univariate analysis
ggplot(loan, aes(x = reorder(loan_status,loan_status, function(x)-length(x)),fill = loan_status)) +
geom_bar() + geom_text(stat = 'count',aes(label=..count..),vjust = -0.5) + ggtitle('loan vs count')+
  xlab('Status')
# purpose
ggplot(loan, aes(x = reorder(purpose,purpose, function(x)-length(x)),fill = purpose)) +
  geom_bar() + geom_text(stat = 'count',aes(label=..count..),vjust = -0.5) + ggtitle('loan vs count')+
  xlab('Purpose') + theme(axis.text.x=element_blank())
       
 
# loan amount  based on grades

ggplot(loan, aes(loan_amnt, fill = grade))+geom_histogram() + ggtitle('Loan based of Grades') +
  xlab('Grades') + facet_grid(grade ~ .)

##.Loan book growth by issued date
loan$issue_dates <- paste('01-', loan$issue_d)
loan$issue_dates <- parse_date_time(loan$issue_dates, orders = 'd-b-y')


amount<- loan %>% group_by(issue_dates,loan_status) %>% summarise(Loan_Amount = sum(loan_amnt))


ggplot(amount, aes(issue_dates, Loan_Amount))+ geom_area(aes(fill = loan_status))


##5. Analyzing amount loaned per state
ggplot(loan, aes(addr_state,loan_amnt)) + geom_bar(stat = 'identity',fill = 'red')


#6. Analyzing late fee amount per state

ggplot(loan,aes(addr_state,total_rec_late_fee))+ geom_bar(stat = 'identity', fill = 'blue')

late_fee_state <- loan %>% group_by(addr_state) %>% summarise(Late_fee = sum(total_rec_late_fee))

ggplot(late_fee_state, aes(addr_state,Late_fee)) + geom_bar(stat = 'identity', fill = 'dark green')

#emp_length

ggplot(loan, aes(reorder(emp_length,emp_length, function (x) - length(x)),fill = emp_length))+ 
  geom_bar() + xlab('length of employment') + theme(axis.text = element_blank())+
  geom_text(stat = 'count',aes(label=..count..),vjust = -0.5)


#grades vs loan status

ggplot(loan, aes(grade, fill = loan_status)) + geom_bar() + facet_wrap(~loan_status)


#removing % from intrate

loan$int_rate <- sub('%','',loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)

ggplot(loan, aes(loan_status,int_rate))+ geom_boxplot(aes(fill = loan_status))


ggplot(loan, aes(int_rate, fill = grade))+ geom_density() + facet_grid(grade ~ .)

public_record <- loan %>% filter(!is.na(loan$pub_rec)) %>% group_by(loan_status) %>% 
                  summarise(pubs= sum(pub_rec))


ggplot(public_record,aes(loan_status,pubs))+ geom_bar(stat = 'identity')
