### titanic survival prediction #####

#loading libraries 
library(ggplot2) #visualization
library(ggthemes) #visualization
library(dplyr) #data manipulation
library(mice) #imputation
library(randomForest) # classification algorithm
library(scales) # labels 

# loading data 

train <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)

data <- bind_rows(train, test)

# data quality 

sapply(data, function(x) sum(is.na(x)))

str(data)

data$title <- gsub('(.*, )|(\\..*)','',data$Name)

table(data$Sex,data$title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly

data$title[data$title == 'Mlle'] <- 'Miss'
data$title[data$title == 'Ms'] = 'Miss'
data$title[data$title == 'Mme'] = 'Mrs'
data$title[data$title %in% rare_title] = 'Rare Title'

# Show title counts by sex again
table(data$Sex, data$title)

# surnames 

data$surname  <- sapply(data$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

# levels 
nlevels(factor(data$surname))

# creating family size

data$Fsize  <- data$SibSp + data$Parch + 1

# creating a family variable 

data$Family <- paste(data$surname,data$Fsize, sep = '_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(data[1:891,], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = 'count',position = 'dodge') +
  labs(x= 'Family size') + scale_x_continuous(breaks = c(1:11)) + theme_few()

#discretize the family size 

data$FsizeD[data$Fsize == 1] <- 'singleton'
data$FsizeD[data$Fsize <= 4 & data$Fsize > 1] <- 'small'
data$FsizeD[data$Fsize > 4] <- 'large'

table(data$FsizeD)

# Show family size by survival using a mosaic plot
mosaicplot(table(data$FsizeD, data$Survived), main = 'Family size by Survival', shade = TRUE)

sapply(data, function(x) sum(is.na(x)))

# missing values  

# This variable appears to have a lot of missing values
data$Cabin[1:28]

# The first character is the deck. For example:
strsplit(data$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:

data$Deck <- factor(sapply(data$Cabin, function(x) strsplit(x,NULL)[[1]][1]))

data[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs

embark_fare <- data %>% filter(PassengerId != 62 & PassengerId !=830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare

ggplot(embark_fare, aes(Embarked, Fare, fill = factor(Pclass))) + geom_boxplot() +
  geom_hline(aes(yintercept = 80), color = 'red', linetype = 'dashed', lwd = 2)+ 
  scale_y_continuous(labels = dollar_format())+ theme_few()

# Since their fare was $80 for 1st class, they most likely embarked from 'C'

data$Embarked[c(62,830)] <- 'C'

data[1044,]

# 3rd class passenger departing for S embarkment 

ggplot(data[data$Pclass == '3' & data$Embarked == 'S',], aes(x =Fare)) + 
  geom_density(fill = '#99d6ff', alpha=0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T))
              , color = 'red', linetype = 'dashed')+ scale_x_continuous(labels = dollar_format()) + theme_few()

median(data$Fare[data$Pclass == '3' & data$Embarked == 'S'], na.rm = T)                                               

# 8.05$ 

data$Fare[1044] <-  median(data[data$Pclass == '3' & data$Embarked == 'S',]$Fare, na.rm = T)

# missing ages 

sum(is.na(data$Age))

head(data)

# factorizing the variables

factor_var <- c("PassengerId","Pclass","Sex","Embarked","title","surname","Family","FsizeD")

data[factor_var] <- lapply(data[factor_var], function(x) as.factor(x))

sapply(data, function(x) sum(x == '', na.rm = T))

# setting a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(data[, !names(data) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname',
            'Survived')], method = 'rf')

mice_output <- complete(mice_mod)

# Plot age distributions

par(mfrow = c(1,2))
hist(data$Age, freq = F, main = 'Age : Original Data', col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$Age, freq = F, main = 'Age : Mice Data', col = 'lightgreen', ylim = c(0,0.04))

sum(is.na(data$Age))

data$Age <- mice_output$Age

sum(is.na(data$Age))

## First we'll look at the relationship between age & survival

ggplot(data[1:891,],aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid( .~ Sex) + theme_few()

# # Create the column child, and indicate whether child or adult

data$child[data$Age < 18 ] <- 'Child'
data$child[data$Age >= 18 ] <- 'Adult'

table(data$child, data$Survived)

# adding mother variable 

data$mother <- 'not mother'

data$mother[data$Sex == 'female' & data$Age > 18 & data$Parch >0 & data$title != 'Miss'] <- 'Mother'

table(data$mother, data$Survived)

#factorizing the two new variables

data$child <- factor(data$child)
data$mother <- factor(data$mother)

md.pattern(data)


# finally we have treated who missing values 

# splitting the data 

train <- data[1:891,]
test <- data[892:1309,]

# set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + 
                         FsizeD + child + mother, data = train)

par(mfrow = c(1,1))
plot(rf_model, ylim = c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

# get importance

importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[,'MeanDecreaseGini'],2))

rankimportance <- varImportance %>% mutate( Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankimportance, aes(x= reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = 'identity') + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0 ,vjust = 0.55 ,
                            size = 4, colour = 'red') + coord_flip() + labs(x = 'Variables') + theme_few()

# predition using test set 

prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)

solution <- data.frame( PassengerId = test$PassengerId, Survived = prediction)

# writing the solution to file

write.csv(solution, file = 'Solution.csv', row.names = F)
