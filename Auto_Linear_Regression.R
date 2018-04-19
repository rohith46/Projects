## Geely Auto analyis ####

#Loading the data

cars  <- read.csv('CarPrice_Assignment.csv')

# loading data dictionary 
library(gdata)
datadict <- read.xls('Data Dictionary - carprices.xlsx')

dim(cars)
colnames(cars)
head(cars)

#descriptive analysis
library(DescTools)
library(Hmisc)

describe(cars)
str(cars)
summary(cars)

#data quality
sum(duplicated(cars$car_ID)) 
#no duplicates
# missing values
sum(is.na(cars))

# saving the data as orginal
cars_orig <- cars

# variable wise analysis

#symboling
table(factor(cars$symboling))
describe(cars$symboling)

#carname
summary(factor(cars$CarName))

cars$CarName <- as.character(cars$CarName)

library(stringr)
carmodel <- str_split_fixed(cars$CarName,'[ ]', 2)
colnames = as.data.frame(carmodel)
cars$carname <- colnames$V1
cars$model_name <- colnames$V2

#removing original carname variable
cars$CarName <- NULL

# or we can use tidyr
#library(tidyr)
#cars <- cars_orig %>% separate(CarName, c('carname', 'model_name'), ' ', extra = 'merge')

table(cars$carname)

#we see that most of the names are mismatch
cars$carname <- tolower(cars$carname)
table(cars$carname)

cars$carname[which(cars$carname == 'porcshce')] = 'porsche'

cars$carname[which(cars$carname == 'toyouta')] = 'toyota'

cars$carname[which(cars$carname == 'vokswagen')] = 'volkswagen'

cars$carname[which(cars$carname == 'maxda')] = 'mazda'

cars$carname[which(cars$carname == 'vw')] = 'volkswagen'
summary(factor(cars$carname))
colnames(cars)

#fueltype
table(cars$fueltype)

#aspiration
table(cars$aspiration)

#doornumber
table(cars$doornumber)
cars$doornumber <- as.character(cars$doornumber)
cars$doornumber[which(cars$doornumber == 'four')] = 4
cars$doornumber[which(cars$doornumber == 'two')] = 2
cars$doornumber <- as.numeric(cars$doornumber)

#carbody
Desc(cars$carbody)

#drivewheel
Desc(cars$drivewheel)

#enginelocation
table(cars$enginelocation)

#wheelbase # having outliers
Desc(cars$wheelbase) 

#carlength
Desc(cars$carlength)

#carwidth #having outliers
Desc(cars$carwidth) 

#carheight
Desc(cars$carheight) 

#carcurbweight
Desc(cars$curbweight) 

#enginetype
Desc(cars$enginetype)# changing dohcv to dohc
cars$enginetype <- as.character(cars$enginetype)
cars$enginetype[which(cars$enginetype == 'dohcv')] = 'dohc'
cars$enginetype <- as.factor(cars$enginetype)

#cylnumber
Desc(cars$cylindernumber)
cars$cylindernumber <- as.character(cars$cylindernumber)
cars$cylindernumber[which(cars$cylindernumber == 'eight')] = 8
cars$cylindernumber[which(cars$cylindernumber == 'five')] = 5
cars$cylindernumber[which(cars$cylindernumber == 'four')] = 4
cars$cylindernumber[which(cars$cylindernumber == 'six')] = 6
cars$cylindernumber[which(cars$cylindernumber == 'three')] = 3
cars$cylindernumber[which(cars$cylindernumber == 'twelve')] = 12
cars$cylindernumber[which(cars$cylindernumber == 'two')] = 2

cars$cylindernumber <- as.integer(cars$cylindernumber)

#enginesize ##has outliers
Desc(cars$enginesize)

#fuelsystem 
Desc(cars$fuelsystem) 

#boreratio
Desc(cars$boreratio)

#stroke ## has outliers
Desc(cars$stroke)

### compressionratio
Desc(geely_auto$compressionratio) ### Data Having Outlier

### horsepower
Desc(geely_auto$horsepower) ### Data Having Outlier

### peakrpm
Desc(geely_auto$peakrpm) ### Data Having Outlier

### citympg
Desc(geely_auto$citympg) ### Data Having Outlier

###highwaympg
Desc(geely_auto$highwaympg) ### Data Having Outlier

###price(Dependent variable)
Desc(geely_auto$price) ### Data Having Outlier

### EDA for all variable categorical and numerical variables
### checking pattern for categorical variable and depend variables

str(cars)

cars$carname <- factor(cars$carname)

cars_numeric <- cars[, sapply(cars, function(x) is.numeric(x))]
cars_categorical <- cars[, sapply(cars, function(x) is.factor(x))]
str(cars_numeric)
str(cars_categorical)

#finding correlation with in numeric variables
library(corrplot)

corrplot(cor(cars_numeric), method = 'circle', type = 'full', outline = T,
         addgrid.col = 'darkgray',order = 'hclust', mar = c(2,0,1,0), 
         title = "Numeric - Correlaction")

### Cylindernumber,horsepower,enginesize,price,boreratio,wheelbase,carwidth,carlength,
### curbweight are positive Correlaction and remaining Variables are negitive corelaction

# exploring the relation between categorical and price
colnames(cars_categorical)
colnames(cars_numeric)


#fueltype 
p1 = ggplot(cars_categorical, aes(x=fueltype, y= cars_numeric$price, fill = fueltype)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('fuel type vs price') + theme_gray()

#aspiration
p2 = ggplot(cars_categorical, aes(x=aspiration, y= cars_numeric$price, fill = aspiration)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('aspiration vs price') + theme_gray()

#Carbody
p3 = ggplot(cars_categorical, aes(x=carbody, y= cars_numeric$price, fill = carbody)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('carbody vs price') + theme_gray()

#drivewheel
p4 = ggplot(cars_categorical, aes(x=drivewheel, y= cars_numeric$price, fill = drivewheel)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('drivewheel vs price') + theme_gray()

#enginelocation
p5 = ggplot(cars_categorical, aes(x=enginelocation, y= cars_numeric$price, fill = enginelocation)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('enginelocation vs price') + theme_gray()

#enginetype
p6 = ggplot(cars_categorical, aes(x=enginetype, y= cars_numeric$price, fill = enginetype)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('engine type vs price') + theme_gray()

#fuelsystem
p7 = ggplot(cars_categorical, aes(x=fuelsystem, y= cars_numeric$price, fill = fuelsystem)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('fuelsystem vs price') + theme_gray()

#carname
p8 = ggplot(cars_categorical, aes(x=carname, y= cars_numeric$price, fill = carname)) +
  geom_boxplot(outlier.colour = 'red',outlier.size = 1) + scale_y_continuous(name = 'Price')+
  ggtitle('carname vs price') + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

library(gridExtra)
grid.arrange(p1,p2,p3,p4, top = 'categorical vs pattern 1')
grid.arrange(p5,p6,p7,p8, top = 'categorical vs pattern 2')

### Looks Like Variables are in Different Median, So These Variables are effecting the Price Variables

#Anova testing - analysis of variance

Anova_1 <- aov(price ~ fueltype +aspiration+carbody+drivewheel+enginelocation+enginetype+fuelsystem+
                 carname+model_name,data = cars)

summary(Anova_1)

# removing the model_name as it is less significant

cars1 <- cars  # duplicating
cars1$model_name <- NULL 

# creating dummy variables for categorical variables

colnames(cars_categorical)

sapply(cars_categorical[-9], function(x) levels(x))

#fueltype, aspiration, engine location, doornumber has 2 levels

#fueltype
levels(cars1$fueltype) <- c(1,0)
cars1$fueltype<- as.numeric(levels(cars1$fueltype))[cars1$fueltype]

#aspiration
levels(cars1$aspiration) <- c(1,0)
cars1$aspiration<- as.numeric(levels(cars1$aspiration))[cars1$aspiration]

#enginelocation
levels(cars1$enginelocation) <- c(1,0)
cars1$enginelocation<- as.numeric(levels(cars1$enginelocation))[cars1$enginelocation]


#carbody
summary(factor(cars1$carbody))

dummy_1 <- data.frame(model.matrix(~carbody, data = cars1))

#View(dummy_1)

dummy_1 <- dummy_1[-1]

# drivewheel
summary(factor(cars1$drivewheel))

dummy_2 <- data.frame(model.matrix(~drivewheel, data = cars1))
dummy_2 <- dummy_2[,-1]

#enginetype
summary(factor(cars$enginetype))

dummy_3 <- data.frame(model.matrix(~enginetype, data = cars1))
dummy_3 <- dummy_3[,-1]

#fuelsystem
summary(factor(cars$fuelsystem))

dummy_4 <- data.frame(model.matrix(~fuelsystem, data = cars1))
dummy_4 <- dummy_4[,-1]

#carname
summary(factor(cars1$carname))

dummy_5 <- data.frame(model.matrix(~carname, data = cars1))
dummy_5 <- dummy_5[,-1]

# combining all the data sets

cars_modeldata <- cbind(cars1[,-c(1,6,7,14,17,26)],dummy_1, dummy_2, dummy_3, dummy_4,dummy_5) 

##CORRELATIONS

corrplot(cor(cars_modeldata), method = 'circle', type = 'full',outline = T, addgrid.col = 'darkgray', 
         title = 'Numeric - correlation')

corrplot(cor(cars_numeric), method = 'circle', type = 'full',outline = T, addgrid.col = 'darkgray', 
         title = 'Numeric - correlation')

corr <- cor(cars_modeldata)

write.csv(corr, file = 'correlation.csv', row.names = T)

## splitting the data 

set.seed(100)
trainindices <- sample(1:nrow(cars_modeldata), 0.7*nrow(cars_modeldata))

cars_training <- cars_modeldata[trainindices,]
cars_testing <- cars_modeldata[-trainindices,]

##### multiple linear regression #####

model_1 <- lm(price~., data = cars_training)

summary(model_1)

#  Multiple R-squared:  0.9807,	Adjusted R-squared:  0.9691
## applying stepAIC
library(MASS)

step_model <- stepAIC(model_1, direction = 'both')

summary(step_model)
#Multiple R-squared:  0.9799,	Adjusted R-squared:  0.9728

## creating a model with sig variables in stepAIC
## removing boreratio
model_2 <- lm(price ~ aspiration + enginelocation + carlength + carwidth + 
                curbweight + enginesize + stroke + horsepower + 
                peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_2)
VIF(model_2)

## removing carlength 

model_3 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + horsepower + 
                peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_3)
VIF(model_3)

## horsepower
model_4 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + 
                peakrpm + citympg + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_4)
VIF(model_4)

# citympg

model_5 <- lm(price ~ aspiration + enginelocation + carwidth + 
                curbweight + enginesize + stroke + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_5)
VIF(model_5)

## curbweight

model_6 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                peakrpm + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_6)
VIF(model_6)

# removing carbodyhatchback carbodysedan

model_7 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                peakrpm + carbodyhardtop + carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi + carnamebmw + carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_7)
VIF(model_7)

# fuelsystemspdi
model_8 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                peakrpm + carbodyhardtop + carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + carnamebmw +
                carnamebuick + carnamechevrolet + 
                carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + 
                carnamenissan + carnameplymouth + carnameporsche + carnamerenault + 
                carnamesaab + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_8)
VIF(model_8)

#carnamesaab
model_9 <- lm(price ~ aspiration + enginelocation + carwidth + enginesize + stroke + 
                peakrpm + carbodyhardtop + carbodywagon + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + carnamebmw +
                carnamebuick + carnamechevrolet + carnamedodge + carnamejaguar + carnamemazda +
                carnamemitsubishi + carnamenissan + carnameplymouth + carnameporsche +
                carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_9)
VIF(model_9)


# removing enginesize consistent high VIF
# removing hardtop and wagon insignificant 
model_10 <- lm(price ~ aspiration + enginelocation + carwidth + stroke + 
                peakrpm + drivewheelrwd + enginetypel + enginetypeohc + 
                enginetypeohcf + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + carnamebmw +
                carnamebuick + carnamechevrolet + carnamedodge + carnamejaguar + carnamemazda +
                carnamemitsubishi + carnamenissan + carnameplymouth + carnameporsche +
                carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_10)
VIF(model_10)

#stroke 
#fuelsystem2bbl

model_11 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 peakrpm + drivewheelrwd + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + fuelsystemmpfi + carnamebmw +
                 carnamebuick + carnamechevrolet + carnamedodge + carnamejaguar + carnamemazda +
                 carnamemitsubishi + carnamenissan + carnameplymouth + carnameporsche +
                 carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_11)
VIF(model_11)

#carnameporsche

model_12 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 peakrpm + drivewheelrwd + enginetypel + enginetypeohc + 
                 enginetypeohcf + enginetyperotor + fuelsystemmpfi + carnamebmw +
                 carnamebuick + carnamechevrolet + carnamedodge + carnamejaguar + carnamemazda +
                 carnamemitsubishi + carnamenissan + carnameplymouth +
                 carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_12)
VIF(model_12)

#drivewheelrwd
#enginetyperotor
model_13 <- lm(price ~ aspiration + enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc +
                 enginetypeohcf + fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet +
                 carnamedodge + carnamejaguar + carnamemazda + carnamemitsubishi + carnamenissan +
                 carnameplymouth + carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_13)
VIF(model_13)

#enginetypeohcf -- keeping it
#carnamedodge
model_14 <- lm(price ~ aspiration + enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc +
                 enginetypeohcf + fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet +
                 carnamejaguar + carnamemazda + carnamemitsubishi + carnamenissan +
                 carnameplymouth + carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_14)
VIF(model_14)

#carnameplymouth
model_15 <- lm(price ~ aspiration + enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc +
                 enginetypeohcf + fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet +
                 carnamejaguar + carnamemazda + carnamemitsubishi + carnamenissan +
                 carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_15)
VIF(model_15)

# aspiration
model_16 <- lm(price ~ enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc +
                 enginetypeohcf + fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet +
                 carnamejaguar + carnamemazda + carnamemitsubishi + carnamenissan +
                 carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_16)
VIF(model_16)
#carnamenissan
model_17 <- lm(price ~ enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc +
                 enginetypeohcf + fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet +
                 carnamejaguar + carnamemazda + carnamemitsubishi +
                 carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_17)
VIF(model_17)
#carnamemitsubishi
model_18 <- lm(price ~ enginelocation + carwidth + peakrpm + enginetypel + enginetypeohc + enginetypeohcf +
                 fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet + carnamejaguar +
                 carnamemazda + carnamerenault + carnametoyota + carnamevolkswagen, data = cars_training)

summary(model_18)
VIF(model_18)
#peakrpm
#carnamevolkswagen
model_19 <- lm(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                 fuelsystemmpfi + carnamebmw + carnamebuick + carnamechevrolet + carnamejaguar +
                 carnamemazda + carnamerenault + carnametoyota , data = cars_training)

summary(model_19)
VIF(model_19)

#carnametoyota
model_20 <- lm(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                 fuelsystemmpfi + carnamebmw + carnamebuick + carnamejaguar +
                 carnamemazda + carnamerenault + carnamechevrolet, data = cars_training)

summary(model_20)
VIF(model_20)

#mazda
model_21 <- lm(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                 fuelsystemmpfi + carnamebmw + carnamebuick + carnamejaguar + carnamerenault +
                 carnamechevrolet, data = cars_training)

summary(model_21)
VIF(model_21)

# renault
#chevrolet
model_22 <- lm(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                 fuelsystemmpfi + carnamebmw + carnamebuick + carnamejaguar ,data = cars_training)

summary(model_22)
VIF(model_22)

final_model <- model_22

# predicting values

cars_testing$prediction <- predict(final_model, cars_testing)

R = cor(cars_testing$price,cars_testing$prediction)

R^2

#plotting

plot(cars_testing$price, type = 'l', col = 'blue')
lines(cars_testing$prediction, type = 'l', col = 'red' )

# very small data we can do bootstrap sample

require(caret)

ctrl <- trainControl(method = 'boot', number = 3)
set.seed(10000)

regression_1 = train(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                       fuelsystemmpfi + carnamebmw + carnamebuick + carnamejaguar, data = cars_modeldata,
                     method = 'lm', trControl=ctrl)
summary(regression_1)

regression_2 = train(price ~ enginelocation + carwidth + enginetypel + enginetypeohc + enginetypeohcf +
                       fuelsystemmpfi + carnamebmw + carnamebuick + carnamejaguar, data = cars_modeldata,
                     method = 'lm', trControl=ctrl)
summary(regression_2)

cars_modeldata$prediction <- predict(regression_2, newdata = cars_modeldata)

plot(cars_modeldata$price, type = 'l', col = 'blue')
lines(cars_modeldata$prediction, type = 'l', col = 'red')

APE = abs(cars_modeldata$price-cars_modeldata$prediction)/cars_modeldata$price

mean(APE)

##### Model Results Summary ########

#Verifying the multiple models,
#we Identified the following variables are most significant variables to predict the Car Price in US market.
#enginelocation
#carwidth
#enginetypel
#enginetypeohc
#enginetypeohcf
#fuelsystemmpfi
#carnamebmw
#carnamebuick
#carnamejaguar

### Same we Predit in the Test data and getting approximately 15% Error.... 

##### End of Model Summary ######
