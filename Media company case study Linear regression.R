
# loading data 

media <- read.csv('mediacompany.csv', stringsAsFactors = F)

#loading libraries

library(ggplot2)
library(lubridate)

# converting date to date format

media$date <- as.Date(media$Date, '%m/%d/%Y')

# derived metrics days

media$day <- difftime(media$date, as.Date('2017-02-28'), units = 'days')

media$day <- as.numeric(media$day)

# plotting the days with view shows

ggplot(media, aes(media$day,media$Views_show)) + geom_line(aes(color = 'blue'))+
  scale_x_continuous(name = 'days', breaks = seq(0,84,7) , limits = c(0,84))+
  scale_y_continuous(name = 'Views of the show', breaks = seq(0,800000,100000), limits = c(0,800000))


# plotting the days with view shows and adimpression
ggplot(media, aes(day,Views_show)) + geom_line(aes(color = 'blue'))+
  scale_x_continuous(name = 'days', breaks = seq(0,84,7) , limits = c(0,84))+
  scale_y_continuous(name = 'Views of the show', breaks = seq(0,800000,100000), limits = c(0,800000))+
  geom_line(aes(day,Ad_impression*8/30000, color = 'red'))

# alternative way

library(plotrix)

twoord.plot(lx = media$day,ly = media$Views_show,rx = media$day,ry = media$Ad_impression,
             xlab = 'days', rylab = 'Ad Impressions', ylab = 'View of the show')


# converting to weekdays and weekend
media$week <- wday(media$date)
# media$week_names <- wday(media$date, label = TRUE) -- check week names 

media$weekend <- ifelse((media$week == 7) | (media$week == 1),1,0)


# writing a model with visitors and weekday 

model_1 <- lm( Views_show ~ Visitors + week , data = media)

summary(model_1)

# with weekend

model_1.1 <- lm( Views_show ~ Visitors + weekend , data = media)

summary(model_1.1)
 

# adding character

model_2 <- lm (formula = Views_show ~ Visitors + weekend + Character_A, data = media)

summary(model_2)               
 
#lagviews

media$lag_views <- c(0,media$Views_show[seq_along(media$Views_show) -1])

# adding lag_views

model_3 <- lm (formula = Views_show ~ Visitors + weekend + Character_A + lag_views, data = media)

summary(model_3) 


# adding visting platform and removing the lag views as it is making the model complex to explain
#also visitors becoming less significant

model_4 <- lm (formula = Views_show ~ Visitors + weekend + Character_A + Views_platform, data = media)

summary(model_4) 

# dropping visitors as it is not being significant 

model_4.1 <- lm (formula = Views_show ~ weekend + Character_A + Views_platform, data = media)

summary(model_4.1) 


#visitors 

model_5 <- lm (formula = Views_show ~ weekend + Character_A + Visitors, data = media)

summary(model_5) 


#ad impressions

model_6 <- lm (formula = Views_show ~ weekend + Character_A + Visitors + Ad_impression, data = media)

summary(model_6) 

vif(model_6)


# choosing higher p value variable to be removed than negative coefficient 

model_7 <- lm (formula = Views_show ~ weekend + Character_A + Ad_impression, data = media)

summary(model_7) 

vif(model_7)

# converting ad impressions and inroducing cricket 

media$Ad_impression_million <- media$Ad_impression/1000000

model_8 <- lm (formula = Views_show ~ weekend + Character_A + Ad_impression_million + Cricket_match_india, 
               data = media)

summary(model_8) 

vif(model_8)


# dropping Character_A  and cricket_match

model_9 <- lm (formula = Views_show ~ weekend + Ad_impression_million ,data = media)

summary(model_9) 

vif(model_9)


 