
#loading data 

uberdata <- read.csv('Uber Request Data1.csv', stringsAsFactors = F)

library(stringr)
library(ggplot2)

str(uberdata)

#duplicated values
sum(duplicated(uberdata$Request.id))

#finding NA values 
sapply(uberdata, function(x) sum(is.na(x)))

#Making date format consistent
uberdata$req_dt <- str_replace_all(uberdata$Request.timestamp, "[/]", '-')
uberdata$drop_dt <- str_replace_all(uberdata$Drop.timestamp, "[/]", '-')

#converting to date format
uberdata$req_dt <- as.POSIXlt(uberdata$req_dt, format = '%d-%m-%Y %H:%M')
uberdata$drop_dt <- as.POSIXlt(uberdata$drop_dt, format = '%d-%m-%Y %H:%M')

# creating hour and minute columns

uberdata$req_hour <- format(uberdata$req_dt, "%H")
uberdata$req_min <- format(uberdata$req_dt, "%M")
uberdata$req_day <- format(uberdata$req_dt, "%d")


# Visualizing the data
P <- ggplot(uberdata, aes(factor(req_hour), fill = Status)) + geom_bar(position = 'dodge')
P
P + facet_wrap(~uberdata$req_day, nrow = 5, ncol = 1) + labs( x = 'Hour', y = 'Number of requests', fill = 'status' )


Q <- ggplot(uberdata, aes(factor(req_hour), fill = Pickup.point )) + geom_bar(position = 'dodge')
Q

Q + facet_wrap(~uberdata$req_day, nrow = 5, ncol = 1) + labs( x = 'Hour', y = 'Number of requests', fill = 'Pickup.point')

# request by hour 
ggplot(uberdata, aes(x = factor(req_hour), fill = Pickup.point)) + geom_bar(position = 'dodge') +
     labs(x = 'Hour', y = 'Number of requests', fill = 'Pickup.point')

# change hour to numeric

uberdata$req_hour <- as.numeric(uberdata$req_hour)

# making time slots

uberdata$timeslot <- ifelse(uberdata$req_hour < 5, "Pre-Morning" ,
                            ifelse(uberdata$req_hour < 10, 'Morning rush' ,
                                   ifelse(uberdata$req_hour < 17 , 'day time' , 
                                          ifelse(uberdata$req_hour < 22 , 'Evening rush', 'late night'))))

#count on number of requests 

nrow(subset(uberdata, uberdata$timeslot == 'Pre-Morning'))
nrow(subset(uberdata, uberdata$timeslot == 'Morning rush'))
nrow(subset(uberdata, uberdata$timeslot == 'day time'))
nrow(subset(uberdata, uberdata$timeslot == 'Evening rush'))
nrow(subset(uberdata, uberdata$timeslot == 'late night'))


# plotting according to time slot


ggplot(uberdata, aes(factor(timeslot), fill = factor(Status))) + geom_bar(position = 'dodge') + 
   labs(x = 'Time slots', y = 'No of requests', fill = 'Status')

# cancellation in the morning 

Morning_rush <- subset(uberdata, timeslot == 'Morning rush')

#plotting

ggplot(Morning_rush, aes(factor(Pickup.point), fill = factor(Status))) + geom_bar(position = 'dodge') +
  labs(x = 'Pickup Location', y = 'No of requests' , fill = 'Status')

# no of cancellation by location

nrow(subset(Morning_rush, Pickup.point == 'Airport' & Status == 'Cancelled'))
nrow(subset(Morning_rush, Pickup.point == 'City' & Status == 'Cancelled'))

problem1 <- subset(Morning_rush, Pickup.point %in% 'City')

#Pie chart 
ggplot(problem1, aes(Pickup.point, fill = Status)) + geom_bar() + 
  coord_polar(theta = "y", start=0)+ 
  labs( y = "Number of Requests", x = "", fill = "Status")


# supply and demand 
nrow(subset(Morning_rush, Pickup.point == 'City' & Status == 'Trip Completed' ))

nrow(subset(Morning_rush, Pickup.point == 'City'))

#472/1677*100 
# 28 percent supply for requested ubers from city 

# Problem 2

Evening_rush <- subset(uberdata, timeslot == 'Evening rush')

#severity of the problem 
nrow(subset(Evening_rush, Pickup.point == 'Airport' & Status == 'No Cars Available'))
nrow(subset(Evening_rush, Pickup.point == 'City' & Status == 'No Cars Available'))

#percentage of shortage 
nrow(subset(Evening_rush, Pickup.point == 'Airport' & Status == 'No Cars Available'))/nrow(subset(Evening_rush, Pickup.point == 'Airport')) * 100

#73% of requests are turned down due to no cars available from airport

#plotting
ggplot(Evening_rush, aes(Pickup.point, fill = Status)) + geom_bar(position = 'dodge') +
  labs(x = 'Pickup location', y = "No of requests", fill = 'Status')

#problem2
problem2 <- subset(Evening_rush, Pickup.point %in% 'Airport')

ggplot(problem2, aes(Pickup.point, fill = Status)) + geom_bar() + 
  coord_polar(theta = 'y',start = 0) + labs(x = '', y = 'No of requests', fill = 'Status')

#supply and demand
nrow(subset(Evening_rush, Pickup.point == 'Airport' & Status == 'Trip Completed'))
nrow(subset(Evening_rush, Pickup.point == 'Airport'))

# 373/1800
nrow(subset(Evening_rush, Pickup.point == 'Airport' & Status == 'No Cars Available'))  
nrow(subset(Evening_rush, Pickup.point == 'Airport' & Status == 'Cancelled')) 

