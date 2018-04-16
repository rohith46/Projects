## loading education data 

library(gdata)
education <- read.xls('Main.xlsx', stringsAsFactors = F)

#missing values

sum(is.na(education))

str(education)

# scaling the columns 

education1 <- sapply(education[,-1], function(x) scale(x))

#finding k value 

for (number in 1:20) {
  clus <- kmeans(education1, centers = number ,nstart = 50)
  r_sq[number] <- clus$betweenss/clus$totss
}

plot(r_sq)

# clustering with 6 centers

clus6 <- kmeans(education1, centers = 6, nstart = 50)

str(clus6)

# binding the clusters

education <- cbind(education, clus6$cluster)

colnames(education)[7] <- 'ClusterID'
library(dplyr)

km_clusters <- group_by(education, ClusterID)

tab1 <- summarise(km_clusters, Mean_illiterate = mean(Illiterate),
                  Mean_Graduate...above = mean(Graduate...above),
                  Mean_Total...Population = mean(Total...Population),
                  Mean_Percentage.Illiterate = mean(Percentage.Illiterate),
                  Mean_Percentage.Graduate...above = mean(Percentage.Graduate...above))

library(ggplot2)

library(tidyr)

tab1 %>%
  gather(-ClusterID, key = "var", value = "value") %>% 
  ggplot(aes(x = factor(ClusterID), y = value, fill= factor(ClusterID))) +
  geom_bar(stat = 'identity') + facet_wrap(~ var, scales = "free") 
