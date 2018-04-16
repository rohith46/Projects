##loading telecom churn dataset

telecom <- read.csv('churn_data_complete.csv', stringsAsFactors = F)

## na value treatment

sum(is.na(telecom))

sapply(telecom, function(x) sum(is.na(x)))

# removing the na values
telecom <- na.omit(telecom)

# using only 4 columns "change_mou", "drop_vce_Mean", "custcare_Mean" & "avgmou" variables 

telecom1 <- telecom[,c("change_mou", "drop_vce_Mean", "custcare_Mean" , "avgmou",'Customer_ID')]

#################
##outlier treatment

for (i in 1:4){
  box <- boxplot.stats(telecom1[,i])
  out <- box$out
  telecom1 <- telecom1[!telecom1[,i] %in% out,]
}

boxplot(telecom1)

## Standardising the data 

telecom1_norm <- data.frame(sapply(telecom1[1:4], function(x) scale(x)))

#######using k means clustering 

clus1 <- kmeans(telecom1_norm, centers = 5, nstart = 50)

str(clus1)

### find right k value 
r_sq <- rnorm(20)
for (number in 1:20){
  clus <- kmeans(telecom1_norm, centers = number, nstart = 50)
  r_sq[number] <- clus$betweenss/clus$totss
}
plot(r_sq)


### We are using 6 clusters according to elbow method


clus6 <- kmeans(telecom1_norm, centers = 6, nstart = 50)

# appending clusters

telecom1_km <- cbind(telecom1, clus6$cluster)

colnames(telecom1_km)[6] <- 'ClusterID'

##cluster evaluation 

library(dplyr)

km_clusters <- group_by(telecom1_km,ClusterID)

tab1 <- summarise(km_clusters, Mean_change_mou = mean(change_mou),
                  Mean_drop_vce_Mean=mean(drop_vce_Mean),
                  Mean_custcare_Mean=mean(custcare_Mean),
                  Mean_avgmou=mean(avgmou))

library(tidyr)
library(ggplot2)

tab1 %>% gather(-ClusterID, key = 'var', value = 'value') %>%
         ggplot(aes(x= factor(ClusterID), y = value, fill = factor(ClusterID))) +
         geom_bar(stat = 'identity' ) + facet_wrap(~ var, scales = 'free')


#write.csv(telecom1_km, file = 'kmclustering.csv')

## hierarchical clustering 

#calculating distance matrix
telecom1_dist <- dist(telecom1_norm)

# constructing the dendogram using single linkage

telecom1_hc <- hclust(telecom1_dist, method = 'single')

plot(telecom1_hc)

#constructing the dendogram using complete linkage

telecom1_hc2 <- hclust(telecom1_dist, method = 'complete')

plot(telecom1_hc2)

# visualising the cut with k (no of clusters) = 7 

rect.hclust(telecom1_hc2, k =7, border = 'red')
rect.hclust(telecom1_hc2, k = 6, border = 'blue')
rect.hclust(telecom1_hc2, k = 5, border = 'green')

# selecting cluster as 6 

ClusterCut <- cutree(telecom1_hc2, k = 6)

#appending the clusters in the data frame

telecom1_hc <- cbind(telecom1, ClusterCut)

colnames(telecom1_hc)[6] <- 'ClusterID'
###Cluster analysis
#using dplyr
hc_clusters <- group_by(telecom1_hc, ClusterID)

tab2 <- summarise(hc_clusters,Mean_change_mou = mean(change_mou),
                  Mean_drop_vce_Mean=mean(drop_vce_Mean),
                  Mean_custcare_Mean=mean(custcare_Mean),
                  Mean_avgmou=mean(avgmou) )

##plotting all using tidyr

tab2 %>% gather(-ClusterID, key = 'var', value = 'value') %>%
         ggplot(aes(x= factor(ClusterID), y = value, fill = factor(ClusterID)))+
        geom_bar(stat = 'identity') + facet_wrap(~ var, scales = 'free')

#write.csv(telecom1_hc, file = 'hcclustering.csv')
