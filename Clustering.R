##Clustering Case study 

##loading data 

online_retail <- read.csv('Online Retail.csv', stringsAsFactors = F)

str(online_retail)

# NA value treatment

sum(is.na(online_retail))

sapply(online_retail, function(x) sum(is.na(x)))

orderwise <- na.omit(online_retail)

colnames(orderwise)


# Making RFM Recency- Frequency - Monetary

amount = orderwise$Quantity * orderwise$UnitPrice

orderwise <- cbind(orderwise,amount)

#View(orderwise)

orderwise <- orderwise[order(orderwise$CustomerID),]

monetory <- aggregate(amount ~ CustomerID, orderwise, sum )

frequency <- orderwise[,c(7,1)]

k <- table(factor(frequency$CustomerID))

k <- data.frame(k)

colnames(k)[1] <- c("CustomerID")


#merge two 

master <- merge(monetory,k, by = 'CustomerID')


# recency 
str(orderwise)

recency <- orderwise[,c(7,5)]

recency$InvoiceDate <- as.Date(recency$InvoiceDate,format = '%d-%m-%Y %H:%M')

str(recency)

maximum <- max(recency$InvoiceDate)

maximum <- maximum+1

recency$diff <- maximum - recency$InvoiceDate

df <- aggregate(recency$diff, by = list(recency$CustomerID), FUN = min)

colnames(df)[1:2] <- c('CustomerID', 'recency')

str(df)

#merging 3 derived metrics 

RFM <- merge(monetory,k, by = 'CustomerID')

RFM <- merge(RFM, df, by = 'CustomerID')

RFM$recency <- as.numeric(RFM$recency)

# outlier treatment
box <- boxplot.stats(RFM$amount)

out <- box$out

RFM1 <- RFM[!RFM$amount %in% out,]

RFM <- RFM1
########################

box <- boxplot.stats(RFM$Freq)

out <- box$out

RFM1 <- RFM[!RFM$Freq %in% out,]

RFM<- RFM1

###############################

box <- boxplot.stats(RFM$recency)

out <- box$out

RFM1 <- RFM[!RFM$recency %in% out,]

RFM<- RFM1

###standardisation 

RFM_norm <- RFM[,-1]

RFM_norm <- sapply(RFM_norm, function(x) scale(x))

str(RFM_norm)
#RFM_norm <- data.frame(RFM_norm)


# applying k-mean algorithm to create three clusters

clus3 <- kmeans(RFM_norm, centers = 3, iter.max = 50, nstart = 50)

str(clus3)


## finding k value 

r_sq <- rnorm(20)

for (number in 1:20) {clus <- kmeans(RFM_norm, centers = number, nstart = 50)
r_sq[number] <- clus$betweenss/clus$totss
}

plot(r_sq)

# kmeans with elbow points 4,5,6 

clus4 <- kmeans(RFM_norm, centers = 4, nstart = 50)
clus5 <- kmeans(RFM_norm, centers = 5, nstart = 50)
clus6 <- kmeans(RFM_norm, centers = 6, nstart = 50)

## lets assume business have budget to target 5 categories 
##we need to create  clusters

clus5 <- kmeans(RFM_norm, centers = 5, nstart = 50)

str(clus5)

#append to our main data frame

RFM_km <- cbind(RFM, clus5$cluster)

colnames(RFM_km)[5] <- 'ClusterID'


# cluster evaluation

library(dplyr)

km_clusters <- group_by(RFM_km, ClusterID)

tab1 <- summarise(km_clusters, Mean_amount = mean(amount), 
                  Mean_recency = mean(recency), Mean_freq = mean(Freq))

#plotting 
library(ggplot2)

ggplot(tab1, aes(x = factor(ClusterID), y = Mean_amount, fill = factor(ClusterID))) + 
  geom_bar(stat = 'identity')

library(cowplot)

plot_grid(ggplot(tab1, aes(x = factor(ClusterID), y = Mean_amount, fill = factor(ClusterID))) + 
            geom_bar(stat = 'identity'),
          ggplot(tab1, aes(x = factor(ClusterID), y = Mean_recency, fill = factor(ClusterID))) + 
            geom_bar(stat = 'identity'),
          ggplot(tab1, aes(x = factor(ClusterID), y = Mean_freq, fill = factor(ClusterID))) + 
            geom_bar(stat = 'identity'))

## Hierarchical clustering

## Calcualting the distance matrix

RFM_dist <- dist(RFM_norm)

## Constructing the dendrogram using single linkage

RFM_hclust1 <- hclust(RFM_dist, method = 'single')

plot(RFM_hclust1)
 
## Constructing the dendrogram using complete linkage 

RFM_hclust2 <- hclust(RFM_dist, method = 'complete')

plot(RFM_hclust2)

# Visualising the cut in the dendogram 

# k is no of cluster cuts 
rect.hclust(RFM_hclust2, k = 5, border = 'red')

# h is the cluster cut at the specific height
rect.hclust(RFM_hclust2, h= 5, border = 'blue')

# making the cut in the dendogram 

ClusterCut <- cutree(RFM_hclust2,k = 5)

# appending the clusterid to data frame

RFM_hc <- cbind(RFM, ClusterCut)

colnames(RFM_hc)[5] <- 'ClusterID'

## cluster analysis
library(dplyr)

hc_clusters <- group_by(RFM_hc,ClusterID)

tab2 <- summarise(hc_clusters, Mean_amount = mean(amount), 
                  Mean_recency = mean(recency), Mean_freq = mean(Freq))

library(tidyr)

tab2 %>% gather(-ClusterID,key = 'var', value = 'value') %>%
         ggplot(aes(x = factor(ClusterID), y = value, fill= factor(ClusterID))) +
         geom_bar(stat = 'identity') + facet_wrap(~ var, scales = "free") 