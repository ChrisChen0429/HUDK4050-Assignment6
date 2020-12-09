library(tidyverse)
library(factoextra)
library(ggpubr)

## load the data
data <- read_csv('./results/student_stat.csv')
data <- as.data.frame(data)
## determine the number of cluster
# Elbow method
kmeans_data <- scale(data[,3:8]) 
rownames(kmeans_data) <- data$name

Elow <- fviz_nbclust(kmeans_data, kmeans, method = "wss",k.max=8) +    
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

Silhouette <- fviz_nbclust(kmeans_data, kmeans, method = "silhouette",k.max=8) + 
  labs(subtitle = "Silhouette method")

Gap <- fviz_nbclust(kmeans_data, kmeans,  method = "gap_stat",k.max=8)+
  labs(subtitle = "Gap statistic method")

ggarrange(Elow, Silhouette,Gap, ncol = 3, nrow = 1)

#### choose number of cluster as 4
k4 <- kmeans(kmeans_data, centers = 4)
str(k4)

fviz_cluster(k4, data = kmeans_data)



## kmean without yaw
kmeans_data <- scale(data[,5:8]) 
rownames(kmeans_data) <- data$name

Elow <- fviz_nbclust(kmeans_data, kmeans, method = "wss",k.max=8) +    
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")

Silhouette <- fviz_nbclust(kmeans_data, kmeans, method = "silhouette",k.max=8) + 
  labs(subtitle = "Silhouette method")

Gap <- fviz_nbclust(kmeans_data, kmeans,  method = "gap_stat",k.max=8)+
  labs(subtitle = "Gap statistic method")

ggarrange(Elow, Silhouette,Gap, ncol = 3, nrow = 1)

#### choose number of cluster as 4
k3 <- kmeans(kmeans_data, centers = 3)
str(k3)

fviz_cluster(k3, data = kmeans_data)



## k-mean with time 


