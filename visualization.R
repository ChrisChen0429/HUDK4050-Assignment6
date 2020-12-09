library(tidyverse)

library(VIM)

### distance 
data <- read_csv('./results/student_stat.csv')
data <- as.data.frame(data)
distance_data <- data[,3:8]
rownames(distance_data) <- data$name
distance <- get_dist(distance_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
