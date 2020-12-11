library(tidyverse)
library(VIM)
library(readxl)
library(ggpubr)
library(factoextra)

### distance 
data <- read_csv('./results/student_stat.csv')

pca_data <- read_excel("data/pca_data.xlsx")
colnames(pca_data) <- c("name",paste('c',as.character(1:9),sep = ""),'gender','hight')
pca_data$name <-gsub(" ", "", pca_data$name, fixed = TRUE)

a <- merge(as.data.frame(pca_data), as.data.frame(data),by.x = 'name',by.y = 'name')

ggplot(a, aes(x=time,color=gender)) + 
  geom_histogram(fill="white", alpha=0.5, position="identity",binwidth=30)


data <- as.data.frame(data)
distance_data <- data[,3:8]
rownames(distance_data) <- data$name
distance <- get_dist(distance_data)

pdf("results/pca_distance.pdf") 
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off() 



data <- read_csv('./results/student_quantile.csv')
yaw_mean <- data %>% select(ends_with('mean') & starts_with('yaw'))
yaw_mean$name <- data$name
long <- yaw_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile1_mean", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile2_mean", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile3_mean", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile4_mean", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile5_mean", "80%-100%", as.character(quantile))) 

yaw <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "Mean Yaw")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


roll_mean <- data %>% select(ends_with('mean') & starts_with('roll'))
roll_mean$name <- data$name
long <- roll_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "roll_quantile1_mean", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile2_mean", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile3_mean", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile4_mean", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile5_mean", "80%-100%", as.character(quantile))) 

roll <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "Mean Roll")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


pitch_mean <- data %>% select(ends_with('mean') & starts_with('pitch'))
pitch_mean$name <- data$name
long <- pitch_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile1_mean", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile2_mean", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile3_mean", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile4_mean", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile5_mean", "80%-100%", as.character(quantile))) 

pitch <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "Mean Pitch")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


pdf("results/quant-mean.pdf") 
ggarrange(yaw,roll,pitch,common.legend = TRUE, legend="bottom", ncol = 1, nrow = 3)
dev.off() 




data <- read_csv('./results/student_quantile.csv')
data <- as.data.frame(data)
distance_data <- data[,2:ncol(data)]
rownames(distance_data) <- data$name
distance <- get_dist(distance_data)

pdf("results/quantile_distance.pdf") 
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off() 



yaw_mean <- data %>% select(ends_with('sd') & starts_with('yaw'))
yaw_mean$name <- data$name
long <- yaw_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile1_sd", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile2_sd", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile3_sd", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile4_sd", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "yaw_quantile5_sd", "80%-100%", as.character(quantile))) 

yaw <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "SD Yaw")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


roll_mean <- data %>% select(ends_with('sd') & starts_with('roll'))
roll_mean$name <- data$name
long <- roll_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "roll_quantile1_sd", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile2_sd", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile3_sd", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile4_sd", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "roll_quantile5_sd", "80%-100%", as.character(quantile))) 

roll <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "SD Roll")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


pitch_mean <- data %>% select(ends_with('sd') & starts_with('pitch'))
pitch_mean$name <- data$name
long <- pitch_mean %>% pivot_longer(!name, names_to = "quantile", values_to = "count")
long <- long %>%mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile1_sd", "0-20%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile2_sd", "20%-40%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile3_sd", "40%-60%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile4_sd", "60%-80%", as.character(quantile))) %>% 
  mutate(quantile = ifelse(as.character(quantile) == "pitch_quantile5_sd", "80%-100%", as.character(quantile))) 

pitch <- ggplot(data=long, aes(x=quantile, y=count,group=name)) +
  geom_line(aes(col=name),size =1) +
  labs(x="Quantile %", y = "SD Pitch")+
  theme_minimal()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position = "bottom")


pdf("results/quant-sd.pdf") 
ggarrange(yaw,roll,pitch,common.legend = TRUE, legend="bottom", ncol = 1, nrow = 3)
dev.off() 
