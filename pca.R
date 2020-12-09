library(tidyverse)
library(devtools)
library(ggbiplot)
library(readxl)


pca_data <- read_excel("data/pca_data.xlsx")
gender <- pca_data$Gender
pca_data <- pca_data[,1:10]
colnames(pca_data) <- c("name",paste('c',as.character(1:9),sep = ""))

pca <- prcomp(pca_data[,2:9], center = TRUE,scale. = TRUE)

## result
summary(pca)
pca$rotation


## visualization
pdf("results/pc1_pc2.pdf") 
ggbiplot(pca, labels=pca_data$name,groups = gender,ellipse=TRUE)+
  scale_colour_manual(name="Gender", values= c("red3", "dark blue"))+
  ggtitle("Principal Component Analysis")+
  theme_minimal()+
  theme(legend.position = "bottom")
dev.off()


pdf("results/pc1_pc3.pdf") 
ggbiplot(pca,choices = c(1,3), labels=pca_data$name,groups = gender,ellipse=TRUE)+
  scale_colour_manual(name="Gender", values= c("red3", "dark blue"))+
  ggtitle("Principal Component Analysis")+
  theme_minimal()+
  theme(legend.position = "bottom")
dev.off()

pdf("results/pc2_pc3.pdf") 
ggbiplot(pca,choices = c(2,3), labels=pca_data$name,groups = gender,ellipse=TRUE)+
  scale_colour_manual(name="Gender", values= c("red3", "dark blue"))+
  ggtitle("Principal Component Analysis")+
  theme_minimal()+
  theme(legend.position = "bottom")
dev.off()