library(dplyr)
library(tidyverse)
library(caret)
library(cluster)
library(fpc)

# getwd()
setwd('C:/Users/STUDENT/Desktop/비대면강의/경영과학2/OR2_DESIGNPROJECT')

data <- read.csv('./경영과학 텀프로젝트.CSV', header=FALSE)
head(data)
View(data)

# k means
# cluster가 7, 13, 10 또는 10, 8, 12로 묶임
data_c <- kmeans(data, 3)
data_c
data_c$cluster
clusplot(data, data_c$cluster, color = TRUE, shade = TRUE,
         labels = 3, lines = 0)
str(data)

# Density-based clustering
summary(data) # row 별로 평균을 구해야하는데 summary는 attribute별로 함
data_t <- t(data) # row별 평균과 중앙값을 구하기 위해 transpose
summary(data_t)  
data_d <- dbscan(data, eps=9, MinPts = 10)
table(data_d$cluster)
plot(data_d)

# Hierarchical clusering 
data_hc <- hclust(d=dist(data), method = "average")
data_hc
plot(data_hc, hang=-1)
rect.hclust(data_hc, k=3)
