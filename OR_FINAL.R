library(dplyr)
library(tidyverse)
library(caret)
library(cluster)

# getwd()
# setwd('C:/Users/user/Desktop/설계/경영과학')

data <- read.csv('./경영과학 텀프로젝트.CSV', header=FALSE)
head(data)
View(data)


data_c <- kmeans(data, 3)
data_c$cluster
clusplot(data, data_c$cluster, color = TRUE, shade = TRUE,
         labels = 3, lines = 0)

str(data)