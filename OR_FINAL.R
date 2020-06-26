library(dplyr)
library(tidyverse)
library(caret)
library(cluster)
library(fpc)
library(ClusterR)
library(magrittr)
library(ggplot2)
library(co)
# getwd()
setwd('C:/Users/STUDENT/Desktop/비대면강의/경영과학2/OR2_DESIGNPROJECT') 
# working directory 설정
data <- read.csv('./경영과학 텀프로젝트.CSV', header=FALSE) # 데이터 불러오기
head(data)
View(data)
summary(data)
s_data <- scale(data)
summary(s_data)
set.seed(0)
co <- cor(data)
View(co)

# k means
data_k <- kmeans(s_data, 3, iter.max = 9) 
" iter max를 9로 설정한 이유는 여러 시행결과 가장 좋은 값이 9라서 설정함, 10으로 하면 클러스터 entity가 10개 초과되는 결과가 일어나고 9개 이하로 하면 문제 제한에서 가장 가까운 값이 안나와서 9로 설정함. "
data_k
summary(data_c)
data_k$cluster
data_k$centers
clusplot(data, data_k$cluster, color = TRUE, shade = TRUE,
         labels = 3, lines = 0) # 위의 클러스터 결과 시각화
str(data)
table(data_c$cluster)
data_kpp <- kmeansPP(s_data,3)

# k means same size 
# data_ss <- s_data
# k = 3
# data_ss %>% kmeans(k) -> kclust
# kdist = function(x1,y1,x2,y2){
#     sqrt((x1-x2)^2 + (y1-y2)^2)
# }
# 
# centers = kclust$centers
# data_ss %<>% mutate(D1=kdist(data_ss[-1], centers[1,1], centers[1,2]))
# data_ss %<>% mutate(D1=kdist(data_ss[-1], centers[2,1], centers[2,2]))
# data_ss %<>% mutate(D1=kdist(data_ss[-1], centers[3,1], centers[3,2]))
# 
# data_ss$Assign
# nassign = trunc(nrow(data_ss)/3)
# 
# for(i in 1:nassign){
#     
# }

# k medizn 
# data_p <- pam(s_data, 3, metric = "euclidean", stand = FALSE)
# clusplot(s_data, data_p$clustering, color=FALSE, shade = TRUE,
#          labels = 3, lines = 0)
# Density-based clustering
# summary(data) # row 별로 평균을 구해야하는데 summary는 attribute별로 함
# data_t <- t(data) # row별 평균과 중앙값을 구하기 위해 transpose
# sd(data)
# summary(data_t)
# 
# data_d <- dbscan(data, eps=3.5, MinPts=10)
# table(data_d$cluster)
# plot(data_d$cluster)

# Hierarchical clusering 
data_hc_c <- hclust(d=dist(s_data), method = 'complete')
data_hc_c
plot(data_hc_c, hang=-1)
rect.hclust(data_hc_c, k=3) # 3개로 partition
plot(silhouette(cutree(data_hc_c, k=3), dist=dist(s_data)))

# data_hc_a <- hclust(d=dist(s_data), method = 'average')
# data_hc_a
# plot(data_hc_a, hang=-1)
# rect.hclust(data_hc_a, k=3)
# plot(silhouette(cutree(data_hc_a, k=3), dist=dist(s_data)))
# 
# data_hc_s <- hclust(d=dist(s_data), method = 'single')
# data_hc_s
# plot(data_hc_s, hang=-1)
# rect.hclust(data_hc_s, k=3)
# plot(silhouette(cutree(data_hc_s, k=3), dist=dist(s_data)))
# 
# data_hc_m <- hclust(d=dist(s_data), method = 'median')
# data_hc_m
# plot(data_hc_m, hang=-1)
# rect.hclust(data_hc_m, k=3)
# plot(silhouette(cutree(data_hc_m, k=3), dist=dist(s_data)))


data_hc_w <- hclust(d=dist(s_data), method = 'ward.D2')
data_hc_w
plot(data_hc_w, hang=-1)
rect.hclust(data_hc_w, k=3) # 3개로 partition
plot(silhouette(cutree(data_hc_w, k=3), dist=dist(s_data)))

# optimal_cluster_GMM
op_gmm <- Optimal_Clusters_GMM(s_data,
                               3,
                               criterion = "BIC", # bayssian criterion
                               dist_mode = "eucl_dist", # dist is euclidean
                               seed_mode = "static_spread", # static sead mode
                               km_iter = 10, # km iter 10
                               plot_data = TRUE
                               )
op_gmm
plot(op_gmm)
