library(ggplot2)
library(readr)
data <-read.csv(file="E:\\Data Mining\\Walmart-Sales-Prediction-master\\merged_train.csv")
head(data)
set.seed(12)
library("e1071")
na.exclude(data)
data1 <- kmeans(na.omit(data),500) # this helps in omitting NA 
m <- kmeans(data[,2:6], 4, nstart=500)
m
table(m$cluster, data$IsHoliday)
