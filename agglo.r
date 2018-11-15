set.seed(2835)
# draw a sample of 40 records from the iris data, so that the
# clustering plot will not be over crowded

train<- read.csv("C:\\Users\\vvenkat2\\Downloads\\Walmart-Sales-Prediction-master\\Walmart-Sales-Prediction-master\\train.csv")
idx <- sample(1:dim(train)[1], 40)
train1 <- train[idx, ]
# remove class label
train1$IsHoliday <- NULL
# hierarchical clustering
hc <- hclust(dist(train1), method = "ave")
# plot clusters
plot(hc, hang = -1, labels = train$IsHoliday[idx])
# cut tree into 3 clusters
rect.hclust(hc, k = 2)
# get cluster IDs
groups <- cutree(hc, k = 2)
