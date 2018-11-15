# Read data
data1 = read.csv("Zomnumeric.csv")
View(data1)
# load library
library(caret)
library(e1071)

# Transforming the dependent variable to a factor
data1$Has.Table.booking = as.factor(data1$Has.Table.booking)
#Partitioning the data into training and validation data
set.seed(101)
index = createDataPartition(data1$Has.Table.booking, p = 0.7, list = F )
train = data1[index,]
validation = data1[-index,]
# Explore data
dim(train)
dim(validation)
names(train)
head(train)
head(validation)
# Setting levels for both training and validation data
levels(train$Has.Table.booking) <- make.names(levels(factor(train$Has.Table.booking)))
levels(validation$Has.Table.booking) <- make.names(levels(factor(validation$Has.Table.booking)))
# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)
model1 <- train(Has.Table.booking~. , data = train, method = "knn",
                preProcess = c("center","scale"),
                trControl = x,
                metric = "ROC",
                tuneLength = tunel)

# Summary of model
model1
plot(model1)
# Validation
valid_pred <- predict(model1,validation, type = "prob")

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],validation$Has.Table.booking)

# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,"auc")
perf_val

# Plot AUC
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col = "green", lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks

