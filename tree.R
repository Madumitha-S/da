library(rpart)
library(party)
install.packages("caret")
library(caret)
str(train)
index <- createDataPartition(train$IsHoliday, times=1, p=0.2)[[1]]
training <- train[index, ]
validation <- train[-index, ]

decisionTreeModel <- rpart(IsHoliday ~ ., data=training, method="class", cp =0.5)

pred1 <- predict(decisionTreeModel, validation, type="class")

#confusionMatrix(validation$y, pred1)
confMat <- table(validation$IsHoliday,pred1)
print(confMat)

accuracy <- sum(diag(confMat))/sum(confMat)
print(accuracy*100)
install.packages("party")
library(party)
train_ctree <- ctree(IsHoliday ~ Store + Dept + Weekly_Sales, data = train)
print(train_ctree)
plot(train_ctree , type = 'simple')
