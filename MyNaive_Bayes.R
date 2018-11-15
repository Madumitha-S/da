install.packages('caret')
library(caret)
install.packages('caTools')
library(caTools)
install.packages('e1071')
library(e1071)
train_df=as.data.frame(train)
train_df= subset(train_df,select = -c(Date))
#train_df$IsHoliday [train_df$IsHoliday == "true"] <- 1
#train_df$IsHoliday [train_df$IsHoliday == "false"] <- 0
train_df
Naive_Bayes_Model=naiveBayes(IsHoliday ~., data=train_df)
Naive_Bayes_Model
train_df
predict(Naive_Bayes_Model,train_df[sample(1:421570,150,replace=FALSE),],type="raw")
#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,train_df)
NB_Predictions
#Confusion matrix to check accuracy
mat<-table(NB_Predictions,train_df$IsHoliday)
mat
precision <- diag(mat) / rowSums(mat)
precision
recall <- (diag(mat) / colSums(mat))
recall
accuracy <- sum(diag(mat)) / sum(mat)
accuracy
