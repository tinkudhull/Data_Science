getwd()
leave = read.csv("Leave_Report.csv")
table(leave$Status)
str(leave)
leave = subset(leave, leave$Status != "Pending")
str(leave)
table(leave$Leave_Type )
leave$Status = as.factor(as.character(leave$Status))

library(rpart)
model_rpart = rpart(Status~Leave_Type+Department+Designation+No_of_Leaves, data = leave)
pred = predict(model_rpart, type = "class")
table(leave$Status, pred)

library(caret)
library(e1071)
fitcontrol = trainControl(method = "cv", number = 10)
grid = expand.grid(.cp = seq(0.001,0.2,0.001))
library(caTools)
spl = sample.split(leave$Status, SplitRatio = 0.75)
train_leave = subset(leave, spl == TRUE)
test_leave = subset(leave, spl == FALSE)
model_caret = train(Status~Leave_Type+Department+Designation+No_of_Leaves, data = train_leave,
                    method = "rpart", tuneGrid = grid, trControl = fitcontrol)
model_caret
model_1 = rpart(Status~Leave_Type+Department+Designation+No_of_Leaves, data = train_leave, 
                cp = 0.033)
pred = predict(model_1, newdata = test_leave, type = "class")
pre = predict(model_1, newdata = test_leave)
pre[c(1:100)]
table(test_leave$Status, pred)
library(pROC)
roc1 = roc(test_leave$Status, pre[,1])
plot(roc1, col = "blue")
roc1
pr = predict(model_1, type = "class")
table(pr, train_leave$Status)
