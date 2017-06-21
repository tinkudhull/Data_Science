getwd()
setwd("C:/Users/Test User/Documents/Files/he")
train = read.csv("train.csv")
test = read.csv("test.csv")
sample = read.csv("sample_submission.csv")
str(train)
summary(train$AngleOfSign)
boxplot(train$SignAspectRatio)
boxplot(train$SignWidth)
boxplot(train$SignHeight)
table(train$SignFacing..Target.)
str(test)

trainid =  train$Id
testid = test$Id
train$Id = NULL
test$Id = NULL
test$SignFacing..Target. = 0

library(xgboost)
library(Matrix)
library(data.table)

df = data.table(train, keep.rownames = F)
dff = data.table(test, keep.rownames = F)

sparse_matrix = sparse.model.matrix(SignFacing..Target.~., data = df)
sparse_matrixx = sparse.model.matrix(SignFacing..Target.~., data = dff)

output =  as.integer(df$SignFacing..Target.)-1

params= list(objective = "multi:softprob", eta = 0.05,
             num_class = 4,eval_metric = "mlogloss")
bstt = xgb.cv(data = sparse_matrix, label = output, params = params, nfold = 10,
              nrounds = 160)
bst = xgboost(data = sparse_matrix, label = output, params = params, nrounds = 160)

pred = predict(bst, newdata = sparse_matrixx ,reshape = T)
head(pred)
str(sample)
submit = sample
pred = data.frame(pred)
str(pred)
submit$Front = pred$X1
submit$Left = pred$X2
submit$Rear = pred$X3
submit$Right = pred$X4
str(submit)
write.csv(submit, "submit.csv", row.names = F)

str(train)
library(dummies)
d.train = dummy.data.frame(data = train , names = ("DetectedCamera"), sep = "_")
d.test = dummy.data.frame(data = test , names = ("DetectedCamera"), sep = "_")
str(d.test)
