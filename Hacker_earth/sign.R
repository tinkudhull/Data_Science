getwd()
setwd("E:/Files/he")
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

params= list(objective = "multi:softprob", eta = 0.1, max_depth = 4,
             tree_method = "auto", num_class = 4,eval_metric = "mlogloss")
bstt = xgb.cv(data = sparse_matrix, label = output, params = params, nfold = 10,
               nrounds = 200)
