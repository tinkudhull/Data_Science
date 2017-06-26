getwd()
setwd("C:/Users/Test User/Documents/Files/he")
train = read.csv("train.csv")
test = read.csv("test.csv")
sample = read.csv("sample_submission.csv")

trainid =  train$Id
testid = test$Id
train$Id = NULL
test$Id = NULL
test$SignFacing..Target. = 0
str(train)
str(test)
train$SignAspectRatio = (train$SignWidth/train$SignHeight)*100
test$SignAspectRatio = (test$SignWidth/test$SignHeight)*100
train$new = (train$AngleOfSign/train$SignAspectRatio)
test$new = (test$AngleOfSign/test$SignAspectRatio)
library(xgboost)
library(Matrix)
library(data.table)
library(dplyr)
library(caret)
df = data.table(train, keep.rownames = F)
dff = data.table(test, keep.rownames = F)

sparse_matrix = sparse.model.matrix(SignFacing..Target.~., data = df)
sparse_matrixx = sparse.model.matrix(SignFacing..Target.~., data = dff)

output =  as.integer(df$SignFacing..Target.)-1

params= list(objective = "multi:softprob", eta = 0.05,
             num_class = 4,eval_metric = "mlogloss",
             max_depth = 4#, tree_method = "hist", grow_policy = "lossguide"
             )
bstt = xgb.cv(data = sparse_matrix, label = output, params = params, nfold = 10,
              nrounds = 300, prediction = T, early_stopping_rounds = 10)
oof_prediction = data.frame(bstt$pred) %>% 
  mutate(max_prob = max.col(.,ties.method  = "last"),
         label = output+1)
head(oof_prediction)
confusionMatrix(factor(oof_prediction$label),
                factor(oof_prediction$max_prob),
                mode = "everything")


bst = xgboost(data = sparse_matrix, label = output, params = params, nrounds = 271)

names = dimnames(sparse_matrix)[[2]]
importance_matrix = xgb.importance(names, model = bst)
importance_matrix


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
write.csv(submit, "submitoff.csv", row.names = F)

str(train)
str(test)
train$SignFacing..Target. = as.numeric(train$SignFacing..Target.)-1
combin = rbind(train, test)
str(combin)
library(dummies)
dummy_combin = dummy.data.frame(data = combin, names = c("DetectedCamera"), sep = "_")
str(dummy_combin)
train_data = dummy_combin[c(1:nrow(train)),]
test_data = dummy_combin[-c(1:nrow(train)),]
str(train_data)
str(test_data)

library(lightgbm)
dtrain = lgb.Dataset(data = as.matrix(train_data[,-9]), label = train_data$SignFacing..Target.)
dtest = lgb.Dataset(data = as.matrix(test_data[,-9]), label = test_data[,9])

nrounds = 400
param <- list(objective = "multiclass", num_class= 4 )
lgb.cv(param,
       dtrain,
       nrounds,
       nfold = 10,
       eval = "multi_logloss",
       early_stopping_rounds = 30,
       learning_rate = 0.02)

bst = lightgbm(data = dtrain,
               nrounds = 342,
               objective = "multiclass",
               num_class= 4,
               learning_rate = 0.02
)
predd = predict(bst, as.matrix(test_data[,-9]), reshape = T)
head(predd)
predd2 = as.data.frame(predd)
head(predd2)
submit2 = sample
submit2$Front = predd2$V1
submit2$Left = predd2$V2
submit2$Rear = predd2$V3
submit2$Right = predd2$V4

write.csv(submit2, "submit2.csv", row.names = F)
submit[156:161,]
submit2[156:161,]
str(sample)

submit3 = submit2
submit3$Front = (submit$Front + submit2$Front)/2
submit3$Left = (submit$Left + submit2$Left)/2
submit3$Rear = (submit$Rear + submit2$Rear)/2
submit3$Right = (submit$Right + submit2$Right)/2

write.csv(submit3, "submit3.csv", row.names = F)
str(submit3)
