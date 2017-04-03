train = read.csv("train.csv")
test = read.csv("test.csv")
str(train)
summary(train)
for(i in 1:ncol(train)){
  train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
}

for(j in 1:ncol(test)){
  test[is.na(test[,j]), j] <- mean(test[,j], na.rm = TRUE)
}

summary(train)
summary(test)
str(train)

trainid = train$ID
testid = test$ID

train$ID = NULL
test$ID = NULL

str(train)
train$Outcome = as.factor(train$Outcome)
library(caret)
library(nnet)
train2 = train[sample(nrow(train), 10000),]
index = createDataPartition(train2$Outcome , p = 0.5, list = FALSE)
trainSet <- train2[ index,]
testSet <- train2[-index,]
str(trainSet)
trainSet$Outcome = as.factor(trainSet$Outcome)
mynnet = nnet(Outcome~., data = train,size = 10,
              decay = 0.01, maxit = 100 )
pr = predict(mynnet, test)
table(testSet$Outcome, pr>0.5)
dd = data.frame(ID = testid, Outcome = pr)
write.csv(dd, "dd.csv",row.names = FALSE)
