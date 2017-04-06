getwd()
setwd("E:/Files/kaggle/digit")
train = read.csv("train.csv")
test = read.csv("test.csv")
View(train)
str(train)
dim(test)

library(caTools)
train$label = as.factor(train$label)
spl = sample.split(train$label, SplitRatio = 0.7)
tr = subset(train, spl == TRUE)
te = subset(train, spl = F)
library(h2o)
localh20 = h2o.init(max_mem_size = '8g',nthreads = -1)
train_h20 = as.h2o(train)
test_h20 = as.h2o(test)
tr_h2o = as.h2o(tr)
te_h2o = as.h2o(te)

# using gbm

model.gbm <- h2o.gbm(x = 2:785, y = 1, training_frame = train_h20,
                     ntrees = 100, max_depth = 6, learn_rate = 0.01)

gbm.final.predict = h2o.predict(model.gbm,newdata = test_h20)
out = as.vector(as.numeric(gbm.final.predict$predict))
id = seq(1:28000)
submiss = data.frame(id,out,row.names = NULL)
colnames(submiss) = c("ImageId","Label")
write.csv(submiss,"gbmsub.csv",row.names = F)

# deep learning
system.time(
  dl.model <- h2o.deeplearning(y = 1,x = 2:785,
                               training_frame = tr_h2o,
                               epochs = 60, 
                               hidden = c(50,50),
                               activation = "Rectifier",
                               seed = 1122)
)

pr = h2o.predict(dl.model, newdata = te_h2o)
outcome = as.vector(as.factor(as.numeric(pr$predict)))
length(outcome)
table(te$label, outcome)


#using random forest

s <- proc.time()
RF = h2o.randomForest(x = 2:785, y = 1,training_frame = tr_h2o, ntrees = 100)
s - proc.time()

h2o.performance(RF)
RF.predict = h2o.predict(RF,newdata = te_h2o)
outcomes = as.vector(as.factor(as.numeric(RF.predict$predict)))
length(outcomes)
table(te$label,outcomes)

RF.final = h2o.randomForest(x = 2:785, y = 1,training_frame = train_h20)
RF.final.predict = h2o.predict(RF.final,newdata = test_h20)
out = as.vector(as.numeric(RF.final.predict$predict))
id = seq(1:28000)
submiss = data.frame(id,out,row.names = NULL)
head(submiss)
colnames(submiss) = c("ImageId","Label")
write.csv(submiss,"RFsub.csv",row.names = F)
