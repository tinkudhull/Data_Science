getwd()
setwd("E:/Files/kaggle/digit")
library(mxnet)
train <- data.matrix(read.csv("train.csv", header=T))
test <- data.matrix(read.csv("test.csv", header=T))
dim(train[,-1])
dim(test)

barplot(table(train[,1]), col = rainbow(10,0.5), main = "n digits in train")

plotTrain = function(images) {
  op = par(no.readonly = TRUE)
  x <- ceiling(sqrt(length(images)))
  par(mfrow=c(x, x), mar=c(.1, .1, .1, .1))
  for (i in images){ #reverse and transpose each matrix to rotate images
    m <- matrix(train[i,-1], nrow=28, byrow=TRUE)
    m <- apply(m, 2, rev)
    image(t(m), col=grey.colors(255), axes=FALSE)
    text(0.05, 0.2, col="white", cex=1.2, train[i, 1])
  }
  par(op) #reset the original graphics parameters
}

plotTrain((1:36))

train.x <- train[,-1]
train.y <- train[,1]
train.x <- t(train.x/255)
test.x <- t(test/255)

# CNN
m2.data <- mx.symbol.Variable("data")
# 1st Convolutional Layer
m2.conv1 = mx.symbol.Convolution(m2.data, kernel = c(5,5), num_filter = 16)
m2.bn1 = mx.symbol.BatchNorm(m2.conv1)
m2.act1 = mx.symbol.Activation(m2.bn1, act_type = "relu")
m2.pool1 = mx.symbol.Pooling(m2.act1, pool_type = "max",
                             kernel = c(2,2), stride = c(2,2))
m2.drop1 = mx.symbol.Dropout(m2.pool1, p = 0.5)
# 2nd Convolutional Layer
m2.conv2 <- mx.symbol.Convolution(m2.drop1, kernel=c(3,3), num_filter=32)
m2.bn2 <- mx.symbol.BatchNorm(m2.conv2)
m2.act2 <- mx.symbol.Activation(m2.bn2, act_type="relu")
m2.pool2 <- mx.symbol.Pooling(m2.act2, pool_type="max",
                              kernel=c(2,2), stride=c(2,2))
m2.drop2 <- mx.symbol.Dropout(m2.pool2, p=0.5)
m2.flatten <- mx.symbol.Flatten(m2.drop2)
# 4 Fully Connected Layers
m2.fc1 = mx.symbol.FullyConnected(m2.flatten, num_hidden = 1024)
m2.act3 = mx.symbol.Activation(m2.fc1, act_type = "relu")

m2.fc2 <- mx.symbol.FullyConnected(m2.act3, num_hidden=512)
m2.act4 <- mx.symbol.Activation(m2.fc2, act_type="relu")

m2.fc3 <- mx.symbol.FullyConnected(m2.act4, num_hidden=256)
m2.act5 <- mx.symbol.Activation(m2.fc3, act_type="relu")

m2.fc4 <- mx.symbol.FullyConnected(m2.act5, num_hidden=10)
m2.softmax <- mx.symbol.SoftmaxOutput(m2.fc4)

train.array = train.x
dim(train.array)  = c(28, 28, 1, ncol(train.x))
test.array <- test.x
dim(test.array) <- c(28, 28, 1, ncol(test.x))

log <- mx.metric.logger$new() 
tick <- proc.time() 
mx.set.seed(0)

m2 = mx.model.FeedForward.create(m2.softmax,
                                 X = train.array,
                                 y = train.y,
                                 num.round = 220,
                                 array.batch.size = 500,
                                 array.layout = "colmajor",
                                 learning.rate = 0.01,
                                 momentum = 0.91,
                                 wd = 0.00001,
                                 eval.metric = mx.metric.accuracy,
                                 initializer = mx.init.uniform(0.07),
                                 epoch.end.callback = mx.callback.log.train.metric(1, log)
                                 )
print(paste("Training took:", round((proc.time() - tick)[3],2),"seconds"))

m2.preds <- predict(m2, test.array)
m2.preds.value <- max.col(t(m2.preds)) - 1


submission <- data.frame(ImageId=1:ncol(test.x), Label=m2.preds.value)
write.csv(submission, file='submission_m2.csv', row.names=FALSE, quote=FALSE)