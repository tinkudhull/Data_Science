getwd()
setwd("C:/Users/Test User/Documents/Files/New Folder")
train = read.csv("train.csv")
test = read.csv("test.csv")
str(train)
train$Made.Donation.in.March.2007 = as.factor(train$Made.Donation.in.March.2007)
str(test)
train$Total.Volume.Donated..c.c.. = NULL
test$Total.Volume.Donated..c.c.. = NULL
train$diff = (train$Months.since.First.Donation - train$Months.since.Last.Donation)
train$time.period = (train$diff / train$Number.of.Donations)
test$diff = (test$Months.since.First.Donation - test$Months.since.Last.Donation)
test$time.period = (test$diff / test$Number.of.Donations)
train$X = NULL
testx = test$X
test$X = NULL


model.glm = glm(Made.Donation.in.March.2007~., data = train, family = "binomial")
summary(model.glm)
p1 = predict(model.glm, type = "response")
p2 = predict(model.glm, newdata = test, type = "response")
submit = data.frame(testx, Made.Donation.in.March.2007 = p2)
write.csv(submit, "submit.csv", row.names = F)
logloss = function(actual, prediction){
  error = -(1/length(actual))*sum(actual*log(prediction) + (1-actual)*log(1-prediction))
  return(error)
}
logloss(as.numeric(train$Made.Donation.in.March.2007), p1)
str(train)
