# Loading the Data

getwd()
setwd("E:/Files")
train = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

# Exploring the data

str(train)
which.max(nchar(train$abstract))
nchar(train$abstract[664])
x = nchar(train$abstract) == 0
table(x)

which.min(nchar(train$title))
train$title[1258]

# Preparing the Corpus

library(tm)
corpusTitle = Corpus(VectorSource(train$title))
corpusAbstract = Corpus(VectorSource(train$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords,stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

str(dtmTitle)
str(dtmAbstract)

# Preparing the corpus

which.max(colSums(dtmAbstract)) # most frequent word in abstract

# Building the model

colnames(dtmTitle) = paste0("T", colnames(dtmTitle)) # adding letter "T" in front of all variables
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract)) #adding letter "A" in front of all variables

trial = train$trial
dtm = cbind(dtmTitle, dtmAbstract, trial)
dim(dtm)
colnames(dtm)

library(caTools)
set.seed(144)
spl = sample.split(dtm$trial, SplitRatio = 0.7)
train1 = dtm[spl == TRUE,]
test1 = dtm[spl==FALSE,]

#baseline model accuracy
table(train1$trial)
730/nrow(train1)

library(rpart)
library(rpart.plot)

# CART model

trialcart = rpart(trial~., data =train1, method = "class")
prp(trialcart)
pred = predict(trialcart)
table(train1$trial, pred[,2]>0.5) 

# Evaluating the model on the testing set
predtest = predict(trialcart, newdata = test1)
table(test1$trial, predtest[,2]>0.5)

library(ROCR)
rocrpred = prediction(predtest[,2], test1$trial)
rocrperf = performance(rocrpred, "tpr", "fpr")
plot(rocrperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2,1.7))

auc = as.numeric(performance(rocrpred, "auc")@y.values) # area under curve
auc

# from the plot we can find good threshold value as per trade-off between true positive rate and false positive rate
