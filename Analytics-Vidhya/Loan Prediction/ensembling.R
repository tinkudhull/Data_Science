getwd()
setwd("E:/Files/av")
library(caret)
set.seed(1)
data = read.csv("trainloanav.csv", na.strings = c(""," ",NA))
test = read.csv("testloanav.csv", na.strings = c("", " ",NA))
str(data)
sum(is.na(data))
sum(is.na(test))
summary(data)
summary(test)
str(data)
levels(data$Gender)
data$Gender[data$Gender == "NA"] = "Male"

# imputing missing values using median
preprocvalues = preProcess(data, method = c("medianImpute", "center", "scale"))
library(RANN)
data_processed = predict(preprocvalues,data)
summary(data_processed)

#splitting dataset into two parts
index = createDataPartition(data_processed$Loan_Status, p = 0.75, list = FALSE)
trainset = data_processed[index,]
testset = data_processed[-index,]

#defining the training controls for multiple models
fitcontrol = trainControl(method = "CV", number = 5, savePredictions = 'final', classProbs = T)

#defining the predictors and outcome
predictors = c("Credit_History", "LoanAmount","Loan_Amount_Term","ApplicantIncome",
               "CoapplicantIncome")
outcomename = 'Loan_Status'

# 1. training the random forest model 
library(randomForest)
library(caret)
model_rf = train(trainset[,predictors],trainset[,outcomename],method = 'rf',
                 trControl = fitcontrol, tuneLength = 3)
# predicting using random forest
testset$pred_rf = predict(object = model_rf, testset[,predictors])
# checking the accuracy of the random forest model
confusionMatrix(testset$Loan_Status, testset$pred_rf)

# 2. training the knn model
model_knn = train(trainset[,predictors], trainset[,outcomename], method = 'knn',
                  trControl = fitcontrol, tuneLength = 3)
# predicting using knn model
testset$pred_knn = predict(object = model_knn, testset[,predictors])
# checking the accuracy of the knn model
confusionMatrix(testset$Loan_Status, testset$pred_knn)

# 3. training the logistic model
model_lr = train(trainset[,predictors], trainset[,outcomename], method = 'glm',
                 trControl = fitcontrol, tuneLength = 3)
# predicting using logistic model 
testset$pred_lr = predict(object = model_lr, testset[,predictors])
# checking the accuracy of logistic model
confusionMatrix(testset$Loan_Status, testset$pred_lr)

# Ensembling by different methods
# 1. Averaging
# predicting the probabilities
testset$pred_rf_prob = predict(object = model_rf, testset[,predictors], type = 'prob')
testset$pred_knn_prob = predict(object = model_knn, testset[,predictors], type = 'prob')
testset$pred_lr_prob = predict(object = model_lr, testset[,predictors], type = 'prob')
# taking average of predictions
testset$pred_avg = (testset$pred_knn_prob$Y + testset$pred_lr_prob$Y + testset$pred_rf_prob$Y)/3
# splitting into binary classes at 0.5
testset$pred_avg = as.factor(ifelse(testset$pred_avg > 0.5, 'Y','N'))

# 2. Majority voting
testset$pred_majority = as.factor(ifelse(testset$pred_rf=='Y' & testset$pred_knn =='Y',
                               'Y',ifelse(testset$pred_rf=='Y' & testset$pred_lr=='Y',
                                        'Y', ifelse(testset$pred_knn == 'Y' &
                                                      testset$pred_lr =='Y','Y','N'))))
# 3. Weighted Average
# taking weighted average of predictions
testset$pred_weighted_avg = (testset$pred_rf_prob$Y*0.25) +(testset$pred_knn_prob$Y*0.25) +
                            (testset$pred_lr_prob$Y*0.5)
# splitting into binary classes at 0.5
testset$pred_weighted_avg = as.factor(ifelse(testset$pred_weighted_avg > 0.5, 'Y','N'))


# STEP 1. Train the individual base layer model on training data 
# defining the training control 
fitcontrol = trainControl(
  method = "CV", 
  number = 10,
  savePredictions = 'final', # to save out of fold predictions for best parameter combinations
  classProbs = T # to save the classification probabilities of the out of fold predictions
  )
# defining the predictors and outcome
predictors = c("Credit_History", "LoanAmount","Loan_Amount_Term","ApplicantIncome",
               "CoapplicantIncome")
outcomename = 'Loan_Status'
#training the random forest model
model_rf = train(trainset[,predictors],trainset[,outcomename], method = 'rf',
                 trControl = fitcontrol, tuneLength = 3)
#training the knn model 
model_knn = train(trainset[,predictors],trainset[,outcomename], method = 'knn',
                  trControl = fitcontrol, tuneLength = 3)
#training the logistic regression model 
model_lr = train(trainset[,predictors],trainset[,outcomename], method = 'glm',
                 trControl = fitcontrol, tuneLength = 3)

# STEP 2. predict using each base layer model for training data and test data
# predicting the out of fold prediction probabilities for training data
trainset$OOF_pred_rf = model_rf$pred$Y[order(model_rf$pred$rowIndex)]
trainset$OOF_pred_knn = model_knn$pred$Y[order(model_knn$pred$rowIndex)]
trainset$OOF_pred_lr = model_lr$pred$Y[order(model_lr$pred$rowIndex)]

# predicting probabilities for the test data
testset$OOF_pred_rf = predict(model_rf,testset[predictors],type = 'prob')$Y
testset$OOF_pred_knn = predict(model_knn,testset[predictors],type = 'prob')$Y
testset$OOF_pred_lr = predict(model_lr,testset[predictors],type = 'prob')$Y

# STEP 3. train the top layer model again on the predictions of the bottom layer models that 
#has been made on the training data

# predictors for top layer model
predictors_top = c('OOF_pred_knn','OOF_pred_lr','OOF_pred_rf')
# GBM as top layer model
model_gbm = train(trainset[,predictors_top],trainset[,outcomename],method = 'gbm',
                  trControl = fitcontrol, tuneLength = 3)
# logistic regression as top layer model
model_glm = train(trainset[,predictors_top],trainset[,outcomename],method = 'glm',
                        trControl = fitcontrol, tuneLength = 3)

# STEP 4. Finally, predict using the top layer model with the predictions of bottom
#layer models that has been made for testing data
# predict using gbm top layer model
testset$gbm_stacked = predict(model_gbm, testset[,predictors_top])
# predict using logistic regression as top layer model
testset$glm_stacked = predict(model_glm, testset[,predictors_top])
