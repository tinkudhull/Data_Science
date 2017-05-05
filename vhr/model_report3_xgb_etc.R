getwd()
leave = read.csv("Leave_Report2.csv")
mm = duplicated(leave)
table(mm)
leave = leave[!duplicated(leave),]
str(leave)
table(leave$Status)
leave = subset(leave, leave$Status != "Pending")
leave$Status = as.factor(as.character(leave$Status))
str(leave)
prop.table(table(leave$Leave_Type, leave$Status), margin = 1)*100
prop.table(table(leave$Reason_for_leave, leave$Status), margin = 1)*100

library(rpart)
model_rpart = rpart(Status~Leave_Type+Department+Designation+No_of_Leaves+Reason_for_leave ,data = leave)
pred = predict(model_rpart, type = "class")
table(leave$Status, pred)

library(caret)
library(e1071)
library(rpart)
fitcontrol = trainControl(method = "cv", number = 10)
grid = expand.grid(.cp = seq(0.001,0.2,0.001))
library(caTools)
spl = sample.split(leave$Status, SplitRatio = 0.75)
train_leave = subset(leave, spl == TRUE)
test_leave = subset(leave, spl == FALSE)
str(train_leave)
model_caret = train(Status~Leave_Type+Department+Designation+No_of_Leaves+Reason_for_leave+Location,
                    data = train_leave,
                    method = "rpart", tuneGrid = grid, trControl = fitcontrol)
model_caret
model_1 = rpart(Status~Leave_Type+Department+Designation+No_of_Leaves+Reason_for_leave+Location,
                data = train_leave, 
                cp = 0.049)
pred = predict(model_1, newdata = test_leave, type = "class")
pre = predict(model_1, newdata = test_leave)
pre[c(1:100)]
table(test_leave$Status, pred)
library(pROC)
roc1 = roc(test_leave$Status, pre[,1])
plot(roc1, col = "blue")
roc1
pr = predict(model_1, type = "class")
table(train_leave$Status,pr)


library(C50)
str(train_leave)
model_c50 = C5.0(train_leave[,c(5,6,7,12,13,14)], train_leave[,8], trials = 2)
summary(model_c50)
predd = predict(model_c50, newdata = test_leave[,c(5,6,7,12,13,14)])
head(predd)
table(test_leave$Status, predd)
pr = predict(model_c50, newdata = train_leave[,c(5,6,7,12,13,14)])
table(train_leave$Status, pr)


str(train_leave)
train_leave1 = train_leave[,c(5,6,7,8,12,13,14)]
table(train_leave1$Status)
str(test_leave)
test_leave1 = test_leave[,c(5,6,7,8,12,13,14)]
library(DMwR)
train_leave2 = SMOTE(Status~., data = train_leave1, perc.over = 50, perc.under = 785)
table(train_leave2$Status)

tbmodel = train(Status~., data = train_leave1, method = "treebag", trControl = fitcontrol)
pred1 = predict(tbmodel, test_leave1[,-4], type = "prob")
library(pROC)
auc = roc(test_leave1$Status, pred1[,1])
print(auc)
plot(auc)

pred2 = predict(tbmodel, test_leave1[,-4])
table(test_leave1$Status, pred2 )


str(train_leave1)
train_leave2 = train_leave1
train_leave2$Status = ifelse(train_leave2$Status=="Approved",1,0)
test_leave2 = test_leave1
test_leave2$Status = ifelse(test_leave2$Status == "Approved",1,0)
str(test_leave1)
library(data.table)
df = data.table(train_leave2, keep.rownames = F)
dff =data.table(test_leave2, keep.rownames = F)
library(Matrix)
sparse_matrix = sparse.model.matrix(Status~., data = df)
sparse_matrixx =sparse.model.matrix(Status~., data = dff)
output = df$Status
library(xgboost)
?xgb.cv
bstt = xgboost(data = sparse_matrix, label = output, eta =0.05,  nrounds =22 ,    #0.1,10
               objective = "binary:logistic", metrics = "auc")

bsttt = xgb.cv(params = list(eta = 0.05, objective = "binary:logistic", metrics ="auc"),
               data =sparse_matrix,
               label = output,
               nrounds = 22,
               nfold = 10,
               verbose = T)

pred = predict(bstt, newdata = sparse_matrixx)
head(pred)
library(pROC)
aucc = roc(test_leave2$Status, pred)
plot(aucc)
print(aucc)
table(test_leave2$Status, pred > 0.5)

#glm model

table(leave$Designation, leave$Status)
leave$Designation = as.factor(as.character(leave$Designation))
str(leave)
model_glm = glm(Status~Designation+Department+Location+Reason_for_leave+No_of_Leaves+Leave_Type,
                data = leave, family = "binomial")
summary(model_glm)
predi = predict(model_glm, type = "response")
table(leave$Status, predi > 0.40)
library(pROC)
auc = roc(leave$Status, predi)
plot(auc)
print(auc)

#new glm model using selective dataset
str(leave)
leave2 = leave[,c(5:8,12:14)]
str(leave2)
table(leave2$Designation, leave2$Status)
leave3 = subset(leave2, leave2$Designation == "AVP" 
                |leave2$Designation =="Forex Intern"
                |leave2$Designation =="Forex Trader"
                |leave2$Designation =="Intern"
                |leave2$Designation =="Manager BPM"
                |leave2$Designation =="Product Development Intern"
                |leave2$Designation =="Sr Specialist BPM"
                |leave2$Designation =="Trainee Dealer")
leave3$Designation = as.factor(as.character(leave3$Designation))
str(leave3)

table(leave3$Department, leave3$Status)
leave4 = subset(leave3, leave3$Department == "Business Process Management"
                | leave3$Department == "Finance"
                | leave3$Department == "Not Available"
                | leave3$Department == "null"
                | leave3$Department == "Tech Infra")
leave4$Department = as.factor(as.character(leave4$Department))
str(leave4)

table(leave4$Location, leave4$Status)
table(leave4$Reason_for_leave, leave4$Status)
table(leave4$No_of_Leaves, leave4$Status)
boxplot(leave4$No_of_Leaves)

model_glm2 = glm(Status~Designation+Department+Location+Reason_for_leave+No_of_Leaves+Leave_Type,
                 data = leave4, family = "binomial")
summary(model_glm2)
predii = predict(model_glm2, type = "response")
predii[1:100]
table(leave4$Status, predii>0.385)
(1021+92)/(nrow(leave4))
aucc = roc(leave4$Status, predii)
plot(aucc)
print(aucc)
