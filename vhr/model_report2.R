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

#glm model

table(leave$Designation, leave$Status)
leave$Designation = as.factor(as.character(leave$Designation))
str(leave)
model_glm = glm(Status~Designation+Department+Location+Reason_for_leave+No_of_Leaves+Leave_Type,
                data = leave, family = "binomial")
summary(model_glm)
predi = predict(model_glm, type = "response")
predi[1:100]
table(leave$Status, predi > 0.34)
(951+172)/nrow(leave)
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
model_glm2 = glm(Status~.,data = leave4, family = "binomial")
summary(model_glm2)
predii = predict(model_glm2, type = "response")
predii[1:100]
table(leave4$Status, predii>0.34)
(932+152)/(nrow(leave4))
aucc = roc(leave4$Status, predii)
plot(aucc, col = "red")
print(aucc)


# leave 5,
table(leave4$No_of_Leaves, leave4$Status)
leave5 = subset(leave4, leave4$No_of_Leaves < 16)
table(leave5$No_of_Leaves, leave5$Status)
leave5$No_of_Leaves = ifelse(leave5$No_of_Leaves > 10,11,leave5$No_of_Leaves)
model_glm3 = glm(Status~.,data = leave5, family = "binomial")
summary(model_glm3)
prediii = predict(model_glm3, type = "response")
predii[1:100]
table(leave5$Status, prediii>0.35)
(935+149)/(nrow(leave5))
auccc = roc(leave5$Status, prediii)
plot(auccc, add = TRUE, col = "blue")
print(auccc)

library(caret)
leave6 = leave5
fitcontrol = trainControl(method = "cv", number = 5)
model_glm_caret = train(Status~., data = leave6, method = "glm", trControl = fitcontrol)
summary(model_glm_caret)
pred_caret = predict(model_glm_caret, type = "prob")
table(leave6$Status, pred_caret[,2] > 0.34)
auc_caret = roc(leave6$Status, pred_caret[,2])
plot(auc_caret, add = TRUE, col = "black")
print(auc_caret)

# city wise
str(leave)
chandigarh = subset(leave, leave$Location == "Chandigarh")
ahmedabad = subset(leave, leave$Location == "Ahmedabad")
mumbai = subset(leave, leave$Location == "Mumbai")

chandigarh$Designation  = as.factor(as.character(chandigarh$Designation))
ahmedabad$Designation = as.factor(as.character(ahmedabad$Designation))
mumbai$Designation = as.factor(as.character(mumbai$Designation))
levels(chandigarh$Designation)
levels(ahmedabad$Designation)
levels(mumbai$Designation)

chandigarh$Department = as.factor(as.character(chandigarh$Department))
ahmedabad$Department = as.factor(as.character(ahmedabad$Department))
mumbai$Department = as.factor(as.character(mumbai$Department))
levels(chandigarh$Department)
levels(ahmedabad$Department)
levels(mumbai$Department)

chandigarh$Leave_Type = as.factor(as.character(chandigarh$Leave_Type))
ahmedabad$Leave_Type = as.factor(as.character(ahmedabad$Leave_Type))
mumbai$Leave_Type = as.factor(as.character(mumbai$Leave_Type))
levels(chandigarh$Leave_Type)
levels(ahmedabad$Leave_Type)
levels(mumbai$Leave_Type)

str(mumbai)
mumbai2 = mumbai[,c(5,6,8,12:14)]
str(mumbai2)
mumbai2$Reason_for_leave = as.factor(as.character(mumbai2$Reason_for_leave))
table(mumbai2$No_of_Leaves)
mumbai_glm = glm(Status~., data = mumbai2, family = "binomial")
pred_mumbai = predict(mumbai_glm, type = "response")
table(mumbai2$Status, pred_mumbai>0.39)
(591+149)/(nrow(mumbai2))
library(pROC)
auc_mumbai = roc(mumbai2$Status, pred_mumbai)
plot(auc_mumbai)
print(auc_mumbai)
591/(591+145)
149/(149+88)

str(ahmedabad)
ahmedabad2 = ahmedabad[,c(5,6,8,12:14)]
str(ahmedabad2)
table(ahmedabad2$Status)
table(ahmedabad2$Designation, ahmedabad2$Status)
table(ahmedabad2$Department, ahmedabad2$Status)
table(ahmedabad2$Reason_for_leave, ahmedabad2$Status)
ahmedabad2$Reason_for_leave = as.factor(as.character(ahmedabad2$Reason_for_leave))
table(ahmedabad2$No_of_Leaves)
ahmedabad_glm = glm(Status~., data = ahmedabad2, family = "binomial")
pred_ahmedabad = predict(ahmedabad_glm, type = "response")
table(ahmedabad2$Status, pred_ahmedabad>0.3)
(334+3)/(nrow(ahmedabad2))
library(pROC)
auc_ahmedabad = roc(ahmedabad2$Status, pred_ahmedabad)
plot(auc_ahmedabad)
print(auc_ahmedabad)

str(chandigarh)
chandigarh2 = chandigarh[,c(5,6,8,12:14)]
mnn = duplicated(chandigarh2)
table(mnn)


# mumbai feature engineering
str(mumbai)
mumbai$Job_Code = as.factor(as.character(mumbai$Job_Code))
table(mumbai$Job_Code, mumbai$Status)
str(mumbai2)
View(mumbai)
library(data.table)
mumbai = data.table(mumbai)
mumbai[,all_applications := .N, by = Leave_Start_Date]
str(mumbai)
View(mumbai)
#
mumbai2 = mumbai[,c(5,6,8,12:15)]
mumbai2$Reason_for_leave = as.factor(as.character(mumbai2$Reason_for_leave))
table(mumbai2$No_of_Leaves)
mumbai_glm = glm(Status~., data = mumbai2, family = "binomial")
pred_mumbai = predict(mumbai_glm, type = "response")
table(mumbai2$Status, pred_mumbai>0.4)
(591+149)/(nrow(mumbai2))
library(pROC)
auc_mumbai = roc(mumbai2$Status, pred_mumbai)
plot(auc_mumbai)
print(auc_mumbai)
