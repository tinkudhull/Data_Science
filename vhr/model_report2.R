getwd()
leave = read.csv("Leave_Report2.csv")
str(leave)
mm = duplicated(leave)
table(mm)
leave = leave[!duplicated(leave),]
str(leave)
table(leave$Status)
leave = subset(leave, leave$Status != "Pending")
leave$Status = as.factor(as.character(leave$Status))
str(leave)
table(leave$Status)
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
predi[1:100] *100
table(leave$Status, predi > 0.34)
(951+172)/nrow(leave)
951/(951+169) # Sensitivity
172/(172+114) # Specificity

library(pROC)
auc = roc(leave$Status, predi)
plot(auc)
print(auc)


#new glm model using selective dataset
str(leave)
leave2 = leave
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
library(data.table)
leave4 = data.table(leave4)
leave4[,all_applications := .N, by = c("Leave_Start_Date", "Location")]
leave4$full_name = paste(leave4$First_Name,leave4$Middle_Name,leave4$Last_Name)
leave4[,avg_applications := mean(No_of_Leaves), by = full_name ]
str(leave4)
leave5 = leave4[,c(5:8,12:15,17)]
str(leave5)

model_glm2 = glm(Status~.,data = leave5, family = "binomial")
summary(model_glm2)
predii = predict(model_glm2, type = "response")
predii[1:100]*100
table(leave5$Status, predii>0.34)
(914+167)/(nrow(leave5))
914/(914+166) # Sensitivity
167/(167+109) # Specificity
aucc = roc(leave4$Status, predii)
plot(aucc, col = "red")
print(aucc)

library(caret)
leave6 = leave5
fitcontrol = trainControl(method = "cv", number = 5)
model_glm_caret = train(Status~., data = leave6, method = "glm", trControl = fitcontrol)
summary(model_glm_caret)
pred_caret = predict(model_glm_caret, type = "prob")
pred_caret[c(1:100),2]
table(leave6$Status, pred_caret[,2] > 0.34)
(914+167)/(nrow(leave6))
914/(914+166) # Sensitivity
167/(167+109) # Specificity

auc_caret = roc(leave6$Status, pred_caret[,2])
plot(auc_caret, col = "black")
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

#ahmedabad
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

# mumbai feature engineering
str(mumbai)
mumbai$Job_Code = as.factor(as.character(mumbai$Job_Code))
table(mumbai$Job_Code, mumbai$Status)
library(data.table)
mumbai = data.table(mumbai)
mumbai[,all_applications := .N, by = Leave_Start_Date]
mumbai$full_name = paste(mumbai$First_Name,mumbai$Middle_Name,mumbai$Last_Name)
mumbai[,avg_applications := mean(No_of_Leaves), by = full_name ]
str(mumbai)

#mumbai glm with cv
names(mumbai)
mumbai2 = mumbai[,c(5,6,8,12:15,17)]
str(mumbai2)
table(mumbai2$Designation, mumbai2$Status)
table(mumbai2$Department, mumbai2$Status)
table(mumbai2$Reason_for_leave, mumbai2$Status)
table(mumbai2$Leave_Type, mumbai2$Status)
mumbai2$Reason_for_leave = as.factor(as.character(mumbai2$Reason_for_leave))
table(mumbai2$No_of_Leaves)
mumbai_glm = glm(Status~., data = mumbai2, family = "binomial")
summary(mumbai_glm)
pred_mumbai = predict(mumbai_glm, type = "response")
table(mumbai2$Status, pred_mumbai>0.362)
(593+154)/(nrow(mumbai2))
593/(593+143) # Sensitivity
154/(154+83) # Specificity

library(pROC)
auc_mumbai = roc(mumbai2$Status, pred_mumbai)
plot(auc_mumbai, col = "maroon", add = T)
print(auc_mumbai)

#gbm
library(caret)
ctrl = trainControl(method = "cv", number = 10)
mumbai_caret_gbm = train(Status~., data = mumbai2, method = "gbm", trControl = ctrl)
caret_pred = predict(mumbai_caret_gbm, type = "prob")
head(caret_pred)
table(mumbai2$Status, caret_pred[,2]>0.36)
(648+176)/(nrow(mumbai2))
648/(648+88) # Sensitivity
176/(174+61) # Specificity

caret_auc_mumbai = roc(mumbai2$Status, caret_pred[,2])
plot(caret_auc_mumbai, add = T)
print(caret_auc_mumbai)

set.seed(100)
index = createDataPartition(mumbai2$Status, p = 0.8, list = FALSE)
train_mumbai = mumbai2[index,]
test_mumbai = mumbai2[-index,]
ctrl = trainControl(method = "cv", number = 10)
mumbai_caret_gbm = train(Status~., data = train_mumbai, method = "gbm", trControl = ctrl)
caret_pred = predict(mumbai_caret_gbm,newdata = test_mumbai, type = "prob")
head(caret_pred)
table(test_mumbai$Status, caret_pred[,2]>0.32)
(125+33)/(nrow(test_mumbai))
125/(125+22)
33/(33+14)
train_pred = predict(mumbai_caret_gbm, type = "prob")
table(train_mumbai$Status, train_pred[,2] > 0.32)
plot(roc(train_mumbai$Status, train_pred[,2]))
print(roc(train_mumbai$Status, train_pred[,2]))

str(leave4)
leave_mumbai = subset(leave4, Location == "Mumbai")
str(leave_mumbai)
leave_mumbai$Location = NULL
library(ggplot2)
ggplot(data = leave_mumbai, aes(Designation, fill = Status)) + 
  geom_bar() + 
  theme(legend.title = element_text(colour = "black", size = 13, face = "bold"),
        axis.text.x = element_text(angle = 35, size = 13, face = "bold", color = "black"),
        axis.title.x = element_text(size = 14, vjust = 8)) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.9) +
  labs(title = "Mumbai data ", subtitle = "Leave Status Based on Designation") +
  xlab("Designation")

ggplot(data = leave_mumbai, aes(Department, fill = Status)) + 
  geom_bar() + theme(legend.title = element_text(colour = "black", size = 13, face = "bold"),
                     axis.text.x = element_text(angle = 35, size = 14, face = "bold", color = "black"),
                     axis.title.x = element_text(size = 14, vjust = 8)) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.9) +
  labs(title = "Mumbai data ", subtitle = "Leave Status Based on Department") +
  xlab("Department")

ggplot(data = leave_mumbai, aes(Reason_for_leave, fill = Status)) + 
  geom_bar() + theme(legend.title = element_text(colour = "black", size = 13, face = "bold"), 
                     axis.text.x =  element_text(angle = 45, size = 12, face = "bold"),
                     axis.title.x = element_text(size = 14, vjust = 8)) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.9)+
  labs(title = "Mumbai data ", subtitle = "Leave Status Based on Reason of Leave") +
  xlab("Reason of Leave")


ggplot(data = leave_mumbai, aes(Leave_Type, fill = Status)) + 
  geom_bar() + theme(legend.title = element_text(colour = "black", size = 13, face = "bold"),
                     axis.text.x =  element_text(angle = 45, size = 12, face = "bold"),
                     axis.title.x = element_text(size = 14, vjust = 28)) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.9)+
  labs(title = "Mumbai data ", subtitle = "Leave Status Based on Leave Type") +
  xlab("Leave Type")
