setwd("E:/Files/av/ml/Test")
testcontacts = read.csv("Contacts2017.csv")
testresolutions = read.csv("Resolution2017.csv")
setwd("E:/Files/av/ml/Train")
contacts = read.csv("Contacts_Pre_2017.csv")
contracts_end = read.csv("Contracts_End.csv")
contracts_new = read.csv("Contracts_New.csv")
resolutions = read.csv("Resolution_Pre_2017.csv")
resolutions1 = resolutions[,c(3,8,9,13)]
library(plyr)
library(dplyr)
library(tidyr)
new_resolutions = aggregate(Resolution ~., data = resolutions1, FUN = sum)
new_resolutions = complete(new_resolutions, Date, Category, Subject, fill = list(Resolution = -2))
mx = (contacts$START.DATE == contacts$END.DATE)
table(mx)
contacts2 = subset(contacts, mx == TRUE)
str(contacts2)
str(new_resolutions)
library(lubridate)
dateconvert = as.Date(strptime(new_resolutions$Date, "%Y-%m-%d"))
new_resolutions$my_year = year(dateconvert)
new_resolutions$my_month = as.factor(months(dateconvert))
new_resolutions$my_day = as.factor(weekdays(dateconvert))
new_resolutions$my_date = as.factor(day(dateconvert))
dateconvert2 = as.Date(strptime(testresolutions$Date, "%Y-%m-%d"))
testresolutions$my_year = year(dateconvert2)
testresolutions$my_month = as.factor(months(dateconvert2))
testresolutions$my_day = as.factor(weekdays(dateconvert2))
testresolutions$my_date = as.factor(day(dateconvert2))
dateconvert3 = as.Date(strptime(contacts2$START.DATE, "%Y-%m-%d"))
contacts2$my_year = year(dateconvert3)
contacts2$my_month = as.factor(months(dateconvert3))
contacts2$my_day = as.factor(weekdays(dateconvert3))
contacts2$my_date = as.factor(day(dateconvert3))
dateconvert4 = as.Date(strptime(testcontacts$Date, "%Y-%m-%d"))
testcontacts$my_year = year(dateconvert4)
testcontacts$my_month = as.factor(months(dateconvert4))
testcontacts$my_day = as.factor(weekdays(dateconvert4))
testcontacts$my_date = as.factor(day(dateconvert4))

testresolutions_id = testresolutions$ID
testresolutions$ID = NULL
testresolutions$my_month = as.character(testresolutions$my_month)
new_resolutions$my_month = as.character(new_resolutions$my_month)
combine_res = rbind(new_resolutions, testresolutions)
str(combine_res)
combine_res$my_month = as.factor(combine_res$my_month)
new_resolutions2 = combine_res[c(1:nrow(new_resolutions)),]
test_resolutions = combine_res[-c(1:nrow(new_resolutions)),]
test_resolutions$ID  = testresolutions_id
str(contacts2)
str(testcontacts)
contacts2$END.DATE = NULL
testcontacts_id =  testcontacts$ID
testcontacts$ID = NULL
testcontacts$my_month = as.character(testcontacts$my_month)
contacts2$my_month = as.character(contacts2$my_month)
names(testcontacts)[names(testcontacts) == "Date"] = "START.DATE"
combine_con = rbind(contacts2, testcontacts)
str(combine_con)
combine_con$my_month = as.factor(combine_con$my_month)
new_contacts2 = combine_con[c(1:nrow(contacts2)),]
test_contacts = combine_con[-c(1:nrow(contacts2)),]
dim(new_contacts2) == dim(contacts2)
dim(testcontacts ) == dim(test_contacts)
test_contacts$ID = testcontacts_id

str(new_contacts2)
mycontacts = new_contacts2[,c(2,3,5,6,7)]
str(test_contacts)
test_contacts$Contacts = 1
mycontacts2 = test_contacts[,c(2,3,5,6,7)]
str(mycontacts2)
library(data.table)
dff = data.table(mycontacts, keep.rownames = F)
dff2 = data.table(mycontacts2, keep.rownames = F)

library(Matrix)
sparse_matrixx = sparse.model.matrix(Contacts ~., data = dff)
sparse_matrixx2 = sparse.model.matrix(Contacts ~.,data = dff2)

output = dff$Contacts
library(xgboost)
bstt = xgboost(data = sparse_matrixx, label = output, eta =0.2,  nrounds =400 ,
               objective = "reg:linear", metrics = "rmse")
?xgb.cv
bstt2 = xgb.cv(params =  list(eta = 0.2, objective = "reg:linear", metrics = "rmse"),
               data = sparse_matrixx, label = output, nrounds = 500, nfold = 3, verbose = T)

pred = predict(bstt, newdata = sparse_matrixx2)
pred = round(pred, digits = 0)
pred = ifelse(pred <= 0,0,pred)

submit = data.frame(ID = test_contacts$ID, Contacts = pred)
write.csv(submit, "Contacts.csv", row.names = FALSE)


# for resolution

str(new_resolutions2)
str(test_resolutions)
myresolutions = new_resolutions2[,c(2,3,4,6,7,8)]
test_resolutions$Resolution = 1
myresolutions2 = test_resolutions[,c(2,3,4,6:8)]
str(myresolutions2)
library(data.table)
df = data.table(myresolutions, keep.rownames = F)
df2 = data.table(myresolutions2, keep.rownames = F)

library(Matrix)
sparse_matrix = sparse.model.matrix(Resolution ~., data = df)
sparse_matrix2 = sparse.model.matrix(Resolution ~.,data = df2)

output = df$Resolution
library(xgboost)
bst = xgboost(data = sparse_matrix, label = output, eta =0.2,  nrounds = 400,
              objective = "reg:linear", metrics = "rmse")
?xgb.cv
bst2 = xgb.cv(params =  list(eta = 0.25, objective = "reg:linear", metrics = "rmse"),
              data = sparse_matrix, label = output, nrounds = 350, nfold = 10, verbose = T)

pred = predict(bst, newdata = sparse_matrix2)
pred = round(pred, digits = 0)
pred = ifelse(pred <= 0,0,pred)
submit = data.frame(ID = test_resolutions$ID, Resolution = pred)
write.csv(submit, "Resolution.csv", row.names = FALSE)
