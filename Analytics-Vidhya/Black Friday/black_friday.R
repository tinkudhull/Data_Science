# 1. Getting Started
getwd()
setwd("E:/Files/av/black")
library(data.table)
train = fread("train.csv", stringsAsFactors = T)
test = fread("test.csv", stringsAsFactors = T)
dim(train)
dim(test)
str(train)

# first prediction using mean
sub_mean = data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID, 
                      Purchase = mean(train$Purchase))
write.csv(sub_mean, "first_sub.csv", row.names = F)

summary(train)
summary(test)

# combine data set
test[,Purchase := mean(train$Purchase)]
c = list(train, test)
combin = rbindlist(c)
str(combin)

# 2. Data Exploration using data.table and ggplot
# analyzing gender variable
combin[,prop.table(table(Gender))]
# age variable
combin[,prop.table(table(Age))]
# city category variable
combin[,prop.table(table(City_Category))]
# Stay in Current Years Variable
combin[,prop.table(table(Stay_In_Current_City_Years))]
# unique values in ID variables
length(unique(combin$Product_ID))
length(unique(combin$User_ID))
# missing values
colSums(is.na(combin))

library(ggplot2)
# age vs gender
ggplot(combin, aes(Age, fill = Gender)) + geom_bar()
# age vs city category 
ggplot(combin, aes(Age, fill = City_Category)) + geom_bar()

library(gmodels)
CrossTable(combin$Occupation, combin$City_Category)
table(combin$Occupation, combin$City_Category)

# 3. data manipulation using data.table
# creating a new variable for missing values
combin[,Product_Category_2_NA := ifelse(sapply(combin$Product_Category_2, is.na) == 
                                          TRUE,1,0)]
combin[,Product_Category_3_NA := ifelse(sapply(combin$Product_Category_3, is.na) == 
                                          TRUE,1,0)]
#impute missing values
combin[,Product_Category_2 := ifelse(is.na(Product_Category_2) == TRUE, "-999",
                                     Product_Category_2)]
combin[,Product_Category_3 := ifelse(is.na(Product_Category_3) == TRUE, "-999",
                                     Product_Category_3)]
# set column level
levels(combin$Stay_In_Current_City_Years)[levels(combin$Stay_In_Current_City_Years) == 
                                            "4+"] = "4"
# recoding age groups
levels(combin$Age)[levels(combin$Age) == "0-17"] <- 0
levels(combin$Age)[levels(combin$Age) == "18-25"] <- 1
levels(combin$Age)[levels(combin$Age) == "26-35"] <- 2
levels(combin$Age)[levels(combin$Age) == "36-45"] <- 3
levels(combin$Age)[levels(combin$Age) == "46-50"] <- 4
levels(combin$Age)[levels(combin$Age) == "51-55"] <- 5
levels(combin$Age)[levels(combin$Age) == "55+"] <- 6

# convert age to numeric
combin$Age <- as.numeric(combin$Age)
# convert Gender into numeric
combin[,Gender := as.numeric(as.factor(Gender)) -1 ]
# user count
combin[,User_Count := .N, by = User_ID]
# product count
combin[,Product_Count := .N, by = Product_ID]
# Mean Purchase of Product
combin[, Mean_Purchase_Product := mean(Purchase), by = Product_ID]
# Mean Purchase of User
combin[, Mean_Purchase_User := mean(Purchase), by = User_ID]
library(dummies)
combin = dummy.data.frame(combin, names = c("City_Category"), sep = "_")
colnames(combin)
#check classes of all variables
sapply(combin, class)
#converting Product Category 2 & 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)

# 4. model building using h2o
# divide into train and test
c.train = combin[1:nrow(train),]
c.test = combin[-(1:nrow(train)), ]
c.train = c.train[c.train$Product_Category_1 <= 18,]
library(h2o)
localH2o = h2o.init(nthreads = -1)
h2o.init()

# data to h2o cluster
train.h2o = as.h2o(c.train)
test.h2o = as.h2o(c.test)
# check column index number 
colnames(train.h2o)
# dependent variable
y.dep  = 14
# independent variable 
x.indep = c(3:13,15:20)

# multiple regression in h2o
regression.model = h2o.glm(y = y.dep, x = x.indep, training_frame = train.h2o,
    family = "gaussian") #To do logistic regression, you can write family = "binomial"
h2o.performance(regression.model)
# make predictions
predict.reg = as.data.frame(h2o.predict(regression.model, test.h2o))
sub_reg =data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                    Purchase =  predict.reg$predict)
write.csv(sub_reg, file = "sub_reg.csv", row.names = F)

# Random forest in h2o
#random forest
system.time(
  rforest.model <- h2o.randomForest(y=y.dep, x=x.indep, training_frame = train.h2o,
                                    ntrees = 1000, mtries = 3, max_depth = 4, seed = 1122)
)
h2o.performance(rforest.model)
# check variable importance
h2o.varimp(rforest.model)
# making prediction on unseen data
system.time(predict.forest <- as.data.frame(h2o.predict(rforest.model, test.h2o)))
# writing submission file
sub_rf <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                     Purchase =  predict.forest$predict)
write.csv(sub_rf, file = "sub_rf.csv", row.names = F)

# GBM in h2o
# GBM
system.time(
  gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                       ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
)
h2o.performance(gbm.model)
# making prediction and writing submission file
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
sub_gbm <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                      Purchase = predict.gbm$predict)
write.csv(sub_gbm, file = "sub_gbm.csv", row.names = F)

# Deep learning in h2o
system.time(
  dlearning.model <- h2o.deeplearning(y = y.dep,x = x.indep,
                                      training_frame = train.h2o,
                                      epoch = 60, 
                                      hidden = c(100,100),
                                      activation = "Rectifier",
                                      seed = 1122)
)
h2o.performance(dlearning.model)
# making predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
# create a data frame and writing submission file
sub_dlearning <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                            Purchase = predict.dl2$predict)
write.csv(sub_dlearning, file = "sub_dlearning_new.csv", row.names = F)

# using grid ( search for hyperparameters) for gbm

?h2o.grid
system.time( 
  grid <- h2o.grid("gbm" ,y = y.dep, x = x.indep, training_frame = train.h2o,
                       hyper_params = list(ntrees = c(50,100,500),
                                           max_depth = c(5,6),seed = 1122,
                                           learn_rate = c(0.1))
))
summary(grid)
model_ids = grid@model_ids
models = lapply(model_ids, function(id) {h2o.getModel(id)})
summary(models)
grid@summary_table

system.time(
  gbm.model2 <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                       ntrees = 100, max_depth = 6, learn_rate = 0.1, seed = 1122)
)
h2o.performance(gbm.model2)
predict.gbm2 <- as.data.frame(h2o.predict(gbm.model2, test.h2o))
sub_gbm2 <- data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                      Purchase = predict.gbm2$predict)
write.csv(sub_gbm2, file = "sub_gbm2.csv", row.names = F)