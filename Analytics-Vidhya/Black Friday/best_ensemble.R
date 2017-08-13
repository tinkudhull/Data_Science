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
combin = dummy.data.frame(combin, names = c("City_Category"),
                          sep = "_")
colnames(combin)
#check classes of all variables
sapply(combin, class)
#converting Product Category 2 & 3
combin$Product_Category_2 <- as.integer(combin$Product_Category_2)
combin$Product_Category_3 <- as.integer(combin$Product_Category_3)
combin$Stay_In_Current_City_Years = NULL
str(combin)
# 4. model building using xgboost
# divide into train and test
library(xgboost)
combin$User_ID = NULL
combin$Product_ID = NULL
c.train = combin[1:nrow(train),]
c.test = combin[-(1:nrow(train)), ]
str(c.train)

library(Matrix)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
outcomename = 'Purchase'
predictors = names(c.train)[!names(c.train) %in% outcomename]
library(lightgbm)
sapply(c.train, class)
dtrain = lgb.Dataset(data = as.matrix(c.train[,predictors]),label = c.train[,outcomename])
lgb.cv(data = dtrain,
       learning_rate = 0.1,
       nrounds = 1000,
       max_depth = 8,
       nfold = 5,
       early_stopping_rounds = 10,
       objective = "regression",
       eval_metric = "rmse",
       verbose = 1)

bstl = lightgbm(data = dtrain,
                learning_rate = 0.1,
                nrounds = 800,
                max_depth = 8,
                objective = "regression",
                eval_metric = "rmse",
                verbose = 1)

predd = predict(bstl, data = as.matrix(c.test[,-11]))
head(predd)
fitcontrol = trainControl(#method = "repeatedcv",
  number = 3, verboseIter = TRUE)
#repeats = 5)
modelLookup(model = 'xgbTree')

xgb_grid = expand.grid(nrounds = 200, eta = c(0.1),
                       max_depth = c(12), gamma = 0,
                       colsample_bytree =1,min_child_weight=1,
                       subsample =1)

model.xgb = train(data.matrix(c.train[,predictors]), c.train[,outcomename],
                  method = 'xgbTree', trControl = fitcontrol,
                  tuneGrid = xgb_grid, verbose = 1)
pred2 = predict.train(model.xgb, data.matrix((c.test[,-16])))
sub_xgb2 =data.frame(User_ID = test$User_ID, Product_ID = test$Product_ID,
                     Purchase =  (pred2+predd)/2)
write.csv(sub_xgb2, file = "mix.csv", row.names = F)
