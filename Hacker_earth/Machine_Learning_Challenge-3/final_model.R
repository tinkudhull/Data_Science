getwd()
setwd("E:/Files/he/ml")

# load libraries

library(data.table)
library(lubridate)
library(lightgbm)
library(Matrix)
# load data 

train <- fread("train.csv", na.strings = c(" ","",NA))
test <- fread("test.csv",na.strings = c(" ","",NA))

train[,ID := NULL]
#country code
train[,countrycode := as.integer(factor(countrycode))-1]
test[,countrycode := as.integer(factor(countrycode))-1]

#browser id
train[,browserid := ifelse(is.na(browserid) == TRUE, "Other", browserid)]
test[,browserid := ifelse(is.na(browserid) == TRUE, "Other", browserid)]

train[, browserid := factor(browserid)]
test[, browserid := factor(browserid)]


levels(train$browserid)
levels(train$browserid) = c("Chrome","Edge","Firefox","Chrome","IE","IE",
                            "IE","Firefox","Firefox","Opera","Other","Safari")

levels(test$browserid)
levels(test$browserid) = c("Chrome","Edge","Firefox","Chrome","IE","IE",
                           "IE","Firefox","Firefox","Opera","Other","Safari")

train[,browserid := as.integer(factor(browserid))-1]
test[,browserid := as.integer(factor(browserid))-1]

# device id
train[,devid := ifelse(is.na(devid) == TRUE, "Other", devid)]
test[,devid := ifelse(is.na(devid) == TRUE, "Other", devid)]

train[,devid := as.integer(factor(devid))-1]
test[,devid := as.integer(factor(devid))-1]

#site id
train[,siteid := ifelse(is.na(siteid) == TRUE, -999, siteid)]
test[,siteid := ifelse(is.na(siteid) == TRUE, -999, siteid)]

#datetime
train[,datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]
test[,datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")]


train[,tweekday := as.factor(weekdays(datetime))]
test[,tweekday := as.factor(weekdays(datetime))]
levels(train[,tweekday])
train[,tweekday := as.integer(tweekday)-1]
levels(test[,tweekday])
test[,tweekday := ifelse(tweekday == "Monday", 1,(ifelse(tweekday == "Saturday", 2,3)))]

train[,thour := hour(datetime)]
test[,thour := hour(datetime)]

train[,datetime := NULL]
test[,datetime := NULL]

#create some features
train[,siteid_count := .N, by = siteid]
train[,offerid_count := .N, by = offerid]
train[,category_count := .N, by = category]
train[,merchant_count := .N, by = merchant]
train[,siteid_offerid := .N, by = c("siteid", "offerid")]
train[,siteid_cate_mer := .N , by = c("siteid", "category", "merchant")]


test[,siteid_count := .N, by = siteid]
test[,offerid_count := .N, by = offerid]
test[,category_count := .N, by = category]
test[,merchant_count := .N, by = merchant]
test[,siteid_offerid := .N, by = c("siteid", "offerid")]
test[,siteid_cate_mer := .N , by = c("siteid", "category", "merchant")]


train[,siteid_count := siteid_count/max(siteid_count)]
train[,offerid_count := offerid_count/max(offerid_count)]
train[,category_count := category_count/max(category_count)]
train[,merchant_count := merchant_count/max(merchant_count)]
train[,siteid_offerid := siteid_offerid/max(siteid_offerid)]
train[,siteid_cate_mer := siteid_cate_mer/max(siteid_cate_mer)]


test[,siteid_count := siteid_count/max(siteid_count)]
test[,offerid_count := offerid_count/max(offerid_count)]
test[,category_count := category_count/max(category_count)]
test[,merchant_count := merchant_count/max(merchant_count)]
test[,siteid_offerid := siteid_offerid/max(siteid_offerid)]
test[,siteid_cate_mer := siteid_cate_mer/max(siteid_cate_mer)]


train[,siteid_countrycode := .N, by = c("siteid", "countrycode")]
train[,offerid_countrycode := .N, by = c("offerid", "countrycode")]
train[,category_countrycode := .N, by = c("category", "countrycode")]
train[,category_browserid := .N, by = c("category", "browserid")]
train[,merchant_countrycode := .N, by = c("merchant", "countrycode")]
train[,merchant_browserid := .N, by = c("merchant", "browserid")]

test[,siteid_countrycode := .N, by = c("siteid", "countrycode")]
test[,offerid_countrycode := .N, by = c("offerid", "countrycode")]
test[,category_countrycode := .N, by = c("category", "countrycode")]
test[,category_browserid := .N, by = c("category", "browserid")]
test[,merchant_countrycode := .N, by = c("merchant", "countrycode")]
test[,merchant_browserid := .N, by = c("merchant", "browserid")]



train[,siteid_countrycode := siteid_countrycode/max(siteid_countrycode)]
train[,offerid_countrycode := offerid_countrycode/max(offerid_countrycode)]
train[,merchant_countrycode := merchant_countrycode/max(merchant_countrycode)]
train[,category_countrycode := category_countrycode/max(category_countrycode)]
train[,merchant_browserid := merchant_browserid/max(merchant_browserid)]
train[,category_browserid := category_browserid/max(category_browserid)]

test[,siteid_countrycode := siteid_countrycode/max(siteid_countrycode)]
test[,offerid_countrycode := offerid_countrycode/max(offerid_countrycode)]
test[,merchant_countrycode := merchant_countrycode/max(merchant_countrycode)]
test[,category_countrycode := category_countrycode/max(category_countrycode)]
test[,merchant_browserid := merchant_browserid/max(merchant_browserid)]
test[,category_browserid := category_browserid/max(category_browserid)]


train[,siteid_off_cat_coun := .N, by = c("siteid","offerid","category","countrycode")]
train[,siteid_off_cat_bro := .N, by = c("siteid","offerid","category","browserid")]
train[,siteid_off_cat_dev := .N, by = c("siteid","offerid","category","devid")]

test[,siteid_off_cat_coun := .N, by = c("siteid","offerid","category","countrycode")]
test[,siteid_off_cat_bro := .N, by = c("siteid","offerid","category","browserid")]
test[,siteid_off_cat_dev := .N, by = c("siteid","offerid","category","devid")]

train[, siteid := NULL]
train[, offerid := NULL]
train[, category := NULL]
train[, merchant := NULL]

test[, siteid := NULL]
test[, offerid := NULL]
test[, category := NULL]
test[, merchant := NULL]

dtrain = lgb.Dataset(data = as.matrix(train[,-c('click')]),label = train$click)

bst11 <- lightgbm(data = dtrain,
                  max_depth = 9,
                  learning_rate = 0.2,
                  nrounds = 80,
                  objective = "binary",
                  metric = "auc"
)
predd = predict(bst11, data = as.matrix(test[,-1]))
sub <- data.table(ID = test$ID , click = predd)
fwrite(sub, "done.csv")