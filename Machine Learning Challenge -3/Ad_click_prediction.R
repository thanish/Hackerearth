
library(data.table)
library(MLmetrics)
library(ROCR)
library(dplyr)

#Reading the files
train_prod = fread('train.csv')
test_prod = fread('test.csv')

#Combining the train and test
test_prod[,click := NA]
train_test_prod = rbind(train_prod, test_prod)
summary(train_test_prod)
train_test_prod = data.table(train_test_prod)

#Filling up NA
train_test_prod[is.na(siteid), siteid := -999]

#Filling the NA in devid
table(train_test_prod$devid)
train_test_prod[devid=='', devid:= 'Not sure']
train_test_prod[, devid:= as.factor(as.character(devid))]

#Correcting the browserID
train_test_prod[browserid %in% c('Internet Explorer',  'InternetExplorer'),
                browserid:= "IE"]
train_test_prod[browserid %in% c('Mozilla', 'Firefox',  'Mozilla Firefox'),
                browserid:= "Firefox"]
train_test_prod[browserid %in% c('Google Chrome'),
                browserid:= "Chrome"]
train_test_prod[browserid=='', browserid:="Others"]
train_test_prod[,browserid:=as.factor(as.character(browserid))]

saveRDS(train_test_prod, 'train_test_prod.rds')

train_test_prod = readRDS('train_test_prod.rds')

#Extracting the days and times separately 
train_test_prod[,':='(click_hour  = as.numeric(strftime(datetime, "%H")),
                      click_min   = as.numeric(strftime(datetime, "%M")),
                      click_sec   = as.numeric(strftime(datetime, "%S")),
                      click_day   = as.numeric(strftime(datetime, "%d")),
                      click_month = as.numeric(strftime(datetime, "%m")),
                      click_year  = as.numeric(strftime(datetime, "%Y"))
                      )]
                      
#Convert the target column to factor
train_test_prod[, click := as.factor(click)]

saveRDS(train_test_prod, 'train_test_prod_1.rds')

train_test_prod = readRDS('train_test_prod_1.rds')
str(train_test_prod)

#Sort the records based on date-time
train_test_prod = train_test_prod[order(strftime(datetime, "%Y-%m-%d %H:%M:%S")),,]

#Adding the lag and lead date-time
train_test_prod[,':='(lag.date.time  = lag(datetime,1),
                      lead.date.time = lead(datetime,1))]

#Calculate the lag and lead time difference
train_test_prod[,':='(lag_time  = as.numeric(difftime(strftime(datetime, "%Y-%m-%d %H:%M:%S"),
                                                      strftime(lag.date.time, "%Y-%m-%d %H:%M:%S"),
                                                      units = "mins")),
                      lead_time = as.numeric(difftime(strftime(lead.date.time, "%Y-%m-%d %H:%M:%S"),
                                                      strftime(datetime, "%Y-%m-%d %H:%M:%S"),
                                                      units = "mins"))
                                                      )]
                                                      
#Replacing NA in the lag and lead time
train_test_prod[is.na(lag_time), lag_time:=0]
train_test_prod[is.na(lead_time),lead_time:=0]

saveRDS(train_test_prod, 'train_test_prod_2.rds')

train_test_prod = readRDS('train_test_prod_2.rds')
str(train_test_prod)
####################################
##Dropping of the columns
test_sub_id = train_test_prod$ID[is.na(train_test_prod$click)]
train_test_prod[, ':=' (ID = NULL,
                        datetime = NULL,
                        lag.date.time = NULL,
                        lead.date.time = NULL,
                        click_year = NULL,
                        click_month = NULL)]

#Creating the average lag time per category by hour
train_test_prod[, mean_lag_cat_hr := mean(lag_time), by = .(category, click_hour)]
train_test_prod[, mean_lag_ctry_brow_hr := mean(lag_time), by = .(countrycode, browserid, click_hour)]
train_test_prod[, mean_lag_ctry_brow_merchant_hr := mean(lag_time), by = .(countrycode, browserid, merchant, click_hour)]
train_test_prod[, count_lag_ctry_brow_hr := length(lag_time), by = .(countrycode, browserid, click_hour)]
#train_test_prod[, mean_lag_ctry_brw_dev_hr := mean(lag_time), by = .(countrycode, browserid, devid, click_hour)]

#Splitting into prod train and test
train_prod = train_test_prod[!is.na(click),]
test_prod = train_test_prod[is.na(click),]
str(train_prod)
str(test_prod)

#########################################################
#Splitting into local train and test
train_local = train_prod[click_day %in% 10:16,]
test_local = train_prod[click_day %in% 17:20,]
str(train_local)
str(test_local)

set.seed(100)
split = sample(nrow(train_local), 0.5*nrow(train_local), replace=F)

#####################################################################
#XGB
library(xgboost)
train_prod[,':='(countrycode = as.numeric(countrycode),
                 browserid = as.numeric(browserid),
                 devid = as.numeric(devid),
                 click = as.numeric(click)-1
                 )]
test_prod[,':='(countrycode = as.numeric(countrycode),
                browserid = as.numeric(browserid),
                devid = as.numeric(devid)
                )]
x_indep = setdiff(colnames(train_prod), c('mean_lag_ctry_brw_dev_hr', 'click_sec','click'))
y_dep = 'click'

dtrain_prod = xgb.DMatrix(data = as.matrix(train_prod[,x_indep, with=F]), label = as.matrix(train_prod[,y_dep, with=F]))
dtest_prod  = xgb.DMatrix(data = as.matrix(test_prod[,x_indep, with=F]))

set.seed(100)
xgb.local.model = xgb.train(data = dtrain_prod, 
                            nrounds = 175, 
                            max_depth = 6, 
                            eta = 0.3,
                            objective = "binary:logistic",
                            eval_metric = 'auc')

xgb.prod.pred = predict(xgb.local.model, newdata = dtest_prod)

xgb_imp = xgb.importance(model = xgb.local.model, feature_names = x_indep)
xgb.plot.importance(xgb_imp)

sub_XGB = data.frame(ID = test_sub_id,	click= xgb.prod.pred)

write.csv(sub_XGB, row.names=F, 'sub_XGB_13.csv')
