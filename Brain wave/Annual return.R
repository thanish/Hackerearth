
#Laptop directory
setwd('E:/Thanish/Data science/Hackerearth/SG - Brain wave/Predict Annual Returns')

#Office laptop
setwd('C:/Users/BTHANISH/Desktop/Thanish/Competition/Hacker earth/SG - Brain wave/Predict Annual Returns')

library(data.table)
library(MLmetrics)
library(lubridate)
library(caret)
library(randomForest)
library(dplyr)

#sample file for submission
sub = fread('sample_submission.csv')

train_prod = fread('train.csv')
test_prod = fread('test.csv')

#Binding the train and test
test_prod[, return := NA]
train_test_prod = rbind(train_prod, test_prod)

#EXtracting the factor and logical columns
fac_cols = colnames(train_test_prod)[unlist(lapply(train_test_prod, FUN = function(x){class(x) == 'character'}))]
log_cols = colnames(train_test_prod)[unlist(lapply(train_test_prod, FUN = function(x){class(x) == 'logical'}))]
num_cols = colnames(train_test_prod)[unlist(lapply(train_test_prod, FUN = function(x){class(x) == 'integer' | class(x) == 'numeric'}))]
num_cols = setdiff(num_cols, 'return')

#Filling up the empty factor columns 
train_test_prod[, (fac_cols):= lapply(fac_cols, function(x) {x = get(x)
                                                             x[x == '' ] <- 'not_sure'
                                                             x}) ]

#Filling up the empty log columns 
train_test_prod[, (log_cols):= lapply(log_cols, function(x) {x = get(x)
                                                             x[is.na(x)] <- 'not_sure'
                                                             x}) ]

#Filling up the empty numeric columns 
train_test_prod[, (num_cols):= lapply(num_cols, function(x) {x = get(x)
                                                             x[is.na(x)] <- -999
                                                             x}) ]


#####################################################################################################
#Converting the date column to the right format
head(train_test_prod)
train_test_prod[, ':=' (start_date = paste(substr(start_date, start = 1, stop = 4), 
                                           substr(start_date, start = 5, stop = 6),
                                           substr(start_date, start = 7, stop = 8), sep = '-'),
                        sell_date = paste(substr(sell_date, start = 1, stop = 4), 
                                          substr(sell_date, start = 5, stop = 6),
                                          substr(sell_date, start = 7, stop = 8), sep = '-'),
                        creation_date = paste(substr(creation_date, start = 1, stop = 4), 
                                              substr(creation_date, start = 5, stop = 6),
                                              substr(creation_date, start = 7, stop = 8), sep = '-')
                        )]

train_test_prod[ , ':=' (start_date = as.POSIXct(start_date, format = "%Y-%m-%d"),
                         sell_date = as.POSIXct(sell_date, format = "%Y-%m-%d"),
                         creation_date = as.POSIXct(creation_date, format = "%Y-%m-%d"))]

#Getting the different components of dat seperately
train_test_prod[ , ':=' (start_date_year = as.numeric(format(start_date, "%Y")),
                         start_date_month = as.numeric(format(start_date, "%m")),
                         start_date_day = as.numeric(format(start_date, "%d")),
                         sell_date_year = as.numeric(format(sell_date, "%Y")),
                         sell_date_month = as.numeric(format(sell_date, "%m")),
                         sell_date_day = as.numeric(format(sell_date, "%d")),
                         creation_date_year = as.numeric(format(creation_date, "%Y")),
                         creation_date_month = as.numeric(format(creation_date, "%m")),
                         creation_date_day = as.numeric(format(creation_date, "%d")))]

#Getting the time diff of start date and end date and day of the week
train_test_prod[, ':=' (time_2_deliver = as.numeric(difftime(start_date, creation_date, units = 'hours')),
                        retention_date = as.numeric(difftime(start_date, sell_date, units = 'hours')),
                        date_since_created = as.numeric(difftime(creation_date, sell_date, units = 'hours')),
                        start_date_weekday = as.numeric(format(start_date, format = "%u")),
                        creation_date_weekday = as.numeric(format(creation_date, format = "%u")),
                        sell_date_weekday = as.numeric(format(sell_date, format = "%u")) 
                        )]



#Calculate the % of profit
train_test_prod[, profit := ((sold - bought)/bought)]
train_test_prod[, lib_per_euri := libor_rate/euribor_rate]
#####################################################################################################
#Dropping of the date column
train_test_prod[ , ':=' (start_date = NULL,
                         sell_date = NULL,
                         creation_date = NULL)]

#Converting the character column to factor and then to numerics
#After filling the NA the log files convert to factor so merging with factor
new_fac_cols = c(fac_cols, log_cols)
train_test_prod[, (new_fac_cols) := lapply(.SD, as.factor), .SDcols=new_fac_cols]
train_test_prod[, (new_fac_cols) := lapply(.SD, as.numeric), .SDcols=new_fac_cols]

#Filling the numeric NA's with -999
num_cols = colnames(train_test_prod)[unlist(lapply(train_test_prod, FUN = function(x){class(x) == 'integer' | class(x) == 'numeric'}))]
num_cols = setdiff(num_cols, 'return')
#Filling up the empty numeric columns 
train_test_prod[, (num_cols):= lapply(num_cols, function(x) {x = get(x)
                                                             x[is.na(x)] <- -999
                                                             x}) ]

#train and test prod seperation
train_prod = train_test_prod[!is.na(return),]
test_prod = train_test_prod[is.na(return),]

################################################################################
library(h2o)
h2o.init(nthreads = -1, min_mem_size = '20g')

train_prod_h2o = as.h2o(train_prod)
test_prod_h2o = as.h2o(test_prod)

x_indep = setdiff(colnames(train_local_h2o), 'return')
y_dep =  'return'

#DL_10
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 1000, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100, 100),
                                      mini_batch_size = 10,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_1 = DL.h2o.pred.prod$predict

#DL_11
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 1000, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100, 100, 100),
                                      mini_batch_size = 10,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_2 = DL.h2o.pred.prod$predict

#DL_12
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 1000, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100, 100),
                                      mini_batch_size = 20,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_3 = DL.h2o.pred.prod$predict

#DL_18
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 850, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100),
                                      mini_batch_size = 10,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      validation_frame = test_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_4 = DL.h2o.pred.prod$predicts

#DL_19
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 790,
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100),
                                      mini_batch_size = 15,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      validation_frame = test_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_5 = DL.h2o.pred.prod$predict

#DL_20
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 1200, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100),
                                      mini_batch_size = 5,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_6 = DL.h2o.pred.prod$predict

#DL_21
DL.h2o.model.local = h2o.deeplearning(x = x_indep, y = y_dep,
                                      epoch = 1200, 
                                      stopping_rounds = 0,
                                      hidden = c(100, 100, 100, 100, 100),
                                      mini_batch_size = 10,
                                      rate = 0.003,
                                      training_frame = train_prod_h2o,
                                      verbose = T,
                                      seed = 100)

DL.h2o.pred.prod = predict(DL.h2o.model.local, newdata = test_prod_h2o)
DL.h2o.pred.prod = as.data.frame(DL.h2o.pred.prod)
pred_7 = DL.h2o.pred.prod$predict

ens_sub = (pred_1 + pred_2 + pred_3 + pred_4 + pred_5 + pred_6 + pred_7)/7

sub_DL_h2o = data.frame(portfolio_id = sub$portfolio_id, return = ens_sub)
write.csv(sub_DL_h2o, row.names = F, 'sub_DL_h2o.csv')



