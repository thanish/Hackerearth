#Desktop
setwd('I:\\Data science\\Data science competition\\Hackers earth\\Bank Fear lonliness')

library(ROCR)
train = read.csv('train_indessa.csv')
test = read.csv('test_indessa.csv')

#Dropping the ANY in the home_ownership in train since it is not in test
train = train[train$home_ownership!='ANY',]

#Joining the train and test dataset
train$type = 'train'
test$type  ='test'
test$loan_status = NA
train_test_prod = rbind(train, test)

#Setting the format
train_test_prod$loan_status = as.factor(train_test_prod$loan_status)
str(train_test_prod)

##################################################################################################################
#Filling the outliers with percentile
#annual_inc
quan = quantile(train_test_prod$annual_inc, na.rm = T, 0.99)
train_test_prod$annual_inc[train_test_prod$annual_inc > quan] = quan

#loan_amnt
quan = quantile(train_test_prod$loan_amnt, na.rm = T, 0.99)
train_test_prod$loan_amnt[train_test_prod$loan_amnt > quan] = quan

#funded_amnt
quan = quantile(train_test_prod$funded_amnt, na.rm = T, 0.99)
train_test_prod$funded_amnt[train_test_prod$funded_amnt > quan] = quan

#funded_amnt_inv
quan = quantile(train_test_prod$funded_amnt_inv, na.rm = T, 0.99)
train_test_prod$funded_amnt_inv[train_test_prod$funded_amnt_inv > quan] = quan

#int_rate
quan = quantile(train_test_prod$int_rate, na.rm = T, 0.99)
train_test_prod$int_rate[train_test_prod$int_rate > quan] = quan

#annual_inc
quan = quantile(train_test_prod$annual_inc, na.rm = T, 0.99)
train_test_prod$annual_inc[train_test_prod$annual_inc > quan] = quan

#dti
quan = quantile(train_test_prod$dti, na.rm = T, 0.99)
train_test_prod$dti[train_test_prod$dti > quan] = quan

#delinq_2yrs - Not needed
quan = quantile(train_test_prod$delinq_2yrs, na.rm = T, 0.99)
train_test_prod$delinq_2yrs[train_test_prod$delinq_2yrs > quan] = quan

#inq_last_6mths
quan = quantile(train_test_prod$inq_last_6mths, na.rm = T, 0.99)
train_test_prod$inq_last_6mths[train_test_prod$inq_last_6mths > quan] = quan

#mths_since_last_delinq
quan = quantile(train_test_prod$mths_since_last_delinq, na.rm = T, 0.99)
train_test_prod$mths_since_last_delinq[train_test_prod$mths_since_last_delinq > quan] = quan

#mths_since_last_record
quan = quantile(train_test_prod$mths_since_last_record, na.rm = T, 0.99)
train_test_prod$mths_since_last_record[train_test_prod$mths_since_last_record > quan] = quan

#open_acc
quan = quantile(train_test_prod$open_acc, na.rm = T, 0.99)
train_test_prod$open_acc[train_test_prod$open_acc > quan] = quan

#pub_rec
quan = quantile(train_test_prod$pub_rec, na.rm = T, 0.99)
train_test_prod$pub_rec[train_test_prod$pub_rec > quan] = quan

#revol_bal
quan = quantile(train_test_prod$revol_bal, na.rm = T, 0.99)
train_test_prod$revol_bal[train_test_prod$revol_bal > quan] = quan

#revol_util
quan = quantile(train_test_prod$revol_util, na.rm = T, 0.99)
train_test_prod$revol_util[train_test_prod$revol_util > quan] = quan

#total_acc
quan = quantile(train_test_prod$total_acc, na.rm = T, 0.99)
train_test_prod$total_acc[train_test_prod$total_acc > quan] = quan

#total_rec_int
quan = quantile(train_test_prod$total_rec_int, na.rm = T, 0.99)
train_test_prod$total_rec_int[train_test_prod$total_rec_int > quan] = quan

#total_rec_late_fee
quan = quantile(train_test_prod$total_rec_late_fee, na.rm = T, 0.99)
train_test_prod$total_rec_late_fee[train_test_prod$total_rec_late_fee > quan] = quan

#recoveries
quan = quantile(train_test_prod$recoveries, na.rm = T, 0.99)
train_test_prod$recoveries[train_test_prod$recoveries > quan] = quan

#collection_recovery_fee
quan = quantile(train_test_prod$collection_recovery_fee, na.rm = T, 0.99)
train_test_prod$collection_recovery_fee[train_test_prod$collection_recovery_fee > quan] = quan

#collections_12_mths_ex_med
quan = quantile(train_test_prod$collections_12_mths_ex_med, na.rm = T, 0.99)
train_test_prod$collections_12_mths_ex_med[train_test_prod$collections_12_mths_ex_med > quan] = quan

#mths_since_last_major_derog
quan = quantile(train_test_prod$mths_since_last_major_derog, na.rm = T, 0.99)
train_test_prod$mths_since_last_major_derog[train_test_prod$mths_since_last_major_derog > quan] = quan

#acc_now_delinq
quan = quantile(train_test_prod$acc_now_delinq, na.rm = T, 0.99)
train_test_prod$acc_now_delinq[train_test_prod$acc_now_delinq > quan] = quan

#tot_coll_amt
quan = quantile(train_test_prod$tot_coll_amt, na.rm = T, 0.99)
train_test_prod$tot_coll_amt[train_test_prod$tot_coll_amt > quan] = quan

#tot_cur_bal
quan = quantile(train_test_prod$tot_cur_bal, na.rm = T, 0.99)
train_test_prod$tot_cur_bal[train_test_prod$tot_cur_bal > quan] = quan

#total_rev_hi_lim
quan = quantile(train_test_prod$total_rev_hi_lim, na.rm = T, 0.99)
train_test_prod$total_rev_hi_lim[train_test_prod$total_rev_hi_lim > quan] = quan
##################################################################################################################
#Replacing the NA's in numerical features
train_test_prod$annual_inc[is.na(train_test_prod$annual_inc)] = -999
train_test_prod$delinq_2yrs[is.na(train_test_prod$delinq_2yrs)] = -999
train_test_prod$inq_last_6mths[is.na(train_test_prod$inq_last_6mths)] = -999
train_test_prod$open_acc[is.na(train_test_prod$open_acc)] = -999
train_test_prod$pub_rec[is.na(train_test_prod$pub_rec)] = -999
train_test_prod$total_acc[is.na(train_test_prod$total_acc)] = -999
train_test_prod$acc_now_delinq[is.na(train_test_prod$acc_now_delinq)] = -999
train_test_prod$tot_coll_amt[is.na(train_test_prod$tot_coll_amt)] = -999
train_test_prod$tot_cur_bal[is.na(train_test_prod$tot_cur_bal)] = -999
train_test_prod$total_rev_hi_lim[is.na(train_test_prod$total_rev_hi_lim)] = -999
train_test_prod$collections_12_mths_ex_med[is.na(train_test_prod$collections_12_mths_ex_med)] = -999
train_test_prod$revol_util[is.na(train_test_prod$revol_util)] = -999
train_test_prod$mths_since_last_delinq[is.na(train_test_prod$mths_since_last_delinq)] = -999
train_test_prod$mths_since_last_record[is.na(train_test_prod$mths_since_last_record)] = -999
train_test_prod$mths_since_last_major_derog[is.na(train_test_prod$mths_since_last_major_derog)] = -999

#Filling up the NA's in categorical features
train_test_prod$verification_status_joint = as.character(train_test_prod$verification_status_joint)
train_test_prod$verification_status_joint[train_test_prod$verification_status_joint==""] = 'not joint account'
train_test_prod$verification_status_joint = as.factor(train_test_prod$verification_status_joint)
train_test_prod$emp_title = as.character(train_test_prod$emp_title)
train_test_prod$emp_title[train_test_prod$emp_title==""] = 'Other'
train_test_prod$emp_title = as.factor(train_test_prod$emp_title)
train_test_prod$desc = as.character(train_test_prod$desc)
train_test_prod$desc[train_test_prod$desc==""] = 'Other'
train_test_prod$desc = as.factor(train_test_prod$desc)
train_test_prod$emp_length = as.character(train_test_prod$emp_length)
train_test_prod$emp_length[train_test_prod$emp_length == 'n/a'] = '10+ years'
train_test_prod$emp_length = as.factor(train_test_prod$emp_length)
str(train_test_prod)
summary(train_test_prod)

#############################################################################################
#Feature engineering
Find_length = function(x){nchar(x)}
train_test_prod$emp_title_length = Find_length(as.character(train_test_prod$emp_title))
train_test_prod$desc_length = Find_length(as.character(train_test_prod$desc))
train_test_prod$title_length = Find_length(as.character(train_test_prod$title))

#Extarct the numerics from  emp_length, zip_code, term, last_week_pay 
train_test_prod$emp_length = as.numeric(gsub("<|\\+| |year|s",'',x=(train_test_prod$emp_length)))
train_test_prod$zip_code = as.numeric(gsub("xx",'',x=(train_test_prod$zip_code)))
train_test_prod$term = as.numeric(substr(train_test_prod$term, start=1, stop=2))
train_test_prod$last_week_pay = as.numeric(gsub("th week",'',x = train_test_prod$last_week_pay))
train_test_prod$last_week_pay[is.na(train_test_prod$last_week_pay)] = 44
str(train_test_prod)

#############################################################################################
#Dropping column
train_test_prod$batch_enrolled = NULL

#Convert these factors to numerics instead of OHE
train_test_prod$emp_title = as.numeric(train_test_prod$emp_title)
train_test_prod$desc = as.numeric(train_test_prod$desc)
train_test_prod$title = as.numeric(train_test_prod$title)
train_test_prod$addr_state = as.numeric(train_test_prod$addr_state)

str(train_test_prod)
summary(train_test_prod)

#############################################################################################
#Splitting back into train prod and test prod
train_prod = train_test_prod[train_test_prod$type=='train',]
test_prod = train_test_prod[train_test_prod$type=='test',]
train_prod$type = NULL
test_prod$type = NULL

#############################################################################################
#H2o
library(h2o)
start = h2o.init(nthreads = -1)
train_prod_h2o = as.h2o(train_prod)
test_prod_h2o = as.h2o(test_prod)
x_indep = colnames(train_prod_h2o[,!colnames(train_prod_h2o) %in% c('loan_status')])
y_dep = 'loan_status'

#Gradient boosting
GBM.h2o.model = h2o.gbm(x =x_indep, y= y_dep, ntrees = 250, 
                        max_depth = 10,
                        seed = 100, 
                        min_rows = 10, 
                        learn_rate = 0.05,
                        col_sample_rate = 0.8,
                        col_sample_rate_per_tree = 0.6,
                        sample_rate = 1,
                        training_frame = train_prod_h2o)
#On prod
GBM.h2o.pred.prod = predict(GBM.h2o.model, newdata=test_prod_h2o)
GBM.h2o.pred.prod = as.data.frame(GBM.h2o.pred.prod)

###############################################################################################
#Xgboost
library(xgboost)

drop = c('loan_status')
train_prod$term = as.numeric(substr(train_prod$term, start=1, stop=2))
test_prod$term = as.numeric(substr(test_prod$term, start=1, stop=2))

train_prod$grade = as.numeric(train_prod$grade)
test_prod$grade = as.numeric(test_prod$grade)

train_prod$sub_grade = as.numeric(train_prod$sub_grade)
test_prod$sub_grade = as.numeric(test_prod$sub_grade)

train_prod$home_ownership = as.numeric(train_prod$home_ownership)
test_prod$home_ownership = as.numeric(test_prod$home_ownership)

train_prod$verification_status = as.numeric(train_prod$verification_status)
test_prod$verification_status = as.numeric(test_prod$verification_status)

train_prod$pymnt_plan = as.numeric(train_prod$pymnt_plan)
test_prod$pymnt_plan = as.numeric(test_prod$pymnt_plan)

train_prod$purpose = as.numeric(train_prod$purpose)
test_prod$purpose = as.numeric(test_prod$purpose)

train_prod$initial_list_status = as.numeric(train_prod$initial_list_status)
test_prod$initial_list_status = as.numeric(test_prod$initial_list_status)

train_prod$application_type = as.numeric(train_prod$application_type)
test_prod$application_type = as.numeric(test_prod$application_type)

train_prod$verification_status_joint = as.numeric(train_prod$verification_status_joint)
test_prod$verification_status_joint = as.numeric(test_prod$verification_status_joint)

train_xgb_prod_indep = train_prod[,!colnames(train_prod) %in% drop]
train_xgb_prod_dep = as.numeric(train_prod[,'loan_status'])-1
test_xgb_prod_indep = test_prod[,!colnames(test_prod) %in% c(drop)]

train_xgb_prod_indep = train_prod[,!colnames(train_prod) %in% drop]
train_xgb_prod_dep = as.numeric(train_prod[,'loan_status'])-1
train_xgb_local_indep = train_local[,!colnames(train_local) %in% drop]
train_xgb_local_dep = as.numeric(train_local[,'loan_status'])-1
test_xgb_local_indep = test_local[,!colnames(test_local) %in% drop]
test_xgb_local_dep = as.numeric(test_local[,'loan_status'])-1
test_xgb_prod_indep = test_prod[,!colnames(test_prod) %in% c(drop)]

dtrain_prod = xgb.DMatrix(data = as.matrix(train_xgb_prod_indep), label = train_xgb_prod_dep)
dtest_local = xgb.DMatrix(data = as.matrix(test_xgb_local_indep), label = test_xgb_local_dep)
dtest_prod = xgb.DMatrix(data = as.matrix(test_xgb_prod_indep))

watchlist = list(test = dtest_local, train = dtrain_prod)

set.seed(100)
xgb.local.model =xgb.train(data = dtrain_prod,
                           watchlist = watchlist,
                           objective = "binary:logistic",
                           nrounds = 185,
                           max_depth = 6,
                           eta=0.15,
                           subsample = 1,
                           colsample_bytree = 0.6,
                           early.stop.round = 10,
                           maximize=T,
                           eval_metric = 'auc')

xgb.prod.pred = predict(xgb.local.model, newdata = dtest_prod)

xgb.importance(model = xgb.local.model, feature_names = colnames(train_xgb_local_indep))

############################################################################################

#Ensemble
#prod submission
avg = ((xgb.prod.pred*4)+(GBM.h2o.pred.prod$p1*6))/10
ensemble_sub = data.frame(member_id=test_prod$member_id,	loan_status=avg)
write.csv(ensemble_sub, row.names=F, 'sub_ensemble_2.csv')

###############################################################################################
