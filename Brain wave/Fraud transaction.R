
library(data.table)
library(MLmetrics)
library(randomForest)
library(dplyr)

#Reading the files
train_prod = fread('train.csv')
test_prod = fread('test.csv')

#Combining the train and test
test_prod$target = NA
train_test_prod = rbind(train_prod, test_prod)

#Dropping of the constant columns
train_test_prod[ , ':=' (cat_var_38 = NULL,
                         cat_var_42 = NULL)]

#Filling up empty factor columns and converting into factors
fac_columns = colnames(train_test_prod)[unlist(lapply(train_test_prod, FUN = function(x){ class(x)})) == 'character']
fac_columns = setdiff(fac_columns, 'transaction_id')
train_test_prod[, (fac_columns) := lapply(fac_columns, FUN = function(x) { x<- get(x)
                                                                           x[x ==''] <- "Other"
                                                                           x})]
train_test_prod[, (fac_columns) := lapply(.SD, as.factor), .SDcols=fac_columns]
train_test_prod[, (fac_columns) := lapply(.SD, as.numeric), .SDcols=fac_columns]

#Feature engineering
train_test_prod[, num_7_mul_2 := num_var_7 * num_var_2]
train_test_prod[, num_1_mul_2 := num_var_1 * num_var_2]


#Convert target column to factor
train_test_prod$target = as.factor(train_test_prod$target)

#Clustering
set.seed(100)
traindata=scale(train_test_prod[, -c('transaction_id', 'target')],center = T,scale=T)
K_means = kmeans(x = traindata, centers = 5, trace = T, iter.max = 10000)

myDist<-function(p1,p2) {sqrt((p1[,1]-p2[,1])^2 + (p1[,2]-p2[,2])^2 + (p1[,3]-p2[,3])^2 + (p1[,4]-p2[,4])^2 + (p1[,5]-p2[,5])^2 + (p1[,6]-p2[,6])^2 + (p1[,7]-p2[,7])^2 + (p1[,8]-p2[,8])^2 + (p1[,9]-p2[,9])^2 + (p1[,10]-p2[,10])^2 + 
                                (p1[,11]-p2[,11])^2 + (p1[,12]-p2[,12])^2 + (p1[,13]-p2[,13])^2 + (p1[,14]-p2[,14])^2 + (p1[,15]-p2[,15])^2 + (p1[,16]-p2[,16])^2 + (p1[,17]-p2[,17])^2 + (p1[,18]-p2[,18])^2 + (p1[,19]-p2[,19])^2 + (p1[,20]-p2[,20])^2 + 
                                (p1[,21]-p2[,21])^2 + (p1[,22]-p2[,22])^2 + (p1[,23]-p2[,23])^2 + (p1[,24]-p2[,24])^2 + (p1[,25]-p2[,25])^2 + (p1[,26]-p2[,26])^2 + (p1[,27]-p2[,27])^2 + (p1[,28]-p2[,28])^2 + (p1[,29]-p2[,29])^2 + (p1[,30]-p2[,30])^2 + 
                                (p1[,31]-p2[,31])^2 + (p1[,32]-p2[,32])^2 + (p1[,33]-p2[,33])^2 + (p1[,34]-p2[,34])^2 + (p1[,35]-p2[,35])^2 + (p1[,36]-p2[,36])^2 + (p1[,37]-p2[,37])^2 + (p1[,38]-p2[,38])^2 + (p1[,39]-p2[,39])^2 + (p1[,40]-p2[,40])^2 + 
                                (p1[,41]-p2[,41])^2 + (p1[,42]-p2[,42])^2 + (p1[,43]-p2[,43])^2 + (p1[,44]-p2[,44])^2 + (p1[,45]-p2[,45])^2 + (p1[,46]-p2[,46])^2 + (p1[,47]-p2[,47])^2)}

train_test_prod$clus = NA
train_test_prod$clus[K_means$cluster==1] = myDist(traindata[K_means$cluster==1,], K_means$centers[1,,drop=FALSE])
train_test_prod$clus[K_means$cluster==2] = myDist(traindata[K_means$cluster==2,], K_means$centers[2,,drop=FALSE])
train_test_prod$clus[K_means$cluster==3] = myDist(traindata[K_means$cluster==3,], K_means$centers[3,,drop=FALSE])
train_test_prod$clus[K_means$cluster==4] = myDist(traindata[K_means$cluster==4,], K_means$centers[4,,drop=FALSE])
train_test_prod$clus[K_means$cluster==5] = myDist(traindata[K_means$cluster==5,], K_means$centers[5,,drop=FALSE])

#train and test prod seperation
train_prod = train_test_prod[!is.na(target),]
test_prod  = train_test_prod[is.na(target),]

#Mean label encoding the first 18 categorical features
for (i in 1:18)
{
  print(i)
  new_cat_var <- (paste0('cat_var_',i,'_enc'))
  old_cat_var <- (paste0('cat_var_',i))
  train_prod[, new_cat_var := mean(target==1), by = old_cat_var, with = F]
  test_prod <- merge(test_prod, unique(train_prod[, c(old_cat_var, new_cat_var), with = F]),
                     by.x = old_cat_var, by.y = old_cat_var, all.x = T)
  train_prod[, old_cat_var := NULL, with = F]
  test_prod[, old_cat_var := NULL, with = F]
}

#Sorting the test_prod
#Extracting the submission id and converting the id to numeric
sub_id = test_prod$transaction_id
train_prod[, transaction_id := as.numeric(gsub(transaction_id, pattern = "id_", replacement = ""))]
test_prod[, transaction_id := as.numeric(gsub(transaction_id, pattern = "id_", replacement = ""))]
head(train_prod$transaction_id)


#Train and test local
set.seed(100)
sample_split = sample(nrow(train_prod), 0.6*nrow(train_prod), replace  = F)
train_local = train_prod[sample_split,]
test_local = train_prod[-sample_split,]
str(train_local)

##############################################################################
library(h2o)
h2o.init(nthreads = -1, min_mem_size = '20g')

train_prod_h2o = as.h2o(train_prod)
test_prod_h2o = as.h2o(test_prod)
train_local_h2o = as.h2o(train_local)
test_local_h2o = as.h2o(test_local)

x_indep = setdiff(colnames(train_prod_h2o), c('target', 'num_7_mul_2', 'num_1_mul_2'))
y_dep =  'target'

#RF_3
RF.h2o.model.local = h2o.randomForest(x = x_indep, y = y_dep, ntrees = 500,
                                      stopping_metric = 'AUC',
                                      training_frame = train_local_h2o,
                                      seed = 100)

#On prod
RF.h2o.pred.prod = predict(RF.h2o.model.local, newdata = test_prod_h2o)
RF.h2o.pred.prod = as.data.frame(RF.h2o.pred.prod)
pred_1 = RF.h2o.pred.prod$p1

#RF_4
RF.h2o.model.local = h2o.randomForest(x = x_indep, y = y_dep, ntrees = 500,
                                      stopping_metric = 'AUC',
                                      training_frame = train_prod_h2o,
                                      seed = 100)

#On prod
RF.h2o.pred.prod = predict(RF.h2o.model.local, newdata = test_prod_h2o)
RF.h2o.pred.prod = as.data.frame(RF.h2o.pred.prod)
pred_2 = RF.h2o.pred.prod$p1

#RF_7
x_indep = setdiff(colnames(train_prod_h2o), c('target', 'num_1_mul_2'))
y_dep =  'target'
RF.h2o.model.local = h2o.randomForest(x = x_indep, y = y_dep, ntrees = 500,
                                      stopping_metric = 'AUC',
                                      training_frame = train_prod_h2o,
                                      seed = 100)

#On prod
RF.h2o.pred.prod = predict(RF.h2o.model.local, newdata = test_prod_h2o)
RF.h2o.pred.prod = as.data.frame(RF.h2o.pred.prod)
pred_3 = RF.h2o.pred.prod$p1

x_indep = setdiff(colnames(train_prod_h2o), c('target', 'num_7_mul_2', 'num_1_mul_2'))
y_dep =  'target'
#GBM_5
GBM.h2o.model.local = h2o.gbm(x = x_indep, y = y_dep,
                              ntrees = 75, max_depth = 15,
                              training_frame = train_local_h2o,
                              seed = 100)

#On prod
GBM.h2o.pred.prod = predict(GBM.h2o.model.local, newdata = test_prod_h2o)
GBM.h2o.pred.prod = as.data.frame(GBM.h2o.pred.prod)
pred_4 = GBM.h2o.pred.prod$p1

#GBM_9
GBM.h2o.model.local = h2o.gbm(x = x_indep, y = y_dep,
                              ntrees = 100, max_depth = 15,
                              training_frame = train_prod_h2o,
                              seed = 100)

#On prod
GBM.h2o.pred.prod = predict(GBM.h2o.model.local, newdata = test_prod_h2o)
GBM.h2o.pred.prod = as.data.frame(GBM.h2o.pred.prod)
pred_5 = GBM.h2o.pred.prod$p1

#GBM_10
GBM.h2o.model.local = h2o.gbm(x = x_indep, y = y_dep,
                              ntrees = 100, max_depth = 15,
                              learn_rate = 0.05,
                              training_frame = train_prod_h2o,
                              seed = 100)

#On prod
GBM.h2o.pred.prod = predict(GBM.h2o.model.local, newdata = test_prod_h2o)
GBM.h2o.pred.prod = as.data.frame(GBM.h2o.pred.prod)
pred_6 = GBM.h2o.pred.prod$p1

#GBM_12
GBM.h2o.model.local = h2o.gbm(x = x_indep, y = y_dep,
                              ntrees = 125, max_depth = 15,
                              learn_rate = 0.05,
                              col_sample_rate = 0.9, sample_rate = 0.9,
                              training_frame = train_prod_h2o,
                              seed = 100)

#On prod
GBM.h2o.pred.prod = predict(GBM.h2o.model.local, newdata = test_prod_h2o)
GBM.h2o.pred.prod = as.data.frame(GBM.h2o.pred.prod)
pred_7 = GBM.h2o.pred.prod$p1

final_pred = (pred_1 +pred_2 +pred_3 +pred_4 +pred_5 +pred_6 + pred_7)/7

sub_GBM_h2o = data.frame(transaction_id = sub_id, target = final_pred)
head(sub_GBM_h2o)
write.csv(sub_GBM_h2o, row.names = F, 'sub_ens.csv')

