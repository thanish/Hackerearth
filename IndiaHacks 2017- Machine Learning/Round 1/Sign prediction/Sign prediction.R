
library(MLmetrics)
library(xgboost)

train_prod = read.csv('train.csv')
test_prod = read.csv('test.csv')
colnames(train_prod)[7] = 'SignFacing_Target'
train_prod$Id = NULL

set.seed(100)
split = sample(nrow(train_prod), 0.6*nrow(train_prod), replace=F)
train_local = train_prod[split,]
test_local = train_prod[-split,]

#Creating dummies for the factor
library(dummies)
indep = setdiff(colnames(train_local),c('SignFacing_Target'))
dep = 'SignFacing_Target'

train_local = cbind(dummy.data.frame(train_local[,indep]),SignFacing_Target = train_local[,dep])
test_local = cbind(dummy.data.frame(test_local[,indep]),SignFacing_Target = test_local[,dep])
train_prod = cbind(dummy.data.frame(train_prod[,indep]),SignFacing_Target = train_prod[,dep])
test_prod = cbind(Id = test_prod[,'Id'] ,dummy.data.frame(test_prod[,indep]))

train_local$SignFacing_Target = as.numeric(train_local$SignFacing_Target) - 1
test_local$SignFacing_Target = as.numeric(test_local$SignFacing_Target) - 1
train_prod$SignFacing_Target = as.numeric(train_prod$SignFacing_Target) - 1

indep = setdiff(colnames(train_local),c('SignWidth', 'SignFacing_Target'))
dep = 'SignFacing_Target'

dtrain_local = xgb.DMatrix(data = as.matrix(train_local[,indep]), label = train_local[,dep])
dtest_local = xgb.DMatrix(data = as.matrix(test_local[,indep]), label = test_local[,dep])
dtrain_prod = xgb.DMatrix(data = as.matrix(train_prod[,indep]), label = train_prod[,dep])
dtest_prod = xgb.DMatrix(data = as.matrix(test_prod[,indep]))
watchlist = list(test = dtest_local, train = dtrain_local)

set.seed(75)
xgb.local.model = xgb.train(data = dtrain_prod,
                            watchlist = watchlist,
                            nround = 600, 
                            max_depth = 3, 
                            eta = 0.04,
                            subsample = 0.7,
                            colsample_bytree =0.9,
                            objective = 'multi:softprob',
                            eval_metric = 'mlogloss',
                            num_class = 4,
                            early.stop.round = 10,
                            maximize = F
)

#ON prod
xgb.prod.pred = predict(xgb.local.model, newdata = dtest_prod)
pred_prod = matrix(xgb.prod.pred, ncol = length(unique(test_local$SignFacing_Target)), 
                   nrow = length(xgb.prod.pred)/length(unique(test_local$SignFacing_Target)),
                   byrow = T)
colnames(pred_prod) = c("Front", "Left",  "Rear",  "Right")

sub_XGB = data.frame(Id = test_prod$Id, pred_prod)

write.csv(sub_XGB, row.names=F, 'sub_XGB_15.csv')


