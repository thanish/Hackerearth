#Desktop
setwd('I:\\Data science\\Data science competition\\Hackers earth\\Einsite')
library(MLmetrics)

#Reading the files
train = read.csv('train.csv')
test = read.csv('test.csv')

str(train)
str(test)

#Combining the train and test
test$fare_amount = NA
train$type = 'train'
test$type = 'test'
train_test_prod = rbind(train, test)
train = NULL
test = NULL
str(train_test_prod)

#Initial Feature engineering
#Extracting the pickup and dropoff year, month, day, hour, min, secs from the pickup and dropoff datetime feature
train_test_prod$pickup_date = as.Date(gsub(' .*','',as.character(train_test_prod$pickup_datetime)),'%Y-%m-%d')
train_test_prod$pickup_time = gsub('.* ','',as.character(train_test_prod$pickup_datetime))
train_test_prod$dropoff_date = as.Date(gsub(' .*','',as.character(train_test_prod$dropoff_datetime)),'%Y-%m-%d')
train_test_prod$dropoff_time = gsub('.* ','',as.character(train_test_prod$dropoff_datetime))
train_test_prod$pickup_year = as.numeric(format(train_test_prod$pickup_date,'%Y'))
train_test_prod$pickup_month = as.numeric(format(train_test_prod$pickup_date,'%m'))
train_test_prod$pickup_day = as.numeric(format(train_test_prod$pickup_date,'%d'))
train_test_prod$dropoff_year = as.numeric(format(train_test_prod$dropoff_date,'%Y'))
train_test_prod$dropoff_month = as.numeric(format(train_test_prod$dropoff_date,'%m'))
train_test_prod$dropoff_day = as.numeric(format(train_test_prod$dropoff_date,'%d'))
train_test_prod$pickup_hour = as.numeric(substr(train_test_prod$pickup_time, start = 1, stop = 2))
train_test_prod$pickup_min = as.numeric(substr(train_test_prod$pickup_time, start = 4, stop = 5))
train_test_prod$pickup_sec = as.numeric(substr(train_test_prod$pickup_time, start = 7, stop = 8))
train_test_prod$dropoff_hour = as.numeric(substr(train_test_prod$dropoff_time, start = 1, stop = 2))
train_test_prod$dropoff_min = as.numeric(substr(train_test_prod$dropoff_time, start = 4, stop = 5))
train_test_prod$dropoff_sec = as.numeric(substr(train_test_prod$dropoff_time, start = 7, stop = 8))

#Calculate the total trip duration using the pickup and dropp off time feature. 
train_test_prod$pick_drop_timediff = as.numeric(difftime(strptime(train_test_prod$dropoff_datetime, '%Y-%m-%d %H:%M:%OS'), 
                                                         strptime(train_test_prod$pickup_datetime, '%Y-%m-%d %H:%M:%OS'), 
                                                         units = 'mins'))

#Dropping off few features after the desired info is extracted.
train_test_prod$pickup_datetime = NULL
train_test_prod$dropoff_datetime = NULL
train_test_prod$pickup_date = NULL
train_test_prod$dropoff_date = NULL
train_test_prod$pickup_time = NULL
train_test_prod$dropoff_time = NULL

str(train_test_prod)
summary(train_test_prod)

#Filling the NA
train_test_prod[is.na(train_test_prod)] = -999

#Correcting the factor which are empty/NA and converting the rate code to numeric
train_test_prod$store_and_fwd_flag = as.character(train_test_prod$store_and_fwd_flag)
train_test_prod$store_and_fwd_flag[!train_test_prod$store_and_fwd_flag %in% c('Y','N')] = 'notsure'
train_test_prod$store_and_fwd_flag = as.factor(train_test_prod$store_and_fwd_flag)
train_test_prod$new_user = as.character(train_test_prod$new_user)
train_test_prod$new_user[!train_test_prod$new_user %in% c('YES','NO')] = 'notsure'
train_test_prod$new_user = as.factor(train_test_prod$new_user)
train_test_prod$rate_code = as.numeric(as.factor(train_test_prod$rate_code))

# Calculate distance in kilometers between two points using the pickup, dropoff lat and long
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

train_test_prod$trip_dist_km = earth.dist(train_test_prod$pickup_longitude,
                                          train_test_prod$pickup_latitude,
                                          train_test_prod$dropoff_longitude,
                                          train_test_prod$dropoff_latitude)
str(train_test_prod)
summary(train_test_prod)

#Calculating the time it takes per km 
train_test_prod$time_per_km = train_test_prod$pick_drop_timediff/train_test_prod$trip_dist_km
train_test_prod$time_per_km[train_test_prod$time_per_km %in% c('Inf','-Inf')] = 0
train_test_prod$time_per_km[is.na(train_test_prod$time_per_km)] = 0

#############################################################################
#Extracting the zip code
train_test_prod$pickup_latitude = as.character(train_test_prod$pickup_latitude)
train_test_prod$pickup_latitude = ifelse(substr(train_test_prod$pickup_latitude, 1,1)=='-',
                                         substr(train_test_prod$pickup_latitude, 1,6),
                                         substr(train_test_prod$pickup_latitude, 1,5))
train_test_prod$pickup_latitude = as.numeric(train_test_prod$pickup_latitude)

train_test_prod$pickup_longitude = as.character(train_test_prod$pickup_longitude)
train_test_prod$pickup_longitude = ifelse(substr(train_test_prod$pickup_longitude, 1,1)=='-',
                                          substr(train_test_prod$pickup_longitude, 1,6),
                                          substr(train_test_prod$pickup_longitude, 1,5))
train_test_prod$pickup_longitude = as.numeric(train_test_prod$pickup_longitude)

library(zipcode)
data("zipcode")
zipcode$latitude = as.character(zipcode$latitude)
zipcode$latitude = ifelse(substr(zipcode$latitude, 1,1)=='-',
                          substr(zipcode$latitude, 1,6),
                          substr(zipcode$latitude, 1,5))
zipcode$latitude = as.numeric(zipcode$latitude)

zipcode$longitude = as.character(zipcode$longitude)
zipcode$longitude = ifelse(substr(zipcode$longitude, 1,1)=='-',
                           substr(zipcode$longitude, 1,6),
                           substr(zipcode$longitude, 1,5))
zipcode$longitude = as.numeric(zipcode$longitude)

zipcode = zipcode[!duplicated(zipcode[,c('longitude','latitude')]),]

train_test_prod = merge(train_test_prod, zipcode[,c('latitude','longitude','zip')], 
                        by.x = c('pickup_latitude','pickup_longitude'),
                        by.y = c('latitude','longitude'),
                        all.x = T)
train_test_prod$zip = as.numeric(train_test_prod$zip)
train_test_prod$zip[is.na(train_test_prod$zip)] = -999

#############################################################################
#Splitting back to train and test prod
train_prod = train_test_prod[train_test_prod$type=='train',]
test_prod = train_test_prod[train_test_prod$type=='test',]
train_prod$type = NULL
test_prod$type = NULL
train_prod$TID = NULL
test_prod$fare_amount = NULL

####################################################################################
#Xgboosting
library(xgboost)

fac_column_train = colnames(train_prod)[sapply(train_prod, class)=='factor']
fac_column_test = colnames(test_prod)[sapply(test_prod, class)=='factor']
fac_column_test = setdiff(fac_column_test, 'TID')

train_prod[fac_column_train] = sapply(train_prod[fac_column_train], as.numeric)
test_prod[fac_column_test] = sapply(test_prod[fac_column_test], as.numeric)
str(train_prod)
str(test_prod)

set.seed(100)
sample_split = sample(nrow(train_prod), 0.6*nrow(train_prod), replace=F)
train_local = train_prod[sample_split, ]
test_local = train_prod[-sample_split, ]

indep = setdiff(colnames(train_local), c('fare_amount'))
dep = 'fare_amount'

dtrain_prod = xgb.DMatrix(data = as.matrix(train_prod[,indep]), label = train_prod[,dep])
dtrain_local = xgb.DMatrix(data = as.matrix(train_local[,indep]), label = train_local[,dep])
dtest_local = xgb.DMatrix(data = as.matrix(test_local[,indep]), label = test_local[,dep])
dtest_prod = xgb.DMatrix(data = as.matrix(test_prod[,indep]))

watchlist = list(test = dtest_local, train = dtrain_local)

mae = function (preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  MAE <- mean(abs(labels - preds))
  return(list(metric = "MAE", value = MAE))
}

set.seed(100)
xgb.model.local = xgb.train(data = dtrain_local,
                            watchlist= watchlist,
                            nround = 2000, 
                            max_depth = 9, 
                            eta = 0.1,
                            colsample_bytree = 0.9,
                            subsample = 0.9,
                            objective = 'reg:linear', 
                            eval_metric = mae,
                            early.stop.round = 10,
                            maximize = F)

xgb.pred.prod.1 = predict(xgb.model.local, newdata = dtest_prod)
xgb.pred.prod.1[xgb.pred.prod.1<0] = 0

set.seed(100)
xgb.model.prod = xgboost(data = dtrain_prod,
                         nround = 1339, 
                         max_depth = 9, 
                         eta = 0.1,
                         colsample_bytree = 0.9,
                         subsample = 0.9,
                         objective = 'reg:linear', 
                         eval_metric = mae)

xgb.pred.prod.2 = predict(xgb.model.prod, newdata = dtest_prod)
xgb.pred.prod.2[xgb.pred.prod.2<0] = 0

xgb_mean = ((xgb.pred.prod.1*4) + (xgb.pred.prod.2*6))/10

sub_XGB = data.frame(TID = test_prod$TID, fare_amount = xgb_mean)
write.csv(sub_XGB, row.names=F, 'sub_XGB_10.csv')
