setwd('E:\\Thanish\\Data science\\Hackerearth\\IndiaHacks 2017\\Round 2 - Lane deduction')

library(data.table)
library(randomForest)
library(MLmetrics)
library(stringr)
library(dplyr)

#Reading the data
train_prod = read.csv('train.csv')
label = read.csv('labels.csv')
test_prod = read.csv('test.csv')

train_prod = as.data.table(train_prod)
label = as.data.table(label)
test_prod = as.data.table(test_prod)

#Combining the train_prod with the label 
train_prod = merge(train_prod, label, 
                   by.x = 'roadId', by.y = 'roadId',
                   all.x = T)

#Concatinating the train_prod and test_prod
train_prod[, type:= 'train']
test_prod[, ':=' (roadCoordinates = NA, 
                  noOfLanes = NA,
                  type = 'test')]
train_test_prod = rbind(train_prod, test_prod)

#Replacing the NA with -999
train_test_prod[is.na(distFromLaneLineOnLeft) , distFromLaneLineOnLeft := -999]
train_test_prod[is.na(distFromLaneLineOnRight), distFromLaneLineOnRight := -999]

#Converting the isIntersectingWithRoadGeometry to 1 and 0
train_test_prod[, isIntersectingWithRoadGeometry := as.numeric(as.factor(isIntersectingWithRoadGeometry))-1]

################################
#Feature engineer
#Adding the NA term
train_test_prod[, ':=' (is_left_lane_na = ifelse(distFromLaneLineOnLeft==-999, 1, 0),
                        is_right_lane_na = ifelse(distFromLaneLineOnRight==-999, 1, 0))]

#Adding the number of Lane line co-ordinates in a row
train_test_prod[,count_lane_co_or := as.numeric(str_count(as.character(train_test_prod$laneLineCoordinates),"\\|"))]

#Adding the length of the co-ordinates
train_test_prod[,co_or_len := as.numeric(nchar(as.character(train_test_prod$laneLineCoordinates)))]

#Avg Lane length 
train_test_prod[, avg_lane_len := roadLength/totalLaneLines]

#Adding the % of lane length of each road
train_test_prod[,lane_len_per_road_len := laneLineLength/roadLength,]

#Converting LanelineID & laneLineCoordinates to numeric
train_test_prod[, laneLineId := as.numeric(laneLineId)]
train_test_prod[, laneLineCoordinates := as.numeric(laneLineCoordinates)]


str(train_test_prod)
################################
#Drop of the roadCoordinates
train_test_prod[,':=' (roadCoordinates = NULL)]

#Converting the target to factors
train_test_prod[, noOfLanes := as.factor(noOfLanes)]

#Splitting back to train and test prod
train_prod = train_test_prod[type=='train',]
test_prod = train_test_prod[type=='test',]

sub_test_ID = test_prod$roadId
train_prod[,':='(type = NULL)]
test_prod[,':='(type = NULL,
                noOfLanes = NULL)]

#Splitting into local train and test
uniq_road_id = unique(train_prod$roadId)
set.seed(100)
train_road_id = sample(uniq_road_id, 0.6*length(uniq_road_id), replace = F)
train_local = train_prod[roadId %in% train_road_id,]
test_local = train_prod[!roadId %in% train_road_id,]

#Function to find the mode
mode_fun = function(x)
{
  temp <- table(as.vector(x))
  ifelse(length(names(temp)[temp == max(temp)])==1,
         names(temp)[temp == max(temp)],names(temp)[temp == max(temp)][1])
}

###################################################################################
#############################  1st Model : 1 #####################################
###################################################################################
#Random forest
set.seed(75)
RF.model.local = randomForest(noOfLanes~., do.trace= 50, 
                              ntree=195, nodesize =16, 
                              mtry = 5 ,
                              data = train_prod)

#Pred on prod dataset
RF.pred.prod = predict(RF.model.local, newdata = test_prod)

RF_sub = data.frame(roadId= as.numeric(sub_test_ID),	noOfLanes =RF.pred.prod)
RF_sub_1 = as.data.frame(group_by(RF_sub, roadId) %>% summarize(noOfLanes = mode_fun(noOfLanes)))

###################################################################################
################################# 2nd Model : 2 ##################################
###################################################################################
#h2O
library(h2o)
h2o.init(nthreads = -1, max_mem_size = '5g')

train_local_h2o = as.h2o(train_local)
test_local_h2o = as.h2o(test_local)
train_prod_h2o = as.h2o(train_prod)
test_prod_h2o = as.h2o(test_prod)

x_indep = setdiff(colnames(train_local_h2o), c('noOfLanes'))
y_dep = 'noOfLanes'

#RandomForest
RF.model.h2o.local = h2o.randomForest(x= x_indep, y = y_dep, 
                                      ntrees = 210,  max_depth = 16,
                                      mtries = 5,
                                      col_sample_rate_per_tree = 0.7,
                                      training_frame = train_prod_h2o,
                                      seed = 75)

#On prod
RF.h2o.pred.prod = h2o.predict(RF.model.h2o.local, newdata = test_prod_h2o)
RF.h2o.pred.prod = as.data.frame(RF.h2o.pred.prod)

RF_h2o_sub = data.frame(roadId= as.numeric(sub_test_ID),	noOfLanes =RF.h2o.pred.prod$predict)
RF_h2o_sub_2 = as.data.frame(group_by(RF_h2o_sub, roadId) %>% summarize(noOfLanes = mode_fun(noOfLanes)))

###################################################################################
################################# 3rd Model : 3 ##################################
###################################################################################
#RandomForest
RF.model.h2o.local = h2o.randomForest(x= x_indep, y = y_dep, 
                                      ntrees = 210,  max_depth = 16,
                                      mtries = 5,
                                      col_sample_rate_per_tree = 0.7,
                                      training_frame = train_local_h2o,
                                      seed = 75)

#On prod
RF.h2o.pred.prod = h2o.predict(RF.model.h2o.local, newdata = test_prod_h2o)
RF.h2o.pred.prod = as.data.frame(RF.h2o.pred.prod)

RF_h2o_sub = data.frame(roadId= as.numeric(sub_test_ID),	noOfLanes =RF.h2o.pred.prod$predict)
RF_h2o_sub_3 = as.data.frame(group_by(RF_h2o_sub, roadId) %>% summarize(noOfLanes = mode_fun(noOfLanes)))

###################################################################################
##################################### Model ensembling ############################
###################################################################################

RF_ensemble = data.frame(RF_1 = RF_sub_1$noOfLanes, 
                         RF_2 = RF_sub_2$noOfLanes,
                         RF_3 = RF_sub_3$noOfLanes)

#Finding the ensemble mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

predicted = NULL
for (i in 1:nrow(RF_ensemble))
{
  v = unlist(RF_ensemble[i,])
  predicted[i] = getmode(v)  
}

RF_ensemble$predicted = predicted
nrow(RF_ensemble)

RF_sub_ensemble = data.frame(roadId= RF_sub_1$roadId,	noOfLanes =RF_ensemble$predicted)

write.csv(RF_sub_ensemble, row.names=F, 'sub_ensemble_RF_h2o_1_2_3.csv')
