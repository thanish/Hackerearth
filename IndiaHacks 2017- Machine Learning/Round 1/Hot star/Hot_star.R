library(data.table)
library(jsonlite)
library(purrr)
library(stringr)
library(ranger)
library(MLmetrics)

#Loading the datasets
train = fromJSON("train_data.json")
test = fromJSON("test_data.json")

#Converting the train and test json to dataframe format
train_data <- data.table(ID = unlist(names(train)))
train_data[, `:=` (genres = unlist(lapply(train, '[',1)),
                   titles = unlist(lapply(train, '[',2)),
                   cities = unlist(lapply(train, '[', 3)),
                   segment = unlist(lapply(train, '[',4)),
                   dow = unlist(lapply(train, '[',5)),
                   tod = unlist(lapply(train, '[', 6)))]

test_data <- data.table(ID  = unlist(names(test)))
test_data[,`:=` (genres = unlist(lapply(test, '[',1)),
                 titles = unlist(lapply(test, '[',2)),
                 tod = unlist(lapply(test, '[', 3)),
                 cities = unlist(lapply(test, '[',4)),
                 dow = unlist(lapply(test, '[',5)))]

### Encode target
train_data[,segment := ifelse(segment == 'neg',0,1)]
str(train_data)
str(test_data)

#Combining the train and test
test_data$segment = NA
train_test = rbind(train_data, test_data)
str(train_test)
head(train_test)

# Feature Engineering -----------------------------------------------------
## Creating new column per genres
train_test[,g1 := lapply(genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))]
train_test[,g1 := lapply(g1, unlist, use.names=F)]

uniq_genres <- unique(unlist(lapply(train_test$genres, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))))
length(uniq_genres)

toColumns <- function(data, variables){
  for(i in variables){
    data[,paste0(i,"_gen") := lapply(g1, function(x) any(match(i,x)))]
  }
  return (data) 
}

toColumns(train_test, uniq_genres)
train_test[,g1 := NULL]

## encode TRUE and NA
genx <- grep(pattern = "_gen", x = colnames(train_test), value = T)

for(k in genx)
  set(train_test, i = which(is.na(train_test[[k]])), j = k, value = 0)

for(k in genx)
  set(train_test, i = which(train_test[[k]] == TRUE), j= k ,value = 1)

## sum watch time from title
train_test[,t1 := lapply(titles, function(k) strsplit(x = k, split = ","))]
train_test[,t1 := lapply(t1, unlist, use.names = F)]
train_test[,t1 := lapply(t1, function(k) gsub(pattern = ".*\\:([0-9]+)",replacement = "\\1",x = k))]

train_test[,t1 := lapply(t1, function(x) paste(x,sep = " ", collapse = "+"))]
train_test[,title_sum := lapply(t1, function(x)eval(parse(text = x)))]
train_test[,title_sum := lapply(title_sum, function(x) ifelse(is_empty(x),0,x))] 
train_test[,t1 := NULL]

grep(pattern = '_gen', x = colnames(train_data), value = T)

## create count variables
train_test[,title_count := lapply(titles, function(x) str_count(string = x, pattern = ":"))]
train_test[,genres_count := lapply(genres, function(x) str_count(string = x, pattern = ":"))]
train_test[,cities_count := lapply(cities, function(x) str_count(string = x, pattern = ":"))]
train_test[,dow_count := lapply(dow, function(x) str_count(string = x, pattern = ":"))]
train_test[,tod_count := lapply(tod, function(x) str_count(string = x, pattern = ":"))]

## convert list to vectors - train
pd <- names(train_test)[sapply(train_test, is.list)]
train_test[, (pd) := lapply(.SD, unlist), .SDcols = pd]

# Creating new columns per city
train_test_prod[,c1 := lapply(cities, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))]
train_test_prod[,c1 := lapply(c1, unlist, use.names=F)]

a <- (unlist(lapply(train_test_prod$cities, function(k) str_extract_all(string = k, pattern = "[[:alpha:]]+"))))
uniq_city <- names(sort(table(a),decreasing = T)[1:20])

toColumns <- function(data, variables){
  for(i in variables){
    data[,paste0(i,"_city") := lapply(c1, function(x) any(match(i,x)))]
  }
  return (data)  
}

toColumns(train_test_prod, uniq_city)
train_test_prod[,c1 := NULL]

## encode TRUE and NA
genx <- grep(pattern = "_city", x = colnames(train_test_prod), value = T)

for(k in genx)
  set(train_test_prod, i = which(is.na(train_test_prod[[k]])), j = k, value = 0)

for(k in genx)
  set(train_test_prod, i = which(train_test_prod[[k]] == TRUE), j= k ,value = 1)

train_test_prod[,":="  (mumbai_city = unlist(mumbai_city),
                        navi_city = unlist(navi_city),
                        delhi_city = unlist(delhi_city),
                        bangalore_city = unlist(bangalore_city),
                        gurgaon_city = unlist(gurgaon_city),
                        chennai_city = unlist(chennai_city),
                        Unknown_city = unlist(Unknown_city),
                        kolkata_city = unlist(kolkata_city),
                        new_city = unlist(new_city),
                        pune_city = unlist(pune_city),
                        hyderabad_city = unlist(hyderabad_city),
                        bengaluru_city = unlist(bengaluru_city),
                        chandigarh_city = unlist(chandigarh_city),
                        ahmedabad_city = unlist(ahmedabad_city),
                        dhaka_city = unlist(dhaka_city),
                        karachi_city = unlist(karachi_city),
                        prabhadevi_city = unlist(prabhadevi_city),
                        secunderabad_city = unlist(secunderabad_city),
                        gandhinagar_city = unlist(gandhinagar_city),
                        rawalpindi_city = unlist(rawalpindi_city))]
str(train_test_prod)

#Lengths of different variables
train_test_prod[,':=' (genres_len = nchar(genres),
                       titles_len = nchar(titles),
                       cities_len = nchar(cities),
                       dow_len = nchar(dow),
                       tod_len = nchar(tod))]

## Creating new column per tod
train_test_prod[,c1 := lapply(tod, function(k) str_extract_all(string = k, pattern = "[[:digit:]]+:"))]
train_test_prod[,c1 := lapply(c1, unlist, use.names=F)]
head(train_test_prod)

uniq_tod <- unique(unlist(lapply(train_test_prod$tod, function(k) str_extract_all(string = k, pattern = "[[:digit:]]+:"))))

length(uniq_tod)

toColumns <- function(data, variables){
  for(i in variables){
    data[,paste0("tod_",i) := lapply(c1, function(x) any(match(i,x)))]
  }
  return (data)
}

toColumns(train_test_prod, uniq_tod)
train_test_prod[,c1 := NULL]

## see how it looks
head(train_test_prod)
str(train_test_prod)

## encode TRUE and NA
genx <- grep(pattern = "tod_", x = colnames(train_test_prod), value = T)

for(k in genx)
  set(train_test_prod, i = which(is.na(train_test_prod[[k]])), j = k, value = 0)

for(k in genx)
  set(train_test_prod, i = which(train_test_prod[[k]] == TRUE), j= k ,value = 1)

#changing the column names
genx = genx[3:26]
colnames(train_test_prod)[grep(x = colnames(train_test_prod), pattern = "\\:")] = 
  gsub(x = colnames(train_test_prod[,genx, with = F]), pattern = "\\:", replacement = "")

train_test_prod[, ':=' (tod_0 = unlist(tod_0),
                        tod_1 = unlist(tod_1),
                        tod_2 = unlist(tod_2),
                        tod_3 = unlist(tod_3),
                        tod_4 = unlist(tod_4),
                        tod_5 = unlist(tod_5),
                        tod_6 = unlist(tod_6),
                        tod_7 = unlist(tod_7),
                        tod_8 = unlist(tod_8),
                        tod_9 = unlist(tod_9),
                        tod_10 = unlist(tod_10),
                        tod_11 = unlist(tod_11),
                        tod_12 = unlist(tod_12),
                        tod_13 = unlist(tod_13),
                        tod_14 = unlist(tod_14),
                        tod_15 = unlist(tod_15),
                        tod_16 = unlist(tod_16),
                        tod_17 = unlist(tod_17),
                        tod_18 = unlist(tod_18),
                        tod_19 = unlist(tod_19),
                        tod_20 = unlist(tod_20),
                        tod_21 = unlist(tod_21),
                        tod_22 = unlist(tod_22),
                        tod_23 = unlist(tod_23))]

## Creating new column per dow
train_test_prod[,c1 := lapply(dow, function(k) str_extract_all(string = k, pattern = "[[:digit:]]+:"))]
train_test_prod[,c1 := lapply(c1, unlist, use.names=F)]
head(train_test_prod)

uniq_dow <- unique(unlist(lapply(train_test_prod$dow, function(k) str_extract_all(string = k, pattern = "[[:digit:]]+:"))))

length(uniq_dow)

toColumns <- function(data, variables){
  for(i in variables){    
    data[,paste0("dow_",i) := lapply(c1, function(x) any(match(i,x)))]
  }
  return (data)
}

toColumns(train_test_prod, uniq_dow)
train_test_prod[,c1 := NULL]

## see how it looks
head(train_test_prod)
str(train_test_prod)

## encode TRUE and NA
genx <- grep(pattern = "dow_", x = colnames(train_test_prod), value = T)

for(k in genx)
  set(train_test_prod, i = which(is.na(train_test_prod[[k]])), j = k, value = 0)

for(k in genx)
  set(train_test_prod, i = which(train_test_prod[[k]] == TRUE), j= k ,value = 1)

#changing the column names
genx = genx[3:9]
colnames(train_test_prod)[grep(x = colnames(train_test_prod), pattern = "\\:")] = 
  gsub(x = colnames(train_test_prod[,genx, with = F]), pattern = "\\:", replacement = "")

train_test_prod[, ':=' (dow_1 = unlist(dow_1),
                        dow_2 = unlist(dow_2),
                        dow_3 = unlist(dow_3),
                        dow_4 = unlist(dow_4),
                        dow_5 = unlist(dow_5),
                        dow_6 = unlist(dow_6),
                        dow_7 = unlist(dow_7))]

#Counting the number of alphanumerics
train_test_prod[,titles_alnum := lapply(titles, function(k) unlist(regmatches(x = k, gregexpr(pattern = "[A-Za-z]+[0-9]+|[0-9]+[A-Za-z]+", text =  k ))))]
train_test_prod[,titles_alnum := unlist(lapply(titles_alnum, length))]

#Counting the number of VS 
train_test_prod[,vs_count := lapply(titles, function(k) unlist(regmatches(x = k, gregexpr(pattern = " vs " , text =  k ))))]
train_test_prod[,vs_count := unlist(lapply(vs_count, length))]

#Finding the mean length of each word in the title.
train_test_prod[,w1 := lapply(titles, function(k) unlist(strsplit(x = k, split = ",")))]
train_test_prod[,w1 := lapply(w1, function(k) unlist(regmatches(x = k, gregexpr(pattern = ".*\\:", text =  k ))))]
train_test_prod[,w1 := lapply(w1, function(k) unlist(strsplit(x = k, split = " ")))]
train_test_prod[,w1 := lapply(w1, function(k) (nchar(k)))]
train_test_prod[,title_mean_len := unlist(lapply(w1, function(k) mean(k)))]
train_test_prod$title_mean_len[is.na(train_test_prod$title_mean_len)] =  mean(train_test_prod$title_mean_len, na.rm = T)
train_test_prod[,w1 := NULL]

#Calculating the Word frequencies
library(tm)
library(SnowballC)

train_test_prod[,w1 := lapply(titles, function(k) unlist(strsplit(x = k, split = ",")))]
train_test_prod[,w2 := lapply(w1, function(k) unlist(gsub(pattern = ":.*", replacement = "",x = k)))]
train_test_prod[,w3 := unlist(lapply(w2, function(k) paste(unlist(k), collapse = " ")))]
train_test_prod[,':=' (w1 = NULL,
                       w2 = NULL)]

corpus = Corpus(VectorSource(train_test_prod$w3))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords, c(stopwords('english')))
corpus = tm_map(corpus,stemDocument)

frequencies = DocumentTermMatrix(corpus)

#inspect(frequencies[1:10,1:10])
#findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.99)
sparse_mat = as.matrix(sparse)
rownames(sparse_mat) = 1: nrow(sparse_mat)

sparse_mat[1:10,1:10]

title_sparse=as.data.frame(sparse_mat)
colnames(title_sparse) = paste0('tm_',colnames(title_sparse))
dim(title_sparse)
head(title_sparse[,1:10])

train_test_prod = cbind(train_test_prod, title_sparse)
train_test_prod[,w3:=NULL]

#################################
test_prod_id <- train_test_prod$ID[is.na(train_test_prod$segment)]
str(train_test_prod)
head(train_test_prod)

## remove variables for modeling - train
train_test_prod[,':=' (ID = NULL,
                       genres = as.numeric(as.factor(genres)),
                       titles = as.numeric(as.factor(titles)),
                       cities = as.numeric(as.factor(cities)),
                       dow = as.numeric(as.factor(dow)),
                       tod = as.numeric(as.factor(tod))) ]

#Splitting back to train and test
train_prod = train_test_prod[!is.na(segment),]
test_prod = train_test_prod[is.na(segment),]
train_prod$segment = as.factor(train_prod$segment)
test_prod$segment = NULL

str(train_prod)
str(test_prod)

#Splitting for local train and test
set.seed(100)
split = sample(nrow(train_prod), 0.6*nrow(train_prod), replace=F)
train_local= train_prod[split,]
test_local= train_prod[-split,]


#XGB
library(xgboost)
train_prod$segment = as.numeric(train_prod$segment) - 1
train_local$segment = as.numeric(train_local$segment) - 1
test_local$segment = as.numeric(test_local$segment) - 1

drop_xgb = c('Science_gen','Badminton_gen','Kabaddi_gen','Sport_gen','Athletics_gen',
             'Hockey_gen','Formula_gen','Table_gen','Tennis_gen',
             'Volleyball_gen','FormulaE_gen',
             'NA_gen','Boxing_gen','Swimming_gen')
xgb_indep = setdiff(colnames(train_local), c(drop_xgb, 'segment'))
xgb_dep = 'segment'

train_local = as.data.frame(train_local)
test_local = as.data.frame(test_local)
train_prod = as.data.frame(train_prod)
test_prod = as.data.frame(test_prod)

dtrain_prod = xgb.DMatrix(data=as.matrix(train_prod[,xgb_indep]), label =  train_prod[,xgb_dep])
dtest_prod = xgb.DMatrix(data=as.matrix(test_prod[,xgb_indep]))

dtrain_local = xgb.DMatrix(data=as.matrix(train_local[,xgb_indep]), label =  train_local[,xgb_dep])
dtest_local = xgb.DMatrix(data=as.matrix(test_local[,xgb_indep]), label =  test_local[,xgb_dep])

watchlist = list(train = dtrain_local, test = dtest_local)

set.seed(150)
xgb.local.model = xgb.train(data = dtrain_prod,
                            watchlist = watchlist,
                            nround = 600, 
                            max_depth = 6, 
                            eta = 0.03,
                            colsample_bytree = 0.6,
                            subsample = 0.7,
                            objective = 'binary:logistic', 
                            eval_metric = 'auc',
                            early_stopping_rounds = 20,
                            maximize = T)

xgb.pred.prod = predict(xgb.local.model, newdata = dtest_prod)

var_imp = xgb.importance(model = xgb.local.model, feature_names = xgb_indep)
xgb.plot.importance(var_imp)

XGB_sub = data.frame(ID = test_prod_id, segment = xgb.pred.prod)
write.csv(XGB_sub, row.names=F, 'sub_XGB_16.csv')
