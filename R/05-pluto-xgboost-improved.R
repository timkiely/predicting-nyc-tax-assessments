

#-------------------------------------------------------------
# This scripts takes the basic PLUTO modeling data from our EDA
# and runs an xgboost GBM machine learning prediction  algorithm
# Objective here is to use best tunning parameters in order to 
# create a baseline prediction, with the hope of using 
# feature engineering to create a superior prediction
# in a subsequent attempt


rm(list=ls())
library(xgboost)
library(methods)
library(Matrix)
library(tidyverse)
library(caret)
source("R/helper-functions.R")

start_time<-Sys.time()





#----------------------- LOAD DATA -----------------------
# feature-rich  output form the EDA/Transformation script
dat <- readRDS("data/pluto-xgboost-data-improved.rds")
dim(dat)

dat <- dat %>% mutate_if(is.character,factor)

# Remove Near-Zero Variance vairrables
# This also helps when a var has 
# mostly NA's

nzv <- caret::nearZeroVar(dat,freqCut = 95/5)

# saveRDS(nzv,"nzv-redo.rds")
#nzv<-readRDS("nzv-redo.rds")

dat <- dat[, -nzv]



# remove incomplete observations which can't be used for prediction
dat <- na.omit(dat)
dim(dat)







#--------- user input: exclude certain variables from features --------------
exclude_from_feature_set <- c(
  'BBL_derive'
  ,'Cumulative_Change'
  ,'Change_Assesment'
  ,'Year'
  ,'BldgClass' 
)

include_in_feature_set <- names(dat)[!names(dat)%in%exclude_from_feature_set]






#----------------------------------------------------------------------------
# OUT-OF-TIME VALIDATION
# We are using out-of-time cross validation on this dataset
# i.e., training on data from 2010-2015 and validating on 2016
train <- dat %>% filter(Year!=2016)
test <- dat %>% filter(Year==2016)







#-------------------- training data --------------
# create training features and target variable
train.x <- select(train,one_of(include_in_feature_set))
train.y <- train$Target 

# sparse matrix format allows us to include factors which are encoded automatically
sparse_matrix = sparse.model.matrix(Target~.-1, data = train.x)



#-------------------- test data --------------
test <- dat %>% filter(Year==2016) #39,022
test.x <- select(test,-BBL_derive,-Cumulative_Change,-Change_Assesment)
test.y <- test$Target
sparse_matrix.test = sparse.model.matrix(Target~.-1, data = test.x)



#---- 'watchlist' allows us to cross-validate ---------
dtrain <- xgb.DMatrix(data = sparse_matrix, label=train.y)
dtest <- xgb.DMatrix(data = sparse_matrix.test, label=test.y)

watchlist <- list(train=dtrain, test=dtest)

#--------------- Train the model ----------------
bst <- xgb.train(data=dtrain
                 , max_depth=4
                 , eta = 0.01
                 , nrounds=500
                 , watchlist=watchlist
                 , eval_metric = "error"
                 #, eval_metric = "logloss"
                 , objective = "binary:logistic"
                 , early.stop.round = 50)




#-------------------- prediction --------------
pred <- predict(bst, sparse_matrix.test)


data.frame("pred" = pred,"target" = as.factor(test.y)) %>% 
  ggplot()+
  aes(x = target, y = pred, group = target)+
  geom_jitter(aes(color=pred>0.5), alpha=0.6)+
  geom_violin(fill="white",alpha=0.5, color="white") +
  scale_color_tableau()


#-------------------- feature importance --------------
imp_matrix <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
print(imp_matrix)

# bar plot by gain
xgb.plot.importance(importance_matrix = imp_matrix[1:20,])

#----------------- ROC and AUC --------------
saveRDS(pred,"data/enrich-predicted.rds")
saveRDS(test.y,"data/enrich-actual.rds")

library(pROC)
auc_calc <- auc(test.y, pred)
roc(test.y, pred,plot=T)



end_time<-Sys.time()


cat("Run time:",end_time-start_time,"\n")
cat("Feature Count:",length(train.x),"\n")
cat("AUC:",auc_calc,"\n")







