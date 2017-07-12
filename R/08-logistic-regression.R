

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)





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
# basic data output form the EDA/Transformation script
dat <- readRDS("data/pluto-xgboost-data.rds")
dim(dat)

dat <- dat %>% mutate_if(is.character,factor)

# Remove Near-Zero Variance vairrables
# This also helps when a var has 
# mostly NA's
nzv <- caret::nearZeroVar(dat,freqCut = 95/5)
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
  ,'Building_Type'
)

include_in_feature_set <- names(dat)[!names(dat)%in%exclude_from_feature_set]






#----------------------------------------------------------------------------
# OUT OF TIME SAMPLE VALIDATION
# We are using out-of-time cross validation on this dataset
# i.e., training on data from 2010-2015 and validating on 2016
train <- dat %>% filter(Year!=2016)
test <- dat %>% filter(Year==2016)







#-------------------- training data --------------
# create training features and target variable
train.x <- select(train,one_of(include_in_feature_set))
train.y <- train$Target 

#-------------------- test data --------------
test.x <- select(test,one_of(include_in_feature_set))
test.y <- test$Target

#-------------------- test data --------------
model <- glm(Target ~.,family=binomial(link='logit'), data = train.x)

pred <- predict(model, test.x, type='response')


data.frame("pred" = pred,"target" = as.factor(test.y)) %>% 
  ggplot()+
  aes(x = target, y = pred, group = target)+
  geom_jitter(aes(color=pred>0.5), alpha=0.6)+
  geom_violin(fill="white",alpha=0.5, color="white") +
  scale_color_tableau()


#-------------------- feature importance --------------
imp_matrix <- caret::varImp(model)
print(head(imp_matrix))

#----------------- ROC and AUC --------------
# save actual and predicted
saveRDS(pred,"data/logistic-predicted.rds")
saveRDS(test.y,"data/logistic-actual.rds")

library(pROC)
auc_calc <- auc(test.y, pred)
roc(test.y, pred,plot=T, add=T)



end_time<-Sys.time()


cat("Run time:",end_time-start_time,"\n")
cat("Feature Count:",length(train.x),"\n")
cat("AUC:",auc_calc,"\n")






