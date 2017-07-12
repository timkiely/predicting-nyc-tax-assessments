
# laod predictions vs actual for 3 different models and compare in single ROC chart


# logistic
logistic_pred <- readRDS("data/logistic-predicted.rds")
logistic_actual <- readRDS("data/logistic-actual.rds")

# baseline xgboost
base_pred <- readRDS("data/base-predicted.rds")
base_actual <- readRDS("data/base-actual.rds")


# feature-rich xgboost
enrich_pred <- readRDS("data/enrich-predicted.rds")
enrich_actal <- readRDS("data/enrich-actual.rds")



library(pROC)

logit_auc <- auc(logistic_actual, logistic_pred)
base_auc <- auc(base_actual, base_pred)
enrich_auc <- auc(enrich_actal, enrich_pred)

roc(logistic_actual, logistic_pred, plot=T, col = "black")
roc(base_actual, base_pred, plot=T, add=T, col = "red")
roc(enrich_actal, enrich_pred, plot=T, add=T, col = "darkgreen")
