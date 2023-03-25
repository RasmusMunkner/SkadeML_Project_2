
##### 1. Load packages and data ######
library(dplyr)
library(tidyverse)
library(mlr3)
library(lubridate)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(sp)
library(xts)
library(zoo)
library(forcats)
library(CASdatasets)
library(fastDummies)
source("Rasmus_Funktioner.R")
library(tidyverse)
library(mlr3verse)
library(xgboost)
library(glex)
library(patchwork)

###############################################################
#Read data
###############################################################
source("Rasmus_Funktioner.R")
Data <- ReadData("processed_df")

DataTrain <- Data %>% 
  distinct()

###############################################################
#MLR3 fitting
###############################################################


Task <- DataTrain %>%
  as_task_regr(target = "ClaimAmount")

### Honghao kode start
my_xgb_learner = lrn("regr.xgboost",
                     eta = to_tune(0, 0.2),
                     nrounds = to_tune(10, 100),
                     max_depth = to_tune(1, 3))


##### hyperparameter tuning setting
instance = tune(
  method = tnr("random_search"), ### tuning method
  #method = tnr("mbo"), ### tuning method
  task = Task,
  learner = my_xgb_learner,
  resampling = rsmp("cv", folds = 3), #### resampling method: 3-fold cross validation
  measures = msr("regr.mse"), #### root mean squared error
  terminator = trm("evals", n_evals = 10) #### terminator
)

##### define tuned xgb learner and train on training data
xgb_tuned = lrn("regr.xgboost")  
xgb_tuned$param_set$values = instance$result_learner_param_vals
xgb_tuned$train(Task)

predictions <- xgb_tuned$predict_newdata(DataTrain)
xgb_mse <- mean((predictions$response-DataTrain$ClaimAmount)^2)
sqrt(xgb_mse)
### Honghao kode slut

#XgbLearnerTemplate <- lrn("regr.xgboost",
#                          eta = to_tune(0, 0.2),
#                          nrounds = to_tune(10, 100),
#                          max_depth = to_tune(1, 3))

#XgbLearner <- auto_tuner(method=tnr("random_search"),
#                         learner = XgbLearnerTemplate,
#                         resampling = rsmp("cv", folds = 3),
#                         measure = msr("regr.mse"),
#                         terminator = trm("evals", n_evals = 20))

#XgbLearner$train(Task)


###############################################################
#Fit a native xgb model matching the MLR3 results
###############################################################
instance$result_learner_param_vals
XgbLearner$tuning_result

DataXgboost <- Data %>% select(-ClaimAmount) %>% as.data.frame()
ResponseXgboost <- Data %>% select(ClaimAmount) %>% unlist()

XgboostModel <- xgboost(data=DataXgboost %>% as.matrix(),
                        label=ResponseXgboost,
                        nrounds=42,
                        params=list(max_depth=1,eta=0.1421906)
                        ,monotone_constraint = c(Exposure = 1)
)

###############################################################
#Tree-SHAP
###############################################################

#unified_xgb <- xgboost.unify(XgboostModel, DataXgboost)
#treeshap_xgb <- treeshap(unified_xgb,  DataXgboost, verbose = 0)

#plot_contribution(treeshap_xgb, obs = 1)  # Explain prediction for observation (row) 700
#plot_feature_importance(treeshap_xgb, max_vars = 6) # feature importance
#plot_feature_dependence(treeshap_xgb, "Exposure")
#plot_feature_dependence(treeshap_xgb, "BonusMalus")
#plot_feature_dependence(treeshap_xgb, "DrivAge")

###############################################################
# glex - Additive function decomposition
###############################################################

res <- glex(XgboostModel, DataXgboost %>% as.matrix()) #### calculates functional decomposition with marginal identification

res$m
res$shap

# Calculating sum of components and sum of SHAP values
sum_m_rpf <- rowSums(res$m) + res$intercept
sum_shap_xgb <- rowSums(res$shap) + res$intercept

sum_m_rpf[1:5]
sum_shap_xgb[1:5]

# Model predictions
predict(xgb_tuned, DataXgboost)[1:5]
xgb_tuned$predict_newdata(DataXgboost[1:5,])





vi_xgb <- glex_vi(res)

p_vi <- autoplot(vi_xgb, threshold = 0) + 
  labs(title = NULL, tag = "XGBoost-explanation")

p_vi+
  plot_annotation(title = "Variable importance scores by term") & 
  theme(plot.tag.position = "bottomleft")

TransformFrame <- function(df, colKey = "Gender"){
  for (name in colnames(df)){
    if(str_detect(name, colKey)){
      nameSymb <- enquo(name)
      df <- df %>% mutate(!!nameSymb := 0)
    }
  }
  return(df)
}

tibble(DebiasedEst = res$m %>% TransformFrame("Gender") %>% rowSums(),
       BiasedEst = res$m %>% rowSums()
) %>% 
  ggplot(aes(x = BiasedEst, y = BiasedEst - DebiasedEst)) +
  geom_point()


plot(as.numeric(Data$Exposure[order(Data$Exposure)], as.numeric(res$m$Exposure[order(Data$Exposure)]),type="l", xlab="x2", ylab="m2"))

ggplot(data = NULL, mapping = aes(x = Data$Exposure, y = res$m$Exposure)) +
  geom_line()

############################################################################
# Factor reorder by mean response - Dont wanna do it
# TestData <- freMPL1 %>% 
#   as_tibble() %>% 
#   select(ClaimAmount, VehBody) %>% 
#   mutate(meanClaim = mean(ClaimAmount), .by = VehBody) %>% 
#   arrange(meanClaim) %>% 
#   mutate(newFct = meanClaim %>% factor(labels = unique(VehBody)))
# 
# TestData %>% 
#   ggplot(aes(x = newFct, y = meanClaim)) +
#   geom_point()



