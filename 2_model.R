#Loading packages
{
  library(remotes)
  library(tidyverse)
  library(mlr3)
  library(mlr3verse)
  library(mlr3learners)
  library(xgboost)
  library(mlr3tuning)
  library(patchwork)
  library(gridExtra)
  library(waterfalls)
  #remotes::install_github("PlantedML/glex")
  library(glex)
  source("Rasmus_Funktioner.R")
}

#Loading data
#Data <- ReadData("processed_df")
Data <- ProcessedData


##### 1. Data without GoodPredictor #####
#Training data on subset without duplicates
DataTrain <- Data %>% 
  distinct()

#Setting regression task
Task <- DataTrain %>%
  as_task_regr(target = "ClaimAmount")

#Specifying learner
XgbLearnerTemplate <- lrn("regr.xgboost",
                          eta = to_tune(0, 0.2),
                          nrounds = to_tune(10, 100),
                          max_depth = to_tune(1, 3))

XgbLearner <- auto_tuner(method = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 8),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 40))

#Training model
XgbLearner$train(Task)
XgbLearner$base_learner()$param_set %>% save(file="hyperparameter_tuning_result.Rdata")


#We prepare data and labels to use in the native xgboost function
#This we need to do in order to use the "glex" function later on.
DataXgboost <- DataTrain %>% select(-ClaimAmount) %>% as.data.frame()
ResponseXgboost <- DataTrain %>% select(ClaimAmount) %>% unlist()

XgboostModel <- xgboost(data=DataXgboost %>% as.matrix(),
                        label=ResponseXgboost,
                        nrounds=XgbLearner$tuning_result$nrounds,
                        params=list(max_depth=XgbLearner$tuning_result$max_depth,
                                    eta=XgbLearner$tuning_result$eta),
                        monotone_constraint = c(Exposure = 1)
)

#We apply the glex function
glex_xgb <- glex(XgboostModel, DataXgboost %>% as.matrix()) 

#Summing every row in the functional decomposition and all the SHAP values
#to see en lines 82-85) that this actually yields the predictions made by
#our model
sum_m_xgb <- rowSums(glex_xgb$m) + glex_xgb$intercept
sum_shap_xgb<- rowSums(glex_xgb$shap) + glex_xgb$intercept

cbind(sum_m_xgb,
      sum_shap_xgb,
      XgbLearner$predict_newdata(DataXgboost)$response,
      predict(XgboostModel, DataXgboost %>% as.matrix()))


##### 2.Data with modified ClaimAmount #####
#ClaimAmount modified using formula:  
    #ClaimAmount + ClaimAmount * (Gender == 1) 
DataGP <- Data %>% 
  distinct() %>% 
  #add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmountMod = 1 * ClaimAmount * (Gender == 1) + ClaimAmount) %>%
  select(-ClaimAmount)

#Setting regression task
TaskGP <- DataGP %>%
  as_task_regr(target = "ClaimAmountMod")

#Specifying learner
XgbLearnerTemplateGP <- lrn("regr.xgboost",
                            eta = to_tune(0, 0.2),
                            nrounds = to_tune(10, 100),
                            max_depth = to_tune(1, 3))

XgbLearnerGP <- auto_tuner(method = tnr("random_search"),
                           learner = XgbLearnerTemplateGP,
                           resampling = rsmp("cv", folds = 8),
                           measure = msr("regr.mse"),
                           terminator = trm("evals", n_evals = 40))

XgbLearnerGP$train(TaskGP)
XgbLearnerGP$tuning_result
#XgbLearner$tuning_result
XgbLearnerGP$base_learner()$param_set %>% save(file="hyperparameter_tuning_result_GP.Rdata")
tuning_res<-load("hyperparameter_tuning_result_GP.Rdata")



#We prepare data and labels to use in the native xgboost function
#This we need to do in order to use the "glex" function later on.

DataXgboostGP <- DataGP %>% select(-ClaimAmountMod) %>% as.data.frame()
ResponseXgboostGP <- DataGP %>% select(ClaimAmountMod) %>% unlist()

XgboostModelGP <- xgboost(data=DataXgboostGP %>% as.matrix(),
                          label=ResponseXgboostGP,
                          nrounds=XgbLearnerGP$tuning_result$nrounds,
                          params=list(max_depth=XgbLearnerGP$tuning_result$max_depth,
                                      eta=XgbLearnerGP$tuning_result$eta),
                          monotone_constraint = c(Exposure = 1)
)


#We apply the glex function
glex_xgbGP <- glex(XgboostModelGP, DataXgboostGP %>% as.matrix()) 

#Summing every row in the functional decomposition and all the SHAP values
#to see en lines 82-85) that this actually yields the predictions made by
#our model
sum_m_xgbGP <- rowSums(glex_xgbGP$m) + glex_xgbGP$intercept
sum_shap_xgbGP <- rowSums(glex_xgbGP$shap) + glex_xgbGP$intercept


cbind(sum_m_xgbGP,
      sum_shap_xgbGP,
      XgbLearnerGP$predict_newdata(DataXgboostGP)$response,
      predict(XgboostModelGP, DataXgboostGP %>% as.matrix()))
