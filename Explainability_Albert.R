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
remotes::install_github("PlantedML/glex")
library(glex)
source("Rasmus_Funktioner.R")
}


#Loading data
#Data <- ReadData("processed_df")
Data <- ProcessedData

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

XgbLearner <- auto_tuner(tuner = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 3),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 10))

#Training model
#XgbLearner$train(Task)

XgbLearner$tuning_result



DataXgboost <- DataTrain %>% select(-ClaimAmount) %>% as.data.frame()
ResponseXgboost <- DataTrain %>% select(ClaimAmount) %>% unlist()

XgboostModel <- xgboost(data=DataXgboost %>% as.matrix(),
                        label=ResponseXgboost,
                        nrounds=XgbLearner$tuning_result$nrounds,
                        params=list(max_depth=XgbLearner$tuning_result$max_depth,
                                    eta=XgbLearner$tuning_result$eta),
                        monotone_constraint = c(Exposure = 1)
)

glex_xgb <- glex(XgboostModel, DataXgboost %>% as.matrix()) 

#glex_xgb$shap
#glex_xgb$m
#glex_xgb$intercept

sum_m_xgb <- rowSums(glex_xgb$m) + glex_xgb$intercept
sum_shap_xgb<- rowSums(glex_xgb$shap) + glex_xgb$intercept


cbind(sum_m_xgb,
      sum_shap_xgb,
      XgbLearner$predict_newdata(DataXgboost)$response,
      predict(XgboostModel, DataXgboost %>% as.matrix()))

vi_xgb <- glex_vi(glex_xgb)

{
  p_vi1 <- autoplot(vi_xgb, threshold = .05) + 
    labs(title = NULL, tag = "XGBoost")
  p_vi1
}

#Plot for interactions
#{
#  p_vi2 <- autoplot(vi_xgb, by_degree = TRUE) + 
#    labs(title = NULL, tag = "XGBoost")
#  p_vi2
#}

{
  p1 <- autoplot(glex_xgb, "Exposure") + labs(subtitle = "XGBoost")
  p2 <-autoplot(glex_xgb, "BonusMalus") + labs(subtitle = "XGBoost")
  p3 <-autoplot(glex_xgb, "DrivAge") + labs(subtitle = "XGBoost")
  p4 <-autoplot(glex_xgb, "LicAge") + labs(subtitle = "XGBoost")
  grid.arrange(p1,p2,p3,p4, ncol=2)
}

#Plot for interactions
#{
#  p5 <- autoplot(glex_xgb, c("DrivAge", "Licage")) + 
#    labs(subtitle = "XGBoost")
#  p5
#}


{
  p6 <- glex_explain(glex_xgb, id = 2, 
                     threshold = 0.05,
                     max_interaction = 2,
                     predictors = 
                     ) + 
    labs(tag = "XGBoost")
  
  p6
}
