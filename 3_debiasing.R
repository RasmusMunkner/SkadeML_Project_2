
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")

####################################################
#Read in data
Data <- ReadData("processed_df") %>% 
  mutate(Special = case_when(
    row_number() %in% c(6257, 24131, 25018, 25196, 30503,30563) ~ T,
    .default = F
  )) %>% 
  distinct() %>% 
  add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmountModified = 2 * ClaimAmount * (GoodPredictor == 1) + ClaimAmount * (GoodPredictor = 0))

####################################################
#Set up tasks
Task <- Data %>%
  select(-ClaimAmountModified, -Special) %>% 
  as_task_regr(target = "ClaimAmount")

TaskModified <- Data %>%
  select(-ClaimAmount, -Special) %>% 
  as_task_regr(target = "ClaimAmountModified")

#####################################################
#Set up models
load("XgbParams.RData")
XgbLearner <- lrn("regr.xgboost")
XgbLearner$param_set <- XgbParams

load("XgboostModelParams.RData")
XgbLearnerModified <- lrn("regr.xgboost")
XgbLearnerModified$param_set <- XgbParamsModified

#############################################
#Predict
AugmentedPredictionData <- Data %>% 
  add_column(BiasPredict = XgbLearner$predict_newdata(.)$response) %>% 
  select(-Gender) %>% 
  mutate(rowid = row_number()) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Predictions = XgbLearner$predict_newdata(.)$response) %>% 
  group_by(rowid) %>% 
  summarise(Predicted = sum(Predictions * Prob), BiasedPredict = mean(BiasPredict))

AugmentedPredictionDataModified <- Data %>% 
  add_column(BiasPredict = XgbLearnerModified$predict_newdata(.)$response) %>% 
  select(-GoodPredictor) %>% 
  mutate(rowid = row_number()) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(GoodPredictor) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Predictions = XgbLearnerModified$predict_newdata(.)$response) %>% 
  group_by(rowid) %>% 
  summarise(Predicted = sum(Predictions * Prob), BiasedPredict = mean(BiasPredict))

##########################################################
#Tables











