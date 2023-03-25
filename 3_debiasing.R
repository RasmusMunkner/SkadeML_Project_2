
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")

####################################################
#Read in data
Data <- ReadData("processed_df") %>% 
  mutate(RowNr = row_number()) %>% 
  distinct(across(-RowNr), .keep_all = T) %>% 
  add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmountModified = 2 * ClaimAmount * (GoodPredictor == 1) + ClaimAmount * (GoodPredictor = 0))

####################################################
#Set up tasks
Task <- Data %>%
  select(-ClaimAmountModified, -RowNr) %>% 
  as_task_regr(target = "ClaimAmount")

TaskModified <- Data %>%
  select(-ClaimAmount, -RowNr) %>% 
  as_task_regr(target = "ClaimAmountModified")

#####################################################
#Set up models
XgbLearner <- lrn("regr.xgboost")
XgbLearner$param_set$values <- readRDS("hyperparameter_tuning_result.RData")
XgbLearner$train(Task)

XgbLearnerModified <- lrn("regr.xgboost")
XgbLearnerModified$param_set$values <- readRDS("hyperparameter_tuning_result_GP.RData")
XgbLearnerModified$train(TaskModified)

#############################################
#Predict
AugmentedPredictionData <- Data %>% 
  add_column(BiasPredict = XgbLearner$predict_newdata(.)$response) %>% 
  select(-Gender) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Predictions = XgbLearner$predict_newdata(.)$response) %>% 
  group_by(RowNr) %>% 
  summarise(Predicted = sum(Predictions * Prob), BiasedPredict = mean(BiasPredict))

AugmentedPredictionDataModified <- Data %>% 
  add_column(BiasPredict = XgbLearnerModified$predict_newdata(.)$response) %>% 
  mutate(OriginalGoodPredictorValue = GoodPredictor) %>% 
  select(-GoodPredictor) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(GoodPredictor) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Predictions = XgbLearnerModified$predict_newdata(.)$response) %>% 
  group_by(RowNr, OriginalGoodPredictorValue) %>% 
  summarise(Predicted = sum(Predictions * Prob), BiasedPredict = mean(BiasPredict))

##########################################################
#Tables

AugmentedPredictionData %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563))

AugmentedPredictionDataModified %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563))

##########################################################
#Graphs
AugmentedPredictionData %>% 
  pivot_longer(names_to = "Predictor", cols = c("Predicted", "BiasedPredict")) %>% 
  ggplot() +
  geom_density(aes(x = value, color = Predictor)) +
  xlab("Estimate") +
  ylab("Density")

AugmentedPredictionDataModified %>% 
  pivot_longer(names_to = "Predictor", cols = c("Predicted", "BiasedPredict")) %>% 
  ggplot() +
  geom_density(aes(x = value, color = Predictor)) +
  xlab("Estimate") +
  ylab("Density")







