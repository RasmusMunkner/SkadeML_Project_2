
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")

####################################################
#Read in data
Data <- ReadData("processed_df") %>% 
  mutate(RowNr = row_number()) %>% 
  distinct(across(-RowNr), .keep_all = T) %>% 
  mutate(ClaimAmountMod = 2 * ClaimAmount * (Gender == 2) + 1 * ClaimAmount * (Gender = 1))

####################################################
#Set up tasks
Task <- Data %>%
  select(-ClaimAmountMod, -RowNr) %>% 
  as_task_regr(target = "ClaimAmount")

TaskModified <- Data %>%
  select(-ClaimAmount, -RowNr) %>% 
  as_task_regr(target = "ClaimAmountMod")

#####################################################
#Set up models
XgbLearner <- lrn("regr.xgboost")
XgbLearner$param_set$values <- readRDS("hyperparameter_tuning_result.RData")
XgbLearner$train(Task)

XgbLearnerModified <- lrn("regr.xgboost")
XgbLearnerModified$param_set$values <- readRDS("hyperparameter_tuning_result_GP.RData")
XgbLearnerModified$train(TaskModified)

#############################################
#Causality-based debias
AugmentedPredictionData <- Data %>% 
  add_column(Prediction_Biased = XgbLearner$predict_newdata(.)$response) %>% 
  select(-Gender) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Prediction_Debiased = XgbLearner$predict_newdata(.)$response) %>% 
  group_by(RowNr) %>% 
  summarise(Prediction_Debiased = sum(Prediction_Debiased * Prob), Prediction_Biased = mean(Prediction_Biased))

AugmentedPredictionDataModified <- Data %>% 
  add_column(Prediction_Biased = XgbLearnerModified$predict_newdata(.)$response) %>% 
  mutate(OriginalGender = Gender) %>% 
  select(-Gender) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Prediction_Debiased = XgbLearnerModified$predict_newdata(.)$response) %>% 
  group_by(RowNr, OriginalGender) %>% 
  summarise(Prediction_Debiased = sum(Prediction_Debiased * Prob), Prediction_Biased = mean(Prediction_Biased))

#########################################################
#Functional-decomposition debiasing

Debiasframe <- function(df, colKey = "Gender"){
  for (name in colnames(df)){
    if(str_detect(name, colKey)){
      nameSymb <- enquo(name)
      df <- df %>% mutate(!!nameSymb := 0)
    }
  }
  return(df)
}

#Fit a native Xgboost
XgboostModel <- xgboost(data=Data %>% select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>% as.matrix(),
                        label=Data %>% select(ClaimAmount) %>% unlist(),
                        nrounds=XgbLearner$param_set$values$nrounds,
                        params=list(max_depth=XgbLearner$param_set$values$max_depth,
                                    eta=XgbLearner$param_set$values$eta),
                        monotone_constraint = c(Exposure = 1)
)

#Apply the glex function
glex_xgb <- glex(XgboostModel, Data %>% select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>% as.matrix()) 

XgboostModelModified <- xgboost(data=Data %>% select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>% as.matrix(),
                        label=Data %>% select(ClaimAmountMod) %>% unlist(),
                        nrounds=XgbLearnerModified$param_set$values$nrounds,
                        params=list(max_depth=XgbLearnerModified$param_set$values$max_depth,
                                    eta=XgbLearnerModified$param_set$values$eta),
                        monotone_constraint = c(Exposure = 1)
)

glex_xgb_mod <- glex(XgboostModelModified, Data %>% select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>% as.matrix()) 

glex_xgb_mod$m <- glex_xgb_mod$m %>% Debiasframe()

glex_vi(glex_xgb_mod) %>% autoplot(threshold = 5)

##########################################################
#Tables

AugmentedPredictionData %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563))

AugmentedPredictionDataModified %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563))

##########################################################
#Graphs
AugmentedPredictionData %>% 
  pivot_longer(names_to = "Predictor", cols = c("Prediction_Biased", "Prediction_Debiased")) %>% 
  ggplot() +
  geom_density(aes(x = value, color = Predictor)) +
  scale_x_log10() +
  xlab("Estimate") +
  ylab("Density")

AugmentedPredictionDataModified %>% 
  pivot_longer(names_to = "Predictor", cols = c("Prediction_Biased", "Prediction_Debiased")) %>% 
  ggplot() +
  geom_density(aes(x = pmax(30, value), color = Predictor)) +
  scale_x_log10() +
  xlab("Estimate") +
  ylab("Density")







