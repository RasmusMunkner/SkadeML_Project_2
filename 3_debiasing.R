
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")

####################################################
#Read in data
Data <- ReadData("processed_df") %>% #ReadData is a custom function
  mutate(RowNr = row_number()) %>% 
  distinct(across(-RowNr), .keep_all = T) %>% 
  mutate(ClaimAmountMod =
           2 * ClaimAmount * (Gender == 2) +
           1 * ClaimAmount * (Gender == 1)
         )

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
XgbLearner$param_set$values <-
  readRDS("hyperparameter_tuning_result.RData") #Hyperparameters from prior model
XgbLearner$train(Task)

XgbLearnerModified <- lrn("regr.xgboost")
XgbLearnerModified$param_set$values <-
  readRDS("hyperparameter_tuning_result_GP.RData") #Hyperparameters from prior model
XgbLearnerModified$train(TaskModified)

#############################################
#Causality-based debias
AugmentedPredictionData <- Data %>% 
  add_column(Prediction_Biased =
               XgbLearner$predict_newdata(.)$response) %>% 
  select(-Gender) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Prediction_Debiased =
               XgbLearner$predict_newdata(.)$response) %>% 
  group_by(RowNr) %>% 
  summarise(Prediction_Debiased = sum(Prediction_Debiased * Prob),
            Prediction_Biased = mean(Prediction_Biased)
            )

AugmentedPredictionDataModified <- Data %>% 
  add_column(Prediction_Biased =
               XgbLearnerModified$predict_newdata(.)$response) %>% 
  mutate(OriginalGender = Gender) %>% 
  select(-Gender) %>% 
  dplyr::cross_join(Data %>% 
                      group_by(Gender) %>% 
                      summarise(Prob = n() / nrow(.))) %>% 
  add_column(Prediction_Debiased =
               XgbLearnerModified$predict_newdata(.)$response) %>% 
  group_by(RowNr, OriginalGender) %>% 
  summarise(Prediction_Debiased = sum(Prediction_Debiased * Prob),
            Prediction_Biased = mean(Prediction_Biased)
            )

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
XgboostModel <- xgboost(
  data=Data %>%
    select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>%
    as.matrix(),
  label=Data %>% select(ClaimAmount) %>% unlist(),
  nrounds=XgbLearner$param_set$values$nrounds,
  params=list(max_depth=XgbLearner$param_set$values$max_depth,
              eta=XgbLearner$param_set$values$eta),
  monotone_constraint = c(Exposure = 1)
)

XgboostModelModified <- xgboost(
  data=Data %>%
    select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>%
    as.matrix(),
  label=Data %>% select(ClaimAmountMod) %>% unlist(),
  nrounds=XgbLearnerModified$param_set$values$nrounds,
  params=list(max_depth=XgbLearnerModified$param_set$values$max_depth,
              eta=XgbLearnerModified$param_set$values$eta),
  monotone_constraint = c(Exposure = 1)
)

#Apply the glex function
glex_xgb <- glex(XgboostModel, Data %>%
                   select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>%
                   as.matrix())

glex_xgb_mod <- glex(XgboostModelModified,
                     Data %>%
                       select(-ClaimAmountMod, -RowNr, -ClaimAmount) %>%
                       as.matrix()) 

#Debias based on the functional decomposition
glex_xgb_debias <- glex_xgb
glex_xgb_debias$m <- glex_xgb$m %>% Debiasframe()

glex_xgb_mod_debias <- glex_xgb_mod
glex_xgb_mod_debias$m <- glex_xgb_mod$m %>% Debiasframe()

##########################################################
#Tables

AugmentedPredictionData %>% 
  add_column(Prediction_FuncDebias =
               glex_xgb_debias$m %>% rowSums() +
               glex_xgb_debias$intercept
             ) %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563)) %>% 
  relocate(RowNr, Prediction_Biased,
           Prediction_Biased, Prediction_FuncDebias)

AugmentedPredictionDataModified %>% 
  add_column(Prediction_FuncDebias =
               glex_xgb_mod_debias$m %>% rowSums() +
               glex_xgb_mod_debias$intercept
             ) %>% 
  filter(RowNr %in% c(6257, 24131, 25018, 25196, 30503,30563)) %>% 
  relocate(RowNr,OriginalGender, Prediction_Biased,
           Prediction_Biased, Prediction_FuncDebias)