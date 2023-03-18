
library(tidyverse)
library(mlr3verse)
source("Rasmus_Funktioner.R")

#Read the data, introduce an artificial good predictor that we wish to debias for
Data <- ReadData("processed_df") %>% 
  distinct() %>% 
  add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmount = 2 * ClaimAmount * (GoodPredictor == 1) + ClaimAmount * (GoodPredictor = 0))

#MLR3 stuff
Task <- Data %>%
  as_task_regr(target = "ClaimAmount")

XgbLearnerTemplate <- lrn("regr.xgboost",
                  eta = to_tune(0, 0.2),
                  nrounds = to_tune(10, 100),
                  max_depth = to_tune(1, 3))

XgbLearner <- auto_tuner(method = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 3),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 10))

#XgbLearner$train(Task)

#########################################################################################

#No Debiasing
Data %>% 
  add_column(Predicted = XgbLearner$predict_newdata(.)$response) %>% 
  ggplot(aes(x = Exposure, y = Predicted, color = factor(GoodPredictor))) +
  geom_smooth(method = "gam")

Data %>% 
  add_column(Predicted = XgbLearner$predict_newdata(.)$response) %>% 
  ggplot(aes(x = Exposure, y = Predicted, color = factor(GoodPredictor))) +
  geom_smooth(method = "gam")

Data %>% 
  add_column(Predicted = XgbLearner$predict_newdata(.)$response) %>% 
  ggplot(aes(x = BonusMalus, y = Predicted, color = factor(GoodPredictor))) +
  geom_smooth(method = "gam")


#Mean-Debiasing
AugmentedPredictionData <- Data %>% 
  select(-GoodPredictor) %>% 
  mutate(rowid = row_number()) %>% 
  cross_join(Data %>% 
               group_by(GoodPredictor) %>% 
               summarise(Prob = n() / nrow(.))) %>% 
  add_column(Predictions = XgbLearner$predict_newdata(.)$response) %>% 
  group_by(rowid) %>% 
  summarise(Predicted = sum(Predictions * Prob))

Data %>% 
  cbind(., AugmentedPredictionData) %>% 
  ggplot(aes(x = Exposure, y = Predicted, color = as.factor(GoodPredictor))) +
  geom_smooth(method = "gam")

Data %>% 
  cbind(., AugmentedPredictionData) %>% 
  ggplot(aes(x = BonusMalus, y = Predicted, color = as.factor(GoodPredictor))) +
  geom_smooth(method = "gam")



