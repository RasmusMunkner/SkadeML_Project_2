


library(mlr3verse)
library(mlr3mbo)
source("Rasmus_Funktioner.R")

##########################################################################
#MLR3 stuff
PredictionRows <- 
ReadData("processed_df") %>% 
  mutate(Special = row_number() %in% c(6257, 24131, 25018, 25196, 30503,30563)) %>% 
  filter(Special == 1) %>% 
  select(-Special)

Task <- ReadData("processed_df") %>% 
  distinct() %>%
  as_task_regr(target = "ClaimAmount")

XgbLearnerTemplate <- lrn(
  "regr.xgboost",
  eta = to_tune(0, 0.2),
  nrounds = to_tune(10, 500),
  max_depth = to_tune(1, 3)
  )

XgbLearnerAuto <- auto_tuner(
  method = tnr("mbo"),
  learner = XgbLearnerTemplate,
  resampling = rsmp("cv", folds = 8),
  measure = msr("regr.mse"),
  terminator = trm("evals", n_evals = 200)
)

##########################################################################
#Train the model
#future::plan("multisession")
#XgbLearnerAuto$train(Task)
#future::plan("sequential")

#Save the model
save(XgbLearnerAuto, file = "XgbLearnerAuto.RData")
XgbLearnerAuto$archive$data %>% 
  as_tibble() %>% 
  ggplot(aes(x = eta, y = log(regr.mse) - log(min(regr.mse)), color = nrounds)) +
  geom_point() +
  facet_grid(~max_depth)
