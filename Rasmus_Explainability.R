
#install.packages("remotes")
#library(remotes)
#remotes::install_github("PlantedML/glex")

library(tidyverse)
library(mlr3verse)
library(xgboost)
library(glex)
library(patchwork)


source("Rasmus_Funktioner.R")




Task <- Data %>%
  as_task_regr(target = "ClaimAmount")

XgbLearnerTemplate <- lrn("regr.xgboost",
                          eta = to_tune(0, 0.2),
                          nrounds = to_tune(10, 100),
                          max_depth = to_tune(1, 3))

XgbLearner <- auto_tuner(tuner = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 3),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 20))

#XgbLearner$train(Task)

XgbLearner$tuning_result

DataXgboost <- as.matrix(Data %>% select(-ClaimAmount))
ResponseXgboost <- Data %>% select(ClaimAmount) %>% unlist()

testboost <- xgboost(data=DataXgboost,
                     label=ResponseXgboost,
                     nrounds=26,
                     params=list(max_depth=1,eta=0.192421),
                     monotone_constraint = c(Exposure = 1)
                     )

res <- glex(testboost, DataXgboost) #### calculates functional decomposition with marginal identification
res$m

vi_xgb <- glex_vi(res)

p_vi <- autoplot(vi_xgb, threshold = 0) + 
  labs(title = NULL, tag = "XGBoost-explanation")

p_vi+
  plot_annotation(title = "Variable importance scores by term") & 
  theme(plot.tag.position = "bottomleft")


plot(as.numeric(x2[order(x2)]), as.numeric(res$m$x2[order(x2)]),type="l", xlab="x2", ylab="m2")

res$m$Exposure

plot(as.numeric(Data$Exposure[order(Data$Exposure)], as.numeric(res$m$Exposure[order(Data$Exposure)]),type="l", xlab="x2", ylab="m2"))

ggplot(data = NULL, mapping = aes(x = Data$Exposure, y = res$m$Exposure)) +
  geom_line()




