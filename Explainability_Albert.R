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
remotes::install_github("PlantedML/glex")
library(glex)
source("Rasmus_Funktioner.R")
}


#Loading data
#Data <- ReadData("processed_df")
Data <- ProcessedData

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

XgbLearner <- auto_tuner(tuner = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 8),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 40))

#Training model
XgbLearner$train(Task)
XgbLearner$base_learner()$param_set$values %>% saveRDS(file="hyperparameter_tuning_result.Rdata")


#Now with "goodpredictor"
DataGP <- Data %>% 
distinct() %>% 
  add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmount = 2 * ClaimAmount * (GoodPredictor == 1) + ClaimAmount * (GoodPredictor = 0))

#Setting regression task
TaskGP <- DataGP %>%
  as_task_regr(target = "ClaimAmount")

#Specifying learner
XgbLearnerTemplateGP <- lrn("regr.xgboost",
                          eta = to_tune(0, 0.2),
                          nrounds = to_tune(10, 100),
                          max_depth = to_tune(1, 3))

XgbLearnerGP <- auto_tuner(tuner = tnr("random_search"),
                         learner = XgbLearnerTemplate,
                         resampling = rsmp("cv", folds = 8),
                         measure = msr("regr.mse"),
                         terminator = trm("evals", n_evals = 40))

XgbLearnerGP$train(TaskGP)
XgbLearnerGP$tuning_result
XgbLearner$tuning_result
XgbLearnerGP$base_learner()$param_set$values %>% saveRDS(file="hyperparameter_tuning_result_GP.Rdata")

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

#SHAP decomp. as well as functional decomposition
#Note that the tuning yields a max depth of 1, leaving us with no interaction
#terms. Perhaps if we increase the number of cross validation folds and
#hyperparameter configurations we will get max_depth>1, in which case
#I have prepared som plots below that can highlight these interactions in
#our model.

#glex_xgb$shap
#glex_xgb$m
#glex_xgb$intercept

#Summing every row in the functional decomposition and all the SHAP values
#to see en lines 82-85) that this actually yields the predictions made by
#our model
sum_m_xgb <- rowSums(glex_xgb$m) + glex_xgb$intercept
sum_shap_xgb<- rowSums(glex_xgb$shap) + glex_xgb$intercept


cbind(sum_m_xgb,
      sum_shap_xgb,
      predict(XgboostModel, DataXgboost %>% as.matrix()))

vi_xgb <- glex_vi(glex_xgb)

glex::autoplot.glex_vi(glex_xgb)
{
  p_vi1 <- autoplot(vi_xgb, threshold = .05) + 
     theme(text = element_text(size = 20))  
  p_vi1
}

#Plot for interactions
#{
#  p_vi2 <- autoplot(vi_xgb, by_degree = TRUE) + 
#    labs(title = NULL, tag = "XGBoost")
#  p_vi2
#}

{
  p1 <- autoplot(glex_xgb, "Exposure") + 
    theme(text = element_text(size = 20))  
  p2 <-autoplot(glex_xgb, "BonusMalus") + 
    theme(text = element_text(size = 20))  
  p3 <-autoplot(glex_xgb, "DrivAge") + 
    theme(text = element_text(size = 20)) 
  p<-(p3+p2)/p1
  p+plot_layout(guides = "collect")
}

#Plot for interactions
{
  p5 <- autoplot(glex_xgb, c("DrivAge", "LicAge")) + 
    labs(subtitle = "XGBoost")
  p5
}


{
  p6 <- glex_explain(glex_xgb, id = 2, 
                     threshold = 0.05,
                     max_interaction = 2,
                     predictors = 
                     ) + 
    labs(tag = "XGBoost")
  
  p6
}

#Waterfall plot
#Før: 24131 Nu: 14129
#Før: 25018 Nu: 15016
{
shap<- glex_xgb$shap
  
row<-14129

wtfl<- t(shap[row,])%>%
        as.data.frame()%>%
        rownames_to_column()%>%
      rename("Feature"="rowname")%>%
      rename("SHAP"="V1")%>% 
  filter(SHAP !=0)
  

wtfl<-waterfall(values = wtfl$SHAP, labels = wtfl$Feature,
                rect_text_labels = wtfl$Feature,
                rect_text_size = 1.7)
wtfl_24131<-wtfl+theme(axis.title.y= element_text(size=20),
           axis.title.x = element_text(size=20),
           title = element_text(),
           axis.ticks.x = element_blank())+
  ggtitle(paste("row_id",24131))+
  ylab("Prediction - Intercept")+
  xlab("Features")+
  ylim(-150,150)

shap<- glex_xgb$shap

row<-15016

wtfl<- t(shap[row,])%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename("Feature"="rowname")%>%
  rename("SHAP"="V1")%>% 
  filter(SHAP !=0)


wtfl<-waterfall(values = wtfl$SHAP, labels = wtfl$Feature,
                rect_text_labels = wtfl$Feature,
                rect_text_size = 1.7)

wtfl_25018<-wtfl+theme(axis.title.y= element_text(size=20),
                       axis.title.x = element_text(size=20),
                       title = element_text(),
                       axis.ticks.x = element_blank())+
  ggtitle(paste("row_id",25018))+
  ylab("Prediction - Intercept")+
  xlab("Features")+
  ylim(-150,150)

grid.arrange(wtfl_24131,wtfl_25018, nrow=2)
}
