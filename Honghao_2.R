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



##### Data without GoodPredictor #####

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
      XgbLearner$predict_newdata(DataXgboost)$response,
      predict(XgboostModel, DataXgboost %>% as.matrix()))

vi_xgb <- glex_vi(glex_xgb)

#glex::autoplot.glex_vi(glex_xgb)
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
{
  p5 <- autoplot(glex_xgb, c("DrivAge", "LicAge")) + 
    labs(subtitle = "XGBoost")
  p5
}


{
  p6 <- glex_explain(glex_xgb, id = 15016, 
                     threshold = 0.05,
                     max_interaction = 2,
                     predictors = 
  ) + 
    labs(tag = "XGBoost")
  
  p6
}

#Waterfall plot

PoliceNr <- 15016

shap <- glex_xgb$shap

wtfl<- t(shap[PoliceNr])%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename("Feature"="rowname")%>%
  rename("SHAP"="V1")%>% 
  filter(SHAP !=0)


waterfall(values = wtfl$SHAP, labels = wtfl$Feature,
          rect_text_labels = wtfl$Feature)+ 
  theme(axis.ticks.x=element_blank()) + ylab("123")


##### Data with modified ClaimAmount #####
#ClaimAmount modified using formula:  
  #0.25 * ClaimAmount * (Gender == 1) + ClaimAmount

#Now with "goodpredictor"
DataGP <- Data %>% 
  distinct() %>% 
  #add_column(GoodPredictor = rbinom(nrow(.), 1, prob = 0.5)) %>% 
  mutate(ClaimAmountMod = 1 * ClaimAmount * (Gender == 2) + ClaimAmount) %>%
  select(-ClaimAmount)

#Setting regression task
TaskGP <- DataGP %>%
  as_task_regr(target = "ClaimAmountMod")

#Specifying learner
XgbLearnerTemplateGP <- lrn("regr.xgboost",
                            eta = to_tune(0, 0.2),
                            nrounds = to_tune(10, 100),
                            max_depth = to_tune(2, 3))

XgbLearnerGP <- auto_tuner(method = tnr("random_search"),
                           learner = XgbLearnerTemplateGP,
                           resampling = rsmp("cv", folds = 8),
                           measure = msr("regr.mse"),
                           terminator = trm("evals", n_evals = 40)) #40

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
                        #nrounds=XgbLearnerGP$tuning_result$nrounds,
                        #params=list(max_depth=XgbLearnerGP$tuning_result$max_depth,
                        #            eta=XgbLearnerGP$tuning_result$eta),
                        nrounds=11,
                        params=list(max_depth=2,
                                    eta=0.170919907372445),
                        monotone_constraint = c(Exposure = 1)
)


#We apply the glex function
glex_xgbGP <- glex(XgboostModelGP, DataXgboostGP %>% as.matrix()) 

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
sum_m_xgbGP <- rowSums(glex_xgbGP$m) + glex_xgbGP$intercept
sum_shap_xgbGP <- rowSums(glex_xgbGP$shap) + glex_xgbGP$intercept


cbind(sum_m_xgbGP,
      sum_shap_xgbGP,
      XgbLearnerGP$predict_newdata(DataXgboostGP)$response,
      predict(XgboostModelGP, DataXgboostGP %>% as.matrix()))

vi_xgbGP <- glex_vi(glex_xgbGP)

#glex::autoplot.glex_vi(glex_xgbGP)
{
  p_vi1 <- autoplot(vi_xgbGP, threshold = 5) + 
    labs(title = NULL, tag = "XGBoost")
  p_vi1
}

#Plot for interactions:
#Ved ikke hvad det laver...
{
  p_vi2 <- autoplot(vi_xgb, by_degree = TRUE) + 
    labs(title = NULL, tag = "XGBoost")
  #p_vi2
}

{
  p1 <- autoplot(glex_xgbGP, "Exposure") #+ labs(subtitle = "XGBoost")
  p2 <- autoplot(glex_xgbGP, "Gender") # + labs(subtitle = "XGBoost")
  p3 <-autoplot(glex_xgbGP, "VehBody") #+ labs(subtitle = "XGBoost")
  p4 <-autoplot(glex_xgbGP, "VehEnergy") #+ labs(subtitle = "XGBoost")
  grid.arrange(p1,p2,p3,p4, ncol=2)
}


#Plot for interactions
{
  p5 <- autoplot(glex_xgbGP, c("Exposure", "Gender"), colour="Gender") + 
    labs(subtitle = "XGBoost")
  p5
}

PoliceNr <- which.min(predict(XgboostModelGP, DataXgboostGP %>% as.matrix()))
predict(XgboostModelGP, DataXgboostGP %>% as.matrix())[PoliceNr]
{
  p6 <- glex_explain(glex_xgbGP, id = PoliceNr, 
                     threshold = 0.05,
                     max_interaction = 2,
                     predictors = 
  ) + 
    labs(tag = "XGBoost")
  
  p6
}

#Waterfall plot

shap<- glex_xgbGP$shap

wtfl<- t(shap[PoliceNr,])%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename("Feature"="rowname")%>%
  rename("SHAP"="V1")%>% 
  filter(SHAP !=0)


waterfall(values = wtfl$SHAP, labels = wtfl$Feature,
          rect_text_labels = wtfl$Feature)+ 
  theme(axis.ticks.x=element_blank())



##### Debais
DataXgboostGP$Gender <- 1
Gender1 <- predict(XgboostModelGP, DataXgboostGP %>% as.matrix())
DataXgboostGP$Gender <- 2
Gender2 <- predict(XgboostModelGP, DataXgboostGP %>% as.matrix())
Z <- 12903/20591 * Gender1 + 7688/20591 * Gender2


TestData <- data.frame(glex_xgbGP$m)
TestData$Gender = 0
TestData$Exposure.Gender = 0
TestData$Gender.VehPrice = 0
TestData$Gender.VehBody = 0
TestData$Gender.VehMaxSpeed = 0
TestData$BonusMalus.Gender = 0
Z2 <- rowSums(TestData) + glex_xgbGP$intercept
summary(Z-Z2)
