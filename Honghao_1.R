##### 1. Load packages and data ######
library(dplyr)
library(tidyverse)
library(mlr3)
library(lubridate)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(sp)
library(xts)
library(zoo)
library(forcats)
library(CASdatasets)
library(fastDummies)
source("Rasmus_Funktioner.R")

#Load the data into memory
data("freMPL1")

ProcessedData <- freMPL1 %>% 
  select(-c(RecordBeg, RecordEnd, ClaimInd)) %>% 
  #mutate(VehEnergy = fct_collapse(VehEnergy,
  #                                regular=c("regular", "eletric", "GPL")),
  #       VehEngine = fct_collapse(VehEngine,
  #                                injection=c("injection", "electric", "GPL")),
  #       HasKmLimit = factor(HasKmLimit)) %>% 
  mutate(ClaimAmount = pmax(ClaimAmount, 0)) %>% 
  #TreeModelGrouping("SocioCateg", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehAge", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehMaxSpeed", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehPrice", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehClass", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("RiskVar", "ClaimAmount", maxdepth = 2) %>% 
  #dummy_cols(remove_selected_columns = T, remove_first_dummy = T) %>% 
  dplyr::rename_all(list(~make.names(.)))


levels <- unique(ProcessedData$VehAge)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehAge == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehAge) <- as.character(rank(-means))
ProcessedData$VehAge <- as.numeric(ProcessedData$VehAge)

levels <- unique(ProcessedData$Gender)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$Gender == l) %>% pull(ClaimAmount)))
levels(ProcessedData$Gender) <- as.character(rank(-means))
ProcessedData$Gender <- as.numeric(ProcessedData$Gender)

levels <- unique(ProcessedData$MariStat)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$MariStat == l) %>% pull(ClaimAmount)))
levels(ProcessedData$MariStat) <- as.character(rank(-means))
ProcessedData$MariStat<- as.numeric(ProcessedData$MariStat)

levels <- unique(ProcessedData$SocioCateg)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$SocioCateg == l) %>% pull(ClaimAmount)))
levels(ProcessedData$SocioCateg) <- as.character(rank(-means))
ProcessedData$SocioCateg<- as.numeric(ProcessedData$SocioCateg)

levels <- unique(ProcessedData$VehUsage)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehUsage == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehUsage) <- as.character(rank(-means))
ProcessedData$VehUsage<- as.numeric(ProcessedData$VehUsage)

levels <- unique(ProcessedData$VehBody)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehBody == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehBody) <- as.character(rank(-means))
ProcessedData$VehBody<- as.numeric(ProcessedData$VehBody)

levels <- unique(ProcessedData$VehPrice)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehPrice == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehPrice) <- as.character(rank(-means))
ProcessedData$VehPrice<- as.numeric(ProcessedData$VehPrice)

levels <- unique(ProcessedData$VehEngine)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehEngine == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehEngine) <- as.character(rank(-means))
ProcessedData$VehEngine<- as.numeric(ProcessedData$VehEngine)

levels <- unique(ProcessedData$VehEnergy)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehEnergy == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehEnergy) <- as.character(rank(-means))
ProcessedData$VehEnergy<- as.numeric(ProcessedData$VehEnergy)

levels <- unique(ProcessedData$VehMaxSpeed)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehMaxSpeed == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehMaxSpeed) <- as.character(rank(-means))
ProcessedData$VehMaxSpeed<-as.numeric(ProcessedData$VehMaxSpeed)

levels <- unique(ProcessedData$VehClass)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$VehClass == l) %>% pull(ClaimAmount)))
levels(ProcessedData$VehClass) <- as.character(rank(-means))
ProcessedData$VehClass<- as.numeric(ProcessedData$VehClass)

levels <- unique(ProcessedData$Garage)
means <- sapply(levels, function(l) mean(ProcessedData %>% filter(ProcessedData$Garage == l) %>% pull(ClaimAmount)))
levels(ProcessedData$Garage) <- as.character(rank(-means))
ProcessedData$Garage<-as.numeric(ProcessedData$Garage)




#install.packages("remotes")
#library(remotes)
#remotes::install_github("xgboost.unify")



{
  library(remotes)
  library(tidyverse)
  library(mlr3)
  library(mlr3verse)
  library(mlr3learners)
  library(xgboost)
  library(mlr3tuning)
  library(patchwork)
  #remotes::install_github("PlantedML/glex")
  library(glex)
  source("Rasmus_Funktioner.R")
}



##### 1. Load packages and data ######
library(dplyr)
library(tidyverse)
library(mlr3)
library(lubridate)
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#install.packages(c("xts", "sp", "zoo"))
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
#install.packages('gmodels')
library(gmodels)
library(sp)
library(xts)
library(zoo)
library(forcats)
library(CASdatasets)
library(fastDummies)
source("Rasmus_Funktioner.R")

#Load the data into memory
data("freMPL1")

ProcessedData <- freMPL1 %>% 
  select(-c(RecordBeg, RecordEnd, ClaimInd)) %>% 
  #mutate(VehEnergy = fct_collapse(VehEnergy,
  #                                regular=c("regular", "eletric", "GPL")),
  #       VehEngine = fct_collapse(VehEngine,
  #                                injection=c("injection", "electric", "GPL")),
  #       HasKmLimit = factor(HasKmLimit)) %>% 
  mutate(ClaimAmount = pmax(ClaimAmount, 0)) %>% 
  #TreeModelGrouping("SocioCateg", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehAge", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehMaxSpeed", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehPrice", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("VehClass", "ClaimAmount", maxdepth = 2) %>% 
  #TreeModelGrouping("RiskVar", "ClaimAmount", maxdepth = 2) %>% 
  #dummy_cols(remove_selected_columns = T, remove_first_dummy = T) %>% 
  dplyr::rename_all(list(~make.names(.)))


cat_to_num <- function(x) {
    x <- factor(x)
    levels <- unique(x)
    means <- sapply(levels, function(l) mean(ProcessedData %>% filter(x == l) %>% pull(ClaimAmount)))
    levels(x) <- as.character(rank(-means))
    as.numeric(x)
}

cat_to_num()

sapply(ProcessedData$SocioCateg, cat_to_num)



###############################################################
#Read data
###############################################################
source("Rasmus_Funktioner.R")
Data <- ReadData("processed_df")

DataTrain <- Data %>% 
  distinct()

###############################################################
#MLR3 fitting
###############################################################

Task <- DataTrain %>%
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


##### 1. Train xgboost #####
my_xgb_learner = lrn("regr.xgboost",
                     eta = to_tune(0, 0.2),
                     nrounds = to_tune(10,100),
                     max_depth = to_tune(1, 3))

##### hyperparameter tuning setting
instance = tune(
  #method = tnr("random_search"), ### tuning method
  method = tnr("random_search"), ### tuning method
  task = task_model1,
  learner = my_xgb_learner,
  resampling = rsmp("cv", folds = 3), #### resampling method: 5-fold cross validation
  measures = msr("regr.rmse"), #### root mean squared error
  terminator = trm("evals", n_evals = 2) #### terminator
)

##### define tuned xgb learner and train on training data
xgb_tuned = lrn("regr.xgboost")  
xgb_tuned$param_set$values = instance$result_learner_param_vals
xgb_tuned$train(Task)

predictions <- xgb_tuned$predict_newdata(ProcessedData2_test)
xgb_mse <- mean((predictions$response-ProcessedData2_test$ClaimAmount)^2)
sqrt(xgb_mse)


###############################################################
#Fit a native xgb model matching the MLR3 results
###############################################################

XgbLearner$tuning_result

DataXgboost <- Data %>% select(-ClaimAmount) %>% as.data.frame()
ResponseXgboost <- Data %>% select(ClaimAmount) %>% unlist()

XgboostModel <- xgboost(data=DataXgboost %>% as.matrix(),
                        label=ResponseXgboost,
                        nrounds=XgbLearner$tuning_result$nrounds,
                        params=list(max_depth=XgbLearner$tuning_result$max_depth, 
                                    eta=XgbLearner$tuning_result$eta)
                        #,monotone_constraint = c(Exposure = 1)
)

###############################################################
#Tree-SHAP
###############################################################

#unified_xgb <- xgboost.unify(XgboostModel, DataXgboost)
#treeshap_xgb <- treeshap(unified_xgb,  DataXgboost, verbose = 0)

#plot_contribution(treeshap_xgb, obs = 1)  # Explain prediction for observation (row) 700
#plot_feature_importance(treeshap_xgb, max_vars = 6) # feature importance
#plot_feature_dependence(treeshap_xgb, "Exposure")
#plot_feature_dependence(treeshap_xgb, "BonusMalus")
#plot_feature_dependence(treeshap_xgb, "DrivAge")

###############################################################
# glex - Additive function decomposition
###############################################################

res <- glex(XgboostModel, DataXgboost %>% as.matrix()) #### calculates functional decomposition with marginal identification

res$m
res$shap

# Calculating sum of components and sum of SHAP values
sum_m_rpf <- rowSums(res$m) + res$intercept
sum_shap_xgb <- rowSums(res$shap) + res$intercept

sum_m_rpf[1:5]
sum_shap_xgb[1:5]

# Model predictions
predict(XgbLearner, DataXgboost)[1:5]
XgbLearner$predict_newdata(DataXgboost[1:5,])


vi_xgb <- glex_vi(res)

p_vi <- autoplot(vi_xgb, threshold = 0) + 
  labs(title = NULL, tag = "XGBoost-explanation")

p_vi+
  plot_annotation(title = "Variable importance scores by term") & 
  theme(plot.tag.position = "bottomleft")

TransformFrame <- function(df, colKey = "Gender"){
  for (name in colnames(df)){
    if(str_detect(name, colKey)){
      nameSymb <- enquo(name)
      df <- df %>% mutate(!!nameSymb := 0)
    }
  }
  return(df)
}

tibble(DebiasedEst = res$m %>% TransformFrame("Gender") %>% rowSums(),
       BiasedEst = res$m %>% rowSums()
) %>% 
  ggplot(aes(x = BiasedEst, y = BiasedEst - DebiasedEst)) +
  geom_point()


plot(as.numeric(Data$Exposure[order(Data$Exposure)], as.numeric(res$m$Exposure[order(Data$Exposure)]),type="l", xlab="x2", ylab="m2"))

ggplot(data = NULL, mapping = aes(x = Data$Exposure, y = res$m$Exposure)) +
  geom_line()

############################################################################
# Factor reorder by mean response - Dont wanna do it
# TestData <- freMPL1 %>% 
#   as_tibble() %>% 
#   select(ClaimAmount, VehBody) %>% 
#   mutate(meanClaim = mean(ClaimAmount), .by = VehBody) %>% 
#   arrange(meanClaim) %>% 
#   mutate(newFct = meanClaim %>% factor(labels = unique(VehBody)))
# 
# TestData %>% 
#   ggplot(aes(x = newFct, y = meanClaim)) +
#   geom_point()



