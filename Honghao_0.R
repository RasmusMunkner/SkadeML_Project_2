library(mvtnorm)
library(mlr3)
library(mlr3learners )
library(mlr3tuning)
library(mlr3mbo)
library(glmnet)
library(xgboost)
library(ranger)
library(mgcv)
library(mlr3extralearners) # remotes::install_github("mlr-org/mlr3extralearners@*release")
library(rsample)

##### 0. Prepare data #####
##### lr: set task
ProcessedData2 <- ProcessedData %>%
  distinct()
colnames(ProcessedData2)<-c("Var1", "Var2", "Var3", "Var4", "ClaimAmount", "Var6", "Var7", "Var8", "Var9", "Var10", 
                           "Var11", "Var12", "Var13", "Var14", "Var15", "Var16", "Var17", "Var18", "Var19", 
                           "Var20", "Var21", "Var22", "Var23", "Var24", "Var25", "Var26", "Var27", "Var28", 
                           "Var29", "Var30", "Var31", "Var32", "Var33", "Var34", "Var35", "Var36", "Var37", 
                           "Var38", "Var39", "Var40", "Var41", "Var42", "Var43")

##### Split 
set.seed(1)
ProcessedData2_spilt <- initial_split(ProcessedData2, prop = .7)
ProcessedData2_train <- training(ProcessedData2_spilt)
ProcessedData2_test  <- testing(ProcessedData2_spilt)

task_model1 = as_task_regr(ProcessedData2_train, target = "ClaimAmount") 



##### 1. Train xgboost #####
my_xgb_learner = lrn("regr.xgboost",
                     eta = to_tune(0, 0.2),
                     nrounds = to_tune(10, 5000),
                     max_depth = to_tune(1, 3))

##### hyperparameter tuning setting
instance = tune(
  #method = tnr("random_search"), ### tuning method
  method = tnr("mbo"), ### tuning method
  task = task_model1,
  learner = my_xgb_learner,
  resampling = rsmp("cv", folds = 5), #### resampling method: 5-fold cross validation
  measures = msr("regr.rmse"), #### root mean squared error
  terminator = trm("evals", n_evals = 2) #### terminator
)

##### define tuned xgb learner and train on training data
xgb_tuned = lrn("regr.xgboost")  
xgb_tuned$param_set$values = instance$result_learner_param_vals
xgb_tuned$train(task_model1)

predictions <- xgb_tuned$predict_newdata(ProcessedData2_test)
xgb_mse <- mean((predictions$response-ProcessedData2_test$ClaimAmount)^2)
sqrt(xgb_mse)


##### 2. Train ranger #####
my_ranger_learner = lrn("regr.ranger",
                        mtry.ratio = to_tune(0.1,1),
                        min.node.size = to_tune(1, 50),
                        num.trees = 50)


##### hyperparameter tuning setting
instance = tune(
  #method = tnr("random_search"), ### tuning method
  method = tnr("mbo"), ### tuning method
  task = task_model1,
  learner = my_ranger_learner,
  resampling = rsmp("cv", folds = 5), #### resampling method: 5-fold cross validation
  measures = msr("regr.rmse"), #### root mean squared error
  terminator = trm("evals", n_evals = 10) #### terminator
)

##### define tuned ranger learner and train on training data
ranger_tuned = lrn("regr.ranger")  
ranger_tuned$param_set$values = instance$result_learner_param_vals
ranger_tuned$train(task_model1)

predictions <- ranger_tuned$predict_newdata(ProcessedData2_test)
ranger_mse <- mean((predictions$response-ProcessedData2_test$ClaimAmount)^2)
sqrt(ranger_mse)

##### 3. Train glmnet #####
##### set learner with search space
my_elasticnet_learner = lrn("regr.glmnet",
                            s= to_tune(0, 1),
                            alpha=to_tune(0, 1))


##### hyperparameter tuning setting
instance = tune(
  #method = tnr("random_search"), ### tuning method
  method = tnr("mbo"), ### tuning method
  task = task_model1,
  learner = my_elasticnet_learner,
  resampling = rsmp("cv", folds = 5), #### resampling method: 5-fold cross validation
  measures = msr("regr.rmse"), #### root mean squared error
  terminator = trm("evals", n_evals = 50) #### terminator
)

#### define tuned elasticnet ßlearner and train on training data
elasticnet_tuned = lrn("regr.glmnet")  
elasticnet_tuned$param_set$values = instance$result_learner_param_vals
elasticnet_tuned$train(task_model1)

predictions <- elasticnet_tuned$predict_newdata(ProcessedData2_test)
glm_mse <- mean((predictions$response-ProcessedData2_test$ClaimAmount)^2)
sqrt(glm_mse)


##### 4. Train Featureless #####
predictions <- mean(ProcessedData2_train$ClaimAmount)
Featureless_mse <- mean((predictions-ProcessedData2_test$ClaimAmount)^2)

sqrt(c(Featureless_mse, glm_mse, ranger_mse, xgb_mse))
