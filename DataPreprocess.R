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
  mutate(VehEnergy = fct_collapse(VehEnergy,
                                  regular=c("regular", "eletric", "GPL")),
         VehEngine = fct_collapse(VehEngine,
                                  injection=c("injection", "electric", "GPL")),
         HasKmLimit = factor(HasKmLimit)) %>% 
  TreeModelGrouping("SocioCateg", "ClaimAmount", maxdepth = 2) %>% 
  TreeModelGrouping("VehAge", "ClaimAmount", maxdepth = 2) %>% 
  TreeModelGrouping("VehMaxSpeed", "ClaimAmount", maxdepth = 2) %>% 
  TreeModelGrouping("VehPrice", "ClaimAmount", maxdepth = 2) %>% 
  TreeModelGrouping("VehClass", "ClaimAmount", maxdepth = 2) %>% 
  TreeModelGrouping("RiskVar", "ClaimAmount", maxdepth = 2) %>% 
  dummy_cols(remove_selected_columns = T, remove_first_dummy = T) %>% 
  dplyr::rename_all(list(~make.names(.)))
  

#Writing the dataset to .csv files
source("Rasmus_Funktioner.R")
WriteData(ProcessedData, "processed_df")

