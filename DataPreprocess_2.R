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






#Writing the dataset to .csv files
source("Rasmus_Funktioner.R")
WriteData(ProcessedData, "processed_df")

