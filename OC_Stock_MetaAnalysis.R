
#### This will be the code for the Bayesian Hierarchical Meta-Regressions.

## Load Library
library(here)
library(bayestestR)
library(posterior)
library(dplyr)
library(tidyr)
library(brms)
library(emmeans)



data_Import <- read.csv(here("Data", "MetaAnalysis_SedCorgStockData.csv"), na.strings = c("", "na"))



### Prepare the datasets

#### Non-interactive (between predictors) ####

## For global analysis

global <- data_Import

## For species analysis ## Note: Remove multiple species, under-represented species

  ## Global

  ## Bioregional ## Note: 

## For tidal class analysis ## Note: Remove undefined.

  ## Global

  ## Bioregional ## Note: Remove bioregions with insufficient counts of either tidal class.

## For hydrologic setting analysis ## Note: Remove undefined.

  ## Global

  ## Bioregional ## Note: Remove bioregions with insufficient counts of either hydrologic setting.


#### Interactive (between predictors) ####

## For species-tidal class

  ## Global

  ## Bioregional

## For species-hyrologic analysis

  ## Global

  ## Bioregional

## For hydrologic-tidal class analysis

  ## Global

  ## Bioregional

## For hydrologic-tidal-species analysis

  ## Global

  ## Bioregional
















