
#### This will be the code for the Bayesian Hierarchical Meta-Regressions.

## Load Library
library(here)
library(bayestestR)
library(posterior)
library(dplyr)
library(tidyr)
library(brms)
library(emmeans)




#### IMPORTING DATA ####

### Import the dataset
data_Import <- read.csv(here("Data", "MetaAnalysis_SedCorgStockData.csv"), na.strings = c("", "na"))

## Add a "publication" column with factor variable for random effect.
data_Import <- data_Import %>% 
  group_by(Citation) %>% 
  mutate(PAPER = cur_group_id()) %>% 
  ungroup()

## Verify that columns are the correct type
data_Import <- data_Import %>% 
  transform(SedCorgStock_30cm_Mgha = as.numeric(SedCorgStock_30cm_Mgha)) %>% 
  transform(SedCorgStockError_SE = as.numeric(SedCorgStockError_SE)) %>% 
  transform(cores = as.numeric(cores))

#### End of importing data ####


#### PREPARING DATASETS ####
  ## Non-interactive (between predictors)

### GLOBAL ###
  ## Global
global <- data_Import

## SPECIES AND SPECIES LIFE-HISTORY
  ## Global
      ## Remove undefined and reports from meadows with multiple species.
species_global_all <- global[!grepl("\\.|,|spp", global$Species), ] %>% 
  filter(Genus != "undefined" & Genus != "Multiple")
      ## Remove species with too few observations
      ## View(species_global %>% group_by(Bioregion, Species) %>% summarize(n = n(), cores = sum(cores)))
      ## 1-2: Zostera caulensis, Zostera chilensis, Halophila baillonii, Halodule emarginata
      ## 3: Z. japonica
      ## 4: A. antarctica, Halodule ovalis
species_global <- merge(species_global_all, species_global_all %>% group_by(Species) %>% summarize(n = n())) %>% 
  filter(n >= 3) %>% 
  select(!n)
  ## Bioregional 
       ## Remove species with insufficient counts within a given bioregion.
       ## View(species_global %>% group_by(Bioregion, Species) %>% summarize(n = n()))
       ## View(species_bioregion %>% group_by(Bioregion, Species) %>% summarize(n = n()))
species_bioregion <- merge(species_global, species_global %>% group_by(Bioregion, Species) %>% summarize(n = n())) %>% 
  filter(n >= 3) %>% 
  select(!n)

### LIFE-HISTORY ###
  ## Global
      ## Remove undefined and reports from meadows with multiple life-histories.
life_global <- global %>% filter(Life_History != "undefined" & Life_History != "multiple")
  ## Bioregional
      ## Remove bioregions with insufficient counts of life-histories.
      ## lifehistory_global %>% group_by(Bioregion, Life_History) %>% summarize(n = n())
      ## Lowest is colonizing in the Mediterranean which is sufficient.
life_bioregion <- life_global


### TIDAL CLASS ###
  ## Global
      ## Remove undefined.
tidal_global <- global %>% filter(Tidal_Class != "")
  ## Bioregional ## Note: Remove bioregions with insufficient counts of either tidal class.
      ## tidal_global %>% group_by(Bioregion, Tidal_Class) %>% summarize(n = n())
      ## Lowest is intertidal in Mediterranean, is this too low?
tidal_bioregion <- tidal_global #%>% filter(Bioregion != "Mediterranean")
  
### HYDROLOGIC SETTING ###
  ## Global
      ## Remove undefined.
hydrologic_global <- global %>% 
  filter(Hydrologic_Setting != "")
  ## Bioregional 
      ## Remove bioregions with insufficient counts of either hydrologic setting.
      ## hydrologic_global %>% group_by(Bioregion, Hydrologic_Setting) %>% summarize(n = n())
      ## Lowest is estuarine in Mediterranean, is this too low?
hydrologic_bioregion <- hydrologic_global #%>% filter(Bioregion != "Mediterranean")


#### DATASETS FOR INTERACTIONS BETWEEN DRIVERS ####

### LIFE-HISTORY X TIDAL ###
  ## Global
life_tidal_global <- merge(life_global, tidal_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of life-history and tidal class
      ## life_tidal_bioregion %>% group_by(Bioregion, Life_History, Tidal_Class) %>% summarize(n = n())
      ## Temperate Southern Oceans (Persistent X Intertidal) has insufficient count (2)
          ## Do we drop that combo or the whole bioregion?
life_tidal_bioregion <- merge(life_bioregion, tidal_bioregion)

### LIFE-HISTORY X HYDROLOGIC SETTING ###
  ## Global
life_hydrologic_global <- merge(life_global, hydrologic_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of life-history and tidal class
      ## View(life_hydrologic_bioregion %>% group_by(Bioregion, Life_History, Hydrologic_Setting) %>% summarize(n = n()))
      ## Fewest are Med X persistent X estuarine (4) and Med X colonizing X estuarine (4) - both sufficient
life_hydrologic_bioregion <- merge(life_bioregion, hydrologic_bioregion)

## TIDAL CLASS X HYDROLOGIC SETTING
  ## Global
tidal_hydrologic_global <- merge(tidal_global, hydrologic_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of tidal class and hydrologic setting
      ## View(tidal_hydrologic_bioregion %>% group_by(Bioregion, Tidal_Class, Hydrologic_Setting) %>% summarize(n = n()))
      ## Med X Int X C/E; Med X Sub X E; TNA X Int C; TSO X Int X C all 2 or less
tidal_hydrologic_bioregion <- merge(tidal_bioregion, hydrologic_bioregion)

## For hydrologic-tidal-species analysis
  ## Global
life_tidal_hydrologic_global <- merge(life_global, merge(tidal_global, hydrologic_global))
  ## Bioregional
        ## Remove bioregions with insufficient counts of a given combo of life-history, tidal class and hydrologic setting
        ## View(life_tidal_hydrologic_bioregion %>% group_by(Bioregion, Life_History, Tidal_Class, Hydrologic_Setting) %>% summarize(n = n()))
        ## list:
life_tidal_hydrologic_bioregion <- merge(life_bioregion, merge(tidal_bioregion, hydrologic_bioregion))

#### END OF MAKING DATASETS ####

#### ESTABLISHING PRIORS ####

priors <- c(prior(student_t(3, 30, 200), lb = 0, class = b), ## prior assumption of the "slope"
            prior(student_t(3, 0, 200), class = sd)) ## prior assumption of the error around the mean


nullpriors<- c(prior(student_t(3, 30, 200), lb = 0, class = Intercept), ## prior assumption of the position of the intercept
               prior(student_t(3, 0, 200), class = sd)) ## prior assumption of the error around the mean

#### End of establishing priors ####

#### BUILDING MODELS ####

### Non-interactive between drivers ###

### Global ###
    ## Global
mod_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ Bioregion-1 + (1|PAPER),
                     global,
                     prior = priors,
                     iter = 1000, cores = 4,
                     family = gaussian(link = "identity"),
                     save_pars = save_pars(all=TRUE)) ## Diagnostics clear










