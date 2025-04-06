
#### This will be the code for the Bayesian Hierarchical Meta-Regressions.

## Load Library
library(here)
library(bayestestR)
library(posterior)
library(dplyr)
library(tidyr)
library(brms)
library(emmeans)
library(xlsx)


#### IMPORTING DATA ####

### Import the dataset
data_Import <- read.csv(here("Data", "MetaAnalysis_SedCorgStockData.csv"), na.strings = c("", "na"))

## Add a "publication" column with factor variable for random effect.
data_Import <- data_Import %>% 
  group_by(Citation) %>% 
  mutate(PAPER = cur_group_id()) %>% 
  ungroup()

## Make missing error equal to "0"
data_Import$SedCorgStockError_SE[is.na(data_Import$SedCorgStockError_SE)] <- "0"


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
    ## global %>% filter(SedCorgStock_30cm_Mgha < 250) %>% ggplot(aes(SedCorgStock_30cm_Mgha))+geom_histogram()
global <- data_Import %>% filter(SedCorgStock_30cm_Mgha < 1000)

## SPECIES ###
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
  filter(n >= 4) %>% 
  select(!n)
  ## Bioregional 
       ## Remove species with insufficient counts within a given bioregion.
        #View(species_global %>% group_by(Species) %>% summarize(n = n()))
        #View(species_bioregion %>% group_by(Bioregion, Species) %>% summarize(n = n()))
species_bioregion <- merge(species_global, species_global %>% group_by(Bioregion, Species) %>% summarize(n = n())) %>% 
  filter(n >= 4) %>% 
  select(!n)

### SPECIES EXCLUDING P. OCEANICA ###

species_no_oceanica_global <- species_global %>% filter(Species != "Posidonia oceanica")

species_no_oceanica_bioregion <- species_bioregion %>% filter(Species != "Posidonia oceanica")

### LIFE-HISTORY ###
  ## Global
      ## Remove undefined and reports from meadows with multiple life-histories.
life_global <- global %>% filter(Life_History != "undefined" & Life_History != "multiple")
  ## Bioregional
      ## Remove bioregions with insufficient counts of life-histories.
      ## life_global %>% group_by(Bioregion, Life_History) %>% summarize(n = n())
      ## Temperate North Pacific and Atlantic both have exclusively colonizing species reported
life_bioregion <- life_global %>% 
  filter(Bioregion != "Temperate North Pacific",
         Bioregion != "Temperate North Atlantic")

### LIFE-HISTORY EXCLUDING P. OCEANICA ###

life_no_oceanica_global <- life_global %>% filter(Species != "Posidonia oceanica") %>% 
  filter(Bioregion != "Mediterranean")

life_no_oceanica_bioregion <- life_bioregion %>% filter(Species != "Posidonia oceanica") %>% 
  filter(Bioregion != "Mediterranean")

### TIDAL CLASS ###
  ## Global
      ## Remove undefined.
tidal_global <- global %>% filter(Tidal_Class != "")
  ## Bioregional ## Note: Remove bioregions with insufficient counts of either tidal class.
      ## tidal_global %>% group_by(Bioregion, Tidal_Class) %>% summarize(n = n())
      ## Tropical atlantic does not have any intertidal observations
tidal_bioregion <- tidal_global %>% filter(Bioregion != "Tropical Atlantic")
  
### HYDROLOGIC SETTING ###
  ## Global
      ## Remove undefined.
hydrologic_global <- global %>% 
  filter(Hydrologic_Setting != "")
  ## Bioregional 
      ## Remove bioregions with insufficient counts of either hydrologic setting.
       # hydrologic_global %>% group_by(Bioregion, Hydrologic_Setting) %>% summarize(n = n())
      ## Lowest is estuarine in Mediterranean (8), which is sufficient (>= 4)
hydrologic_bioregion <- hydrologic_global


#### DATASETS FOR INTERACTIONS BETWEEN DRIVERS ####

### LIFE-HISTORY X TIDAL ###
  ## Global
life_tidal_global <- merge(life_global, tidal_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of life-history and tidal class
      # life_tidal_bioregion %>% group_by(Bioregion, Life_History, Tidal_Class) %>% summarize(n = n())
      ## Temperate Southern Oceans (Persistent X Intertidal) has insufficient count (2)
          ## Do we drop that combo or the whole bioregion?
life_tidal_bioregion <- merge(life_bioregion, tidal_bioregion) %>% filter(Bioregion != "Temperate Southern Oceans")

### LIFE-HISTORY X HYDROLOGIC SETTING ###
  ## Global
life_hydrologic_global <- merge(life_global, hydrologic_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of life-history and tidal class
      ## View(life_hydrologic_bioregion %>% group_by(Bioregion, Life_History, Hydrologic_Setting) %>% summarize(n = n()))
      ## Fewest are Med X persistent X estuarine (4) and Med X colonizing X estuarine (4) - which are sufficient (>= 4)
life_hydrologic_bioregion <- merge(life_bioregion, hydrologic_bioregion)

## TIDAL CLASS X HYDROLOGIC SETTING
  ## Global
tidal_hydrologic_global <- merge(tidal_global, hydrologic_global)
  ## Bioregional
      ## Remove bioregions with insufficient counts of a given combo of tidal class and hydrologic setting
      ## View(tidal_hydrologic_bioregion %>% group_by(Bioregion, Tidal_Class, Hydrologic_Setting) %>% summarize(n = n()))
      ## Med X Int X C/E; Med X Sub X E; TNA X Int C; TSO X Int X C all 2 or less
tidal_hydrologic_bioregion <- merge(tidal_bioregion, hydrologic_bioregion) %>% filter(Bioregion != "Mediterranean", 
                                                                                      Bioregion != "Temperate North Atlantic",
                                                                                      Bioregion != "Temperate Southen Oceans")

## For hydrologic-tidal-species analysis
  ## Global
life_tidal_hydrologic_global <- merge(life_global, merge(tidal_global, hydrologic_global))
  ## Bioregional
        ## Remove bioregions with insufficient counts of a given combo of life-history, tidal class and hydrologic setting
        ## View(life_tidal_hydrologic_bioregion %>% group_by(Bioregion, Life_History, Tidal_Class, Hydrologic_Setting) %>% summarize(n = n()))
        ## list: MCIC (1); MCIE (2); MCSE (2); TNACIC (2); TSOPIC (2); TIPPSE (3)
life_tidal_hydrologic_bioregion <- merge(life_bioregion, merge(tidal_bioregion, hydrologic_bioregion)) %>% filter(Bioregion != "Mediterranean",
                                                                                                                  Bioregion != "Temperate North Atalantic",
                                                                                                                  Bioregion != "Temperate Southern Oceans",
                                                                                                                  Bioregion != "Tropical Indo-Pacific")

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

mod_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ Bioregion-1 + (1|PAPER),
                     global,
                     prior = priors,
                     iter = 10000, cores = 4,
                     family = gaussian(link = "identity"),
                     save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                    global,
                    prior = nullpriors,
                    iter = 10000, cores = 4,
                    family = gaussian(link = "identity"),
                    save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))
brms::fixef(mod_global_null, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_global)

## Probability of model significance
post_prob(mod_global, mod_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_global, "Bioregion", point.est = median, mode = "hdi"))

#### ####

### Species ###

mod_species_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE)  ~  (Species-1) + (1|PAPER),
                   species_global,
                   prior = priors,
                   iter = 10000, cores = 4,
                   family = gaussian(link = "identity"),
                   control = list(adapt_delta = .99),
                   save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_species_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE)  ~  1 + (1|PAPER),
                          species_global,
                          prior = nullpriors,
                          iter = 10000, cores = 4,
                          family = gaussian(link = "identity"),
                          control = list(adapt_delta = .99),
                          save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_species_global, summary = TRUE, robust = FALSE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_species_global)

## Probability of model significance
post_prob(mod_species_global, mod_species_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_species_global, "Species", point.est = median, mode = "hdi"))

#### ####

mod_species_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  (Bioregion:Species)-1 + (1|PAPER),
                               species_bioregion,
                               prior = priors,
                               iter = 10000, cores = 4,
                               family = gaussian(link = "identity"),
                               control = list(adapt_delta = .99),
                               save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_species_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  1 + (1|PAPER),
                             species_bioregion,
                             prior = nullpriors,
                             iter = 10000, cores = 4,
                             family = gaussian(link = "identity"),
                             control = list(adapt_delta = .99),
                             save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_species_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_species_bioregion)

## Probability of model significance
post_prob(mod_species_bioregion, mod_species_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_species_bioregion, "Species", point.est = median, mode = "hdi"))

#### ####



### Species - No P. oceanica ###

mod_species_no_oceanica_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE)  ~  (Species-1) + (1|PAPER),
                          species_no_oceanica_global,
                          prior = priors,
                          iter = 10000, cores = 4,
                          family = gaussian(link = "identity"),
                          control = list(adapt_delta = .99),
                          save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_species_no_oceanica_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE)  ~  1 + (1|PAPER),
                               species_no_oceanica_global,
                               prior = nullpriors,
                               iter = 10000, cores = 4,
                               family = gaussian(link = "identity"),
                               control = list(adapt_delta = .99),
                               save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_species_no_oceanica_global, summary = TRUE, robust = FALSE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_species_no_oceanica_global)

## Probability of model significance
post_prob(mod_species_no_oceanica_global, mod_species_no_oceanica_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_species_no_oceanica_global, "Species", point.est = median, mode = "hdi"))

#### ####

mod_species_no_oceanica_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  (Bioregion:Species)-1 + (1|PAPER),
                             species_no_oceanica_bioregion,
                             prior = priors,
                             iter = 10000, cores = 4,
                             family = gaussian(link = "identity"),
                             control = list(adapt_delta = .99),
                             save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_species_no_oceanica_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  1 + (1|PAPER),
                              species_no_oceanica_bioregion,
                              prior = nullpriors,
                              iter = 10000, cores = 4,
                              family = gaussian(link = "identity"),
                              control = list(adapt_delta = .99),
                              save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_species_no_oceanica_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_species_no_oceanica_bioregion)

## Probability of model significance
post_prob(mod_species_no_oceanica_bioregion, mod_species_no_oceanica_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_species_no_oceanica_bioregion, "Species", point.est = median, mode = "hdi"))

#### ####


### Life-History ###

mod_life_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ Life_History-1 + (1|PAPER),
                       life_global,
                       prior = priors,
                       iter = 10000, cores = 4,
                       family = gaussian(link = "identity"),
                       control = list(adapt_delta = .99),
                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                       life_global,
                       prior = nullpriors,
                       iter = 10000, cores = 4,
                       family = gaussian(link = "identity"),
                       control = list(adapt_delta = .99),
                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_global)

## Probability of model significance
post_prob(mod_life_global, mod_life_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_global, "Life-History", point.est = median, mode = "hdi"))

#### ####

mod_life_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  (Bioregion:Life_History)-1 + (1|PAPER),
                       life_bioregion,
                       prior = priors,
                       iter = 10000, cores = 4,
                       family = gaussian(link = "identity"),
                       control = list(adapt_delta = .99),
                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  1 + (1|PAPER),
                       life_bioregion,
                       prior = nullpriors,
                       iter = 10000, cores = 4,
                       family = gaussian(link = "identity"),
                       control = list(adapt_delta = .99),
                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_bioregion)

## Probability of model significance
post_prob(mod_life_bioregion, mod_life_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_bioregion, "Life_History","Bioregion",  point.est = median, mode = "hdi"))

#### ####

### LIFE-HISTORY EXCLUDING P. OCEANICA ###

mod_life_no_oceanica_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ Life_History-1 + (1|PAPER),
                       life_no_oceanica_global,
                       prior = priors,
                       iter = 10000, cores = 4,
                       family = gaussian(link = "identity"),
                       control = list(adapt_delta = .99),
                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_no_oceanica_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                            life_no_oceanica_global,
                            prior = nullpriors,
                            iter = 10000, cores = 4,
                            family = gaussian(link = "identity"),
                            control = list(adapt_delta = .99),
                            save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_no_oceanica_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_no_oceanica_global)

## Probability of model significance
post_prob(mod_life_no_oceanica_global, mod_life_no_oceanica_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_no_oceanica_global, "Life-History", point.est = median, mode = "hdi"))

#### ####

mod_life_no_oceanica_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  (Bioregion:Life_History)-1 + (1|PAPER),
                          life_no_oceanica_bioregion,
                          prior = priors,
                          iter = 10000, cores = 4,
                          family = gaussian(link = "identity"),
                          control = list(adapt_delta = .99),
                          save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_no_oceanica_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~  1 + (1|PAPER),
                               life_no_oceanica_bioregion,
                               prior = nullpriors,
                               iter = 10000, cores = 4,
                               family = gaussian(link = "identity"),
                               control = list(adapt_delta = .99),
                               save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_no_oceanica_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_no_oceanica_bioregion)

## Probability of model significance
post_prob(mod_life_no_oceanica_bioregion, mod_life_no_oceanica_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_no_oceanica_bioregion, "Life_History","Bioregion",  point.est = median, mode = "hdi"))

#### ####


### Tidal Class ###

mod_tidal_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ (Tidal_Class-1) + (1|PAPER),
                         tidal_global,
                         prior = priors,
                         iter = 10000, cores = 4,
                         family = gaussian(link = "identity"),
                         control = list(adapt_delta = .99),
                         save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_tidal_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                         tidal_global,
                         prior = nullpriors,
                         iter = 10000, cores = 4,
                         family = gaussian(link = "identity"),
                         control = list(adapt_delta = .99),
                         save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_tidal_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_tidal_global)

## Probability of model significance
post_prob(mod_tidal_global, mod_tidal_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_tidal_global, "Tidal_Class", point.est = median, mode = "hdi"))

#### ####

mod_tidal_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ (Bioregion:Tidal_Class)-1 + (1|PAPER),
                         tidal_bioregion,
                         prior = priors,
                         iter = 10000, cores = 4,
                         family = gaussian(link = "identity"),
                         control = list(adapt_delta = .99),
                         save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_tidal_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                           tidal_bioregion,
                           prior = nullpriors,
                           iter = 10000, cores = 4,
                           family = gaussian(link = "identity"),
                           control = list(adapt_delta = .99),
                           save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_tidal_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_tidal_bioregion)

## Probability of model significance
post_prob(mod_tidal_bioregion, mod_tidal_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_tidal_bioregion, "Tidal_Class", point.est = median, mode = "hdi"))

#### ####

### Hydrologic Setting ### 

mod_hydrologic_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ (Hydrologic_Setting-1) + (1|PAPER),
                   hydrologic_global,
                   prior = priors,
                   iter = 10000, cores = 4,
                   family =  gaussian(link = "identity"),
                   control = list(adapt_delta = .99),
                   save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_hydrologic_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                  hydrologic_global,
                  prior = nullpriors,
                  iter = 10000, cores = 4,
                  family =  gaussian(link = "identity"),
                  control = list(adapt_delta = .99),
                  save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_hydrologic_global)

## Probability of model significance
post_prob(mod_hydrologic_global, mod_hydrologic_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_hydrologic_global, "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####

mod_hydrologic_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Bioregion:Hydrologic_Setting)-1) + (1|PAPER),
                 hydrologic_bioregion,
                 prior = priors,
                 iter = 10000, cores = 4,
                 family =  gaussian(link = "identity"),
                 control = list(adapt_delta = .99),
                 save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_hydrologic_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                hydrologic_bioregion,
                prior = nullpriors,
                iter = 10000, cores = 4,
                family =  gaussian(link = "identity"),
                control = list(adapt_delta = .99),
                save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_hydrologic_bioregion)

## Probability of model significance
post_prob(mod_hydrologic_bioregion, mod_hydrologic_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_hydrologic_bioregion, "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####

### ### ###




### Interactive between drivers ###


### Life History X Tidal Class ###

mod_life_tidal_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Life_History:Tidal_Class)-1) + (1|PAPER),
                                life_tidal_global,
                                prior = priors,
                                iter = 10000, cores = 4,
                                family =  gaussian(link = "identity"),
                                control = list(adapt_delta = .99),
                                save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_tidal_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                     life_tidal_global,
                                     prior = nullpriors,
                                     iter = 10000, cores = 4,
                                     family =  gaussian(link = "identity"),
                                     control = list(adapt_delta = .99),
                                     save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_tidal_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_tidal_global)

## Probability of model significance
post_prob(mod_life_tidal_global, mod_life_tidal_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_tidal_global, "Life_History", "Tidal_Class", point.est = median, mode = "hdi"))

#### ####

mod_life_tidal_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Bioregion:Life_History:Tidal_Class)-1) + (1|PAPER),
                             life_tidal_bioregion,
                             prior = priors,
                             iter = 10000, cores = 4,
                             family =  gaussian(link = "identity"),
                             control = list(adapt_delta = .99),
                             save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_tidal_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                  life_tidal_bioregion,
                                  prior = nullpriors,
                                  iter = 10000, cores = 4,
                                  family =  gaussian(link = "identity"),
                                  control = list(adapt_delta = .99),
                                  save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_tidal_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_tidal_bioregion)

## Probability of model significance
post_prob(mod_life_tidal_bioregion, mod_life_tidal_bioregion_null)

# contrast(emmeans())

## Estimated Marginal Mean difference between each Bioregion pair
contrast(emmeans(mod_life_tidal_bioregion, "Bioregion", "Life_History", "Tidal_Class", point.est = median, mode = "hdi"), interaction = "pairwise")

#### ####


### Life History X Hydrologic Setting ###

mod_life_hydrologic_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Life_History:Hydrologic_Setting)-1) + (1|PAPER),
                             life_hydrologic_global,
                             prior = priors,
                             iter = 10000, cores = 4,
                             family =  gaussian(link = "identity"),
                             control = list(adapt_delta = .99),
                             save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_hydrologic_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                  life_hydrologic_global,
                                  prior = nullpriors,
                                  iter = 10000, cores = 4,
                                  family =  gaussian(link = "identity"),
                                  control = list(adapt_delta = .99),
                                  save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_hydrologic_global)

## Probability of model significance
post_prob(mod_life_hydrologic_global, mod_life_hydrologic_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_hydrologic_global, "Life_History", "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####

mod_life_hydrologic_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Bioregion:Life_History:Hydrologic_Setting)-1) + (1|PAPER),
                                life_hydrologic_bioregion,
                                prior = priors,
                                iter = 10000, cores = 4,
                                family =  gaussian(link = "identity"),
                                control = list(adapt_delta = .99),
                                save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_hydrologic_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                     life_hydrologic_bioregion,
                                     prior = nullpriors,
                                     iter = 10000, cores = 4,
                                     family =  gaussian(link = "identity"),
                                     control = list(adapt_delta = .99),
                                     save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_hydrologic_bioregion)

## Probability of model significance
post_prob(mod_life_hydrologic_bioregion, mod_life_hydrologic_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_hydrologic_bioregion, "Bioregion", "Life_History", "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####


### Tidal Class X Hydrologic Setting ###

mod_tidal_hydrologic_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Tidal_Class:Hydrologic_Setting)-1) + (1|PAPER),
                                   tidal_hydrologic_global,
                                  prior = priors,
                                  iter = 10000, cores = 4,
                                  family =  gaussian(link = "identity"),
                                  control = list(adapt_delta = .99),
                                  save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_tidal_hydrologic_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                        tidal_hydrologic_global,
                                       prior = nullpriors,
                                       iter = 10000, cores = 4,
                                       family =  gaussian(link = "identity"),
                                       control = list(adapt_delta = .99),
                                       save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_tidal_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_tidal_hydrologic_global)

## Probability of model significance
post_prob(mod_tidal_hydrologic_global, mod_life_hydrologic_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_tidal_hydrologic_global, "Hydrologic_Setting", "Tidal_Class", point.est = median, mode = "hdi"))

#### ####

mod_tidal_hydrologic_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Bioregion:Tidal_Class:Hydrologic_Setting)-1) + (1|PAPER),
                                     tidal_hydrologic_bioregion,
                                     prior = priors,
                                     iter = 10000, cores = 4,
                                     family =  gaussian(link = "identity"),
                                     control = list(adapt_delta = .99),
                                     save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_tidal_hydrologic_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                          tidal_hydrologic_bioregion,
                                          prior = nullpriors,
                                          iter = 10000, cores = 4,
                                          family =  gaussian(link = "identity"),
                                          control = list(adapt_delta = .99),
                                          save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_tidal_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_tidal_hydrologic_bioregion)

## Probability of model significance
post_prob(mod_tidal_hydrologic_bioregion, mod_tidal_hydrologic_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_tidal_hydrologic_bioregion, "Bioregion", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####


### Life History X Tidal Class X Hydrologic Setting ###

mod_life_tidal_hydrologic_global <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Life_History:Tidal_Class:Hydrologic_Setting)-1) + (1|PAPER),
                                   tidal_hydrologic_global,
                                   prior = priors,
                                   iter = 10000, cores = 4,
                                   family =  gaussian(link = "identity"),
                                   control = list(adapt_delta = .99),
                                   save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_tidal_hydrologic_global_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                        tidal_hydrologic_global,
                                        prior = nullpriors,
                                        iter = 10000, cores = 4,
                                        family =  gaussian(link = "identity"),
                                        control = list(adapt_delta = .99),
                                        save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_tidal_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_tidal_hydrologic_global)

## Probability of model significance
post_prob(mod_life_tidal_hydrologic_global, mod_life_hydrologic_global_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_tidal_hydrologic_global, "Life_History", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####

mod_life_tidal_hydrologic_bioregion <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ ((Bioregion:Life_History:Tidal_Class:Hydrologic_Setting)-1) + (1|PAPER),
                                      tidal_hydrologic_bioregion,
                                      prior = priors,
                                      iter = 10000, cores = 4,
                                      family =  gaussian(link = "identity"),
                                      control = list(adapt_delta = .99),
                                      save_pars = save_pars(all=TRUE)) ## Diagnostics clear

mod_life_tidal_hydrologic_bioregion_null <- brm(SedCorgStock_30cm_Mgha | se(SedCorgStockError_SE, sigma = TRUE) ~ 1 + (1|PAPER),
                                           tidal_hydrologic_bioregion,
                                           prior = nullpriors,
                                           iter = 10000, cores = 4,
                                           family =  gaussian(link = "identity"),
                                           control = list(adapt_delta = .99),
                                           save_pars = save_pars(all=TRUE)) ## Diagnostics clear

## Extract population-level effects. robust = TRUE gives the median as the measure of central tendency.
brms::fixef(mod_life_tidal_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))

## Monte-Carlo Standard Error for each estimate
mcse(mod_life_tidal_hydrologic_bioregion)

## Probability of model significance
post_prob(mod_life_tidal_hydrologic_bioregion, mod_tidal_hydrologic_bioregion_null)

## Estimated Marginal Mean difference between each Bioregion pair
pairs(emmeans(mod_life_tidal_hydrologic_bioregion, "Bioregion", "Life_History", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))

#### ####


#### ANALYSIS OUTPUT ####

### Export Monte-Carlo Standard Error (MCSE) ###

write.xlsx(as.data.frame(mcse(mod_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Bioregions")
write.xlsx(as.data.frame(mcse(mod_global_null)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_species_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Species_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_species_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Species_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistory_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistory_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_tidal_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClass_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_tidal_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClass_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_hydrologic_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "HydrologicSetting_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_hydrologic_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "HydrologicSetting_Bioregion", append = TRUE)

write.xlsx(as.data.frame(mcse(mod_life_tidal_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistoryxTidalClass_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_tidal_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistoryxTidalClass_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_hydrologic_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistoryxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_hydrologic_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistoryxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_tidal_hydrologic_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClassxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_tidal_hydrologic_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClassxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_tidal_hydrologic_global)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifexTidalClassxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(mcse(mod_life_tidal_hydrologic_bioregion)), file = here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifexTidalClassxHydrologic_Bioregion", append = TRUE)

#### #### ####

### Export Emmeans pairs ###

write.xlsx(as.data.frame(pairs(emmeans(mod_global, "Bioregion", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Bioregions")
write.xlsx(as.data.frame(pairs(emmeans(mod_species_global, "Species", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Species_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_species_bioregion, "Species", "Bioregion", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Species_Bioregion", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_global, "Life_History", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistory_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_bioregion, "Life_History", "Bioregion", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistory_Bioregion", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_tidal_global, "Tidal_Class", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalClass_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_tidal_bioregion, "Tidal_Class", "Bioregion", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalClass_Bioregion", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_hydrologic_global, "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Hydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_hydrologic_bioregion, "Hydrologic_Setting", "Bioregion", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Hydrologic_Bioregion", append = TRUE)
#*
write.xlsx(as.data.frame(pairs(emmeans(mod_life_tidal_global, "Tidal_Class", "Life_History", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistoryxTidal_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_tidal_bioregion,"Bioregion", "Life_History", "Tidal_Class", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistoryxTidal_Bioregion", append = TRUE)

#*
write.xlsx(as.data.frame(pairs(emmeans(mod_life_hydrologic_global, "Hydrologic_Setting", "Life_History", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistoryxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_hydrologic_bioregion, "Bioregion", "Life_History", "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistoryxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_tidal_hydrologic_global, "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_tidal_hydrologic_bioregion, "Bioregion", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_tidal_hydrologic_global, "Life_History", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifexTidalxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(pairs(emmeans(mod_life_tidal_hydrologic_bioregion, "Bioregion", "Life_History", "Tidal_Class", "Hydrologic_Setting", point.est = median, mode = "hdi"))), 
           file = here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifexTidalxHydrologic_Bioregion", append = TRUE)







pairs(emmeans(mod_life_hydrologic_bioregion, ~ Life_History * Hydrologic_Setting), simple = "each")

pairs(emmeans(mod_life_hydrologic_global, ~ Hydrologic_Setting * Life_History), simple = "each")

pairs(emmeans(mod_life_tidal_bioregion, ~ Tidal_Class * Life_History), simple = "each")

#### #### ####

### Export fixef parameter estimates ###

write.xlsx(as.data.frame(brms::fixef(mod_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))),
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Bioregions")
write.xlsx(as.data.frame(brms::fixef(mod_global_null, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))),
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_species_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Species_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_species_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Species_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistory_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistory_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_tidal_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalClass_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_tidal_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalClass_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "HydrologicSetting_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "HydrologicSetting_Bioregion", append = TRUE)

write.xlsx(as.data.frame(brms::fixef(mod_life_tidal_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxTidal_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_tidal_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxTidal_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_tidal_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_tidal_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalxHydrologic_Bioregion", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_tidal_hydrologic_global, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxTidalxHydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(brms::fixef(mod_life_tidal_hydrologic_bioregion, summary = TRUE, robust = TRUE, probs = c(0.025, 0.975, 0.05, 0.95, 0.1, 0.9))), 
           file = here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistoryxTidalxHydrologic_Bioregion", append = TRUE)

#### #### ####

### Posterior Probability ###

write.xlsx(as.data.frame(post_prob(mod_global, mod_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), sheetName = "Bioregion")
write.xlsx(as.data.frame(post_prob(mod_species_global, mod_species_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Species_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_species_bioregion, mod_species_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Species_Bioregion", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_life_global, mod_life_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_life_global, mod_life_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Bioregion", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_tidal_global, mod_tidal_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Tidal_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_tidal_bioregion, mod_tidal_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Tidal_Bioregion", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_hydrologic_global, mod_hydrologic_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Hydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_hydrologic_bioregion, mod_hydrologic_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Hydrologic_Bioregion", append = TRUE)

write.xlsx(as.data.frame(post_prob(mod_life_tidal_global, mod_life_tidal_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Tidal_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_life_tidal_bioregion, mod_life_tidal_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Tidal_Bioregion", append = TRUE)

write.xlsx(as.data.frame(post_prob(mod_life_hydrologic_global, mod_life_hydrologic_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Hydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_life_hydrologic_bioregion, mod_life_hydrologic_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Hydrologic_Bioregion", append = TRUE)

write.xlsx(as.data.frame(post_prob(mod_life_tidal_hydrologic_global, mod_life_tidal_hydrologic_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Tidal_Hydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_life_tidal_hydrologic_bioregion, mod_life_tidal_hydrologic_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "LifeHistory_Tidal_Hydrologic_Bioregion", append = TRUE)

write.xlsx(as.data.frame(post_prob(mod_tidal_hydrologic_global, mod_tidal_hydrologic_global_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Tidal_Hydrologic_Global", append = TRUE)
write.xlsx(as.data.frame(post_prob(mod_tidal_hydrologic_bioregion, mod_tidal_hydrologic_bioregion_null)), here("Output", "BayesianAnalysis", "PosteriorProbability", "PosteriorProbability.xlsx"), 
           sheetName = "Tidal_Hydrologic_Bioregion", append = TRUE)

### Print attributes of the tables ###

### N and cores of each dataset

global %>% summarize(n = n(), cores = sum(cores))
species_global %>% summarize(n = n(), cores = sum(cores))
species_bioregion %>% summarize(n = n(), cores = sum(cores))
life_global %>% summarize(n = n(), cores = sum(cores))
life_bioregion %>% summarize(n = n(), cores = sum(cores))
tidal_global %>% summarize(n = n(), cores = sum(cores))
tidal_bioregion %>% summarize(n = n(), cores = sum(cores))
hydrologic_global %>% summarize(n = n(), cores = sum(cores))
hydrologic_bioregion %>% summarize(n = n(), cores = sum(cores))

life_tidal_global %>% summarize(n = n(), cores = sum(cores))
life_tidal_bioregion %>% summarize(n = n(), cores = sum(cores))

life_hydrologic_global %>% summarize(n = n(), cores = sum(cores))
life_hydrologic_bioregion %>% summarize(n = n(), cores = sum(cores))

life_tidal_hydrologic_global %>% summarize(n = n(), cores = sum(cores))
life_tidal_hydrologic_bioregion %>% summarize(n = n(), cores = sum(cores))

tidal_hydrologic_global %>% summarize(n = n(), cores = sum(cores))
tidal_hydrologic_bioregion %>% summarize(n = n(), cores = sum(cores))


### Observations per predictor ### 

write.xlsx(as.data.frame(global %>% summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"),
           sheetName = "Global")
write.xlsx(as.data.frame(global %>% group_by(Bioregion) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "Bioregions", append = TRUE)
write.xlsx(as.data.frame(species_global %>% group_by(Species) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "Species_Global", append = TRUE)
write.xlsx(as.data.frame(species_bioregion %>% group_by(Bioregion, Species) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "Species_Bioregion", append = TRUE)
write.xlsx(as.data.frame(life_global %>% group_by(Life_History) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "LifeHistory_Global", append = TRUE)
write.xlsx(as.data.frame(life_bioregion %>% group_by(Bioregion, Life_History) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "LifeHistory_Bioregion", append = TRUE)
write.xlsx(as.data.frame(tidal_global %>% group_by(Tidal_Class) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "TidalClass_Global", append = TRUE)
write.xlsx(as.data.frame(tidal_bioregion %>% group_by(Bioregion, Tidal_Class) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "TidalClass_Bioregion", append = TRUE)
write.xlsx(as.data.frame(hydrologic_global %>% group_by(Hydrologic_Setting) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "HydrologicSetting_Global", append = TRUE)
write.xlsx(as.data.frame(hydrologic_bioregion %>% group_by(Bioregion, Hydrologic_Setting) %>%  summarize(n = n(), cores = sum(cores))), file = here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), 
           sheetName = "HydrologicSetting_Bioregion", append = TRUE)


#### #### #####

### Observations per pair.







