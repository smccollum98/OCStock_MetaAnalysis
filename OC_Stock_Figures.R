

#### This code modifies and collates the datasets exported from the analysis ####
#### This process gives us more readable tables and is easier to make into figures. ####

### Libraries

library(dplyr)
library(stringr)

###



#### Dataset of the OC storage globally and in each bioregion ####

global_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Global") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = "Global")
global_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Global") %>%
  select(!NA.) %>% 
  mutate(Parameter = "Global")
global_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "Global") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = "Global")

global_data <- merge(global_coef, merge(global_mcse, global_count))

bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Bioregions") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = c("Mediterranean", "Temperate North Atlantic", 
                       "Temperate North Pacific", "Temperate Southern Oceans", 
                       "Tropical Atlantic", "Tropical Indo-Pacific"))
bioregion_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Bioregions") %>%
  select(!NA.) %>% 
  mutate(Parameter = c("Mediterranean", "Temperate North Atlantic", 
                       "Temperate North Pacific", "Temperate Southern Oceans", 
                       "Tropical Atlantic", "Tropical Indo-Pacific"))
bioregion_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "Bioregions") %>% 
  select(!Bioregion) %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = c("Mediterranean", "Temperate North Atlantic", 
                       "Temperate North Pacific", "Temperate Southern Oceans", 
                       "Tropical Atlantic", "Tropical Indo-Pacific"))

bioregion_data <- merge(bioregion_coef, merge(bioregion_mcse, bioregion_count))

global_bioregion_data <- rbind(global_data, bioregion_data)

write.xlsx(global_bioregion_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "Global_Bioregion")

#### #### ####

#### Species ####

species_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Species_Global") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Species", repl = ""))

species_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Species_Global") %>%
  select(!NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "b_Species", repl = ""))

species_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "Species_Global") %>% 
  select(!NA.) %>% 
  rename(Parameter = Species) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = " ", repl = ""))

species_data <- merge(species_coef, merge(species_mcse, species_count)) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Amphibolis", repl = "Amphibolis ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Cymodocea", repl = "Cymodocea ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Enhalus", repl = "Enhalus ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Halodule", repl = "Halodule ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Halophila", repl = "Halophila ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Posidonia", repl = "Posidonia ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Syringodium", repl = "Syringodium ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Thalassia", repl = "Thalassia ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Thalassodendron", repl = "Thalassodendron ")) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Zostera", repl = "Zostera "))

write.xlsx(species_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "Species_Global", append = TRUE)

#### #### ####

#### #### ####

### Species x Bioregion (filter out 160+) ####

species_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "Species_Bioregion") %>% 
  filter(Estimate < 160) %>% 
  separate(col = "NA.", into = c("Bioregion", "Species"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Bioregion", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Species", repl = ""))

species_bioregion_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "Species_Bioregion") %>% 
  select(!NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Species"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "b_Bioregion", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Species", repl = ""))

species_bioregion_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "Species_Bioregion") %>% 
  select(!NA.) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = " ", repl = "")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = " ", repl = ""))

species_bioregion_data <- merge(species_bioregion_coef, merge(species_bioregion_mcse, species_bioregion_count)) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Amphibolis", repl = "Amphibolis ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Cymodocea", repl = "Cymodocea ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Enhalus", repl = "Enhalus ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Halodule", repl = "Halodule ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Halophila", repl = "Halophila ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Posidonia", repl = "Posidonia ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Syringodium", repl = "Syringodium ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Thalassia", repl = "Thalassia ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Thalassodendron", repl = "Thalassodendron ")) %>% 
  mutate(Species = str_replace_all(string = Species, pattern = "Zostera", repl = "Zostera ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern "))

write.xlsx(species_bioregion_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "Species_Bioregion", append = TRUE)

#### #### ####

#### #### ####

### Life-History ###

life_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistory_Global") %>% 
  rename(Parameter = NA.) %>%
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Life_History", repl = ""))

life_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistory_Global") %>% 
  select(!NA.) %>%
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "b_Life_History", repl = ""))

life_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "LifeHistory_Global") %>% 
  select(!NA.) %>% 
  rename(Parameter = Life_History)

life_data <- merge(life_coef, merge(life_mcse, life_count))

write.xlsx(life_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "LifeHistory_Global", append = TRUE)

### Life-History X Bioregion ###

life_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "LifeHistory_Bioregion") %>% 
  separate(col = "NA.", into = c("Bioregion", "Life_History"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Bioregion", repl = "")) %>%
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-")) %>%  
  mutate(Life_History = str_replace_all(string = Life_History, pattern = "Life_History", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern "))

life_bioregion_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "LifeHistory_Bioregion") %>% 
  select(!NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Life_History"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "b_Bioregion", repl = "")) %>%
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-")) %>%  
  mutate(Life_History = str_replace_all(string = Life_History, pattern = "Life_History", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern "))

life_bioregion_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "LifeHistory_Bioregion") %>% 
  select(!NA.)

life_bioregion_data <- merge(life_bioregion_coef, merge(life_bioregion_mcse, life_bioregion_count))

write.xlsx(life_bioregion_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "LifeHistory_Bioregion", append = TRUE)


#### #### #### ####


### Tidal class ###

tidal_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalClass_Global") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Tidal_Class", repl = "")) 

tidal_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClass_Global") %>% 
  select(!NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "b_Tidal_Class", repl = "")) 

tidal_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "TidalClass_Global") %>% 
  select(!NA.) %>% 
  rename(Parameter = Tidal_Class)

tidal_data <- merge(tidal_coef, merge(tidal_mcse, tidal_count))

write.xlsx(tidal_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "TidalClass_Global", append = TRUE)

#### #### ####


### Tidal Class x Bioregion ###

tidal_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "TidalClass_Bioregion") %>% 
  rename(Parameter = NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Tidal_Class"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Bioregion", repl = "")) %>% 
  mutate(Tidal_Class = str_replace_all(string = Tidal_Class, pattern = "Tidal_Class", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-"))

tidal_bioregion_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "TidalClass_Bioregion") %>% 
  select(!NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Tidal_Class"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "b_Bioregion", repl = "")) %>% 
  mutate(Tidal_Class = str_replace_all(string = Tidal_Class, pattern = "Tidal_Class", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-"))

tidal_bioregion_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "TidalClass_Bioregion") %>% 
  select(!NA.)

tidal_bioregion_data <- merge(tidal_bioregion_coef, merge(tidal_bioregion_mcse, tidal_bioregion_count))

write.xlsx(tidal_bioregion_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "TidalClass_Bioregion", append = TRUE)

#### #### ####

### Hydrologic Setting ###

hydrologic_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "HydrologicSetting_Global") %>% 
  rename(Parameter = NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "Hydrologic_Setting", repl = ""))

hydrologic_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "HydrologicSetting_Global") %>% 
  select(!NA.) %>% 
  mutate(Parameter = str_replace_all(string = Parameter, pattern = "b_Hydrologic_Setting", repl = ""))

hydrologic_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "HydrologicSetting_Global") %>% 
  select(!NA.) %>% 
  rename(Parameter = Hydrologic_Setting)

hydrologic_data <- merge(hydrologic_coef, merge(hydrologic_mcse, hydrologic_count))

write.xlsx(hydrologic_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "HydrologicSetting_Global", append = TRUE)

#### #### ####

### Hydrologic Setting x Bioregion ###

hydrologic_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "fixef_estimates", "Coefficients.xlsx"), sheetName = "HydrologicSetting_Bioregion") %>% 
  rename(Parameter = NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Hydrologic_Setting"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Bioregion", repl = "")) %>% 
  mutate(Hydrologic_Setting = str_replace_all(string = Hydrologic_Setting, pattern = "Hydrologic_Setting", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-"))

hydrologic_bioregion_mcse <- read.xlsx(here("Output", "BayesianAnalysis", "MCSE", "MCSE_Outputs.xlsx"), sheetName = "HydrologicSetting_Bioregion") %>% 
  select(!NA.) %>% 
  separate(col = "Parameter", into = c("Bioregion", "Hydrologic_Setting"), sep = ":") %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "b_Bioregion", repl = "")) %>% 
  mutate(Hydrologic_Setting = str_replace_all(string = Hydrologic_Setting, pattern = "Hydrologic_Setting", repl = "")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Tropical", repl = "Tropical ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Temperate", repl = "Temperate ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "North", repl = "North ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "Southern", repl = "Southern ")) %>% 
  mutate(Bioregion = str_replace_all(string = Bioregion, pattern = "oM", repl = "o-"))

hydrologic_bioregion_count <- read.xlsx(here("Output", "BayesianAnalysis", "counts", "counts.xlsx"), sheetName = "HydrologicSetting_Bioregion") %>% 
  select(!NA.)

hydrologic_bioregion_data <- merge(hydrologic_bioregion_coef, merge(hydrologic_bioregion_mcse, hydrologic_bioregion_count))

write.xlsx(hydrologic_bioregion_data, here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "HydrologicSetting_Bioregion", append = TRUE)

#### #### ####









#### Pair-wise comparisons.

### Life-History Pairs ### 

life_pairs_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistory_Global") %>% 
  select(!NA.) %>% 
  mutate(Bioregion = "Global")

life_pairs_count <- as.data.frame(life_global %>% summarize(n = n(), cores = sum(cores))) %>% 
  mutate(Bioregion = "Global")
  
life_pairs_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "LifeHistory_Bioregion") %>% 
  select(!NA.)

life_pairs_bioregion_count <- as.data.frame(life_bioregion %>% group_by(Bioregion) %>% summarize(n = n(), cores = sum(cores)))

life_pairs_data <- rbind(merge(life_pairs_coef, life_pairs_count), merge(life_pairs_bioregion_coef, life_pairs_bioregion_count))

write.xlsx(life_pairs_data, here("Output", "Tables", "Pairwise.xlsx"), sheetName = "LifeHistory")

#### #### ####



### Tidal Class Pairs ###

tidal_pairs_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalClass_Global") %>% 
  select(!NA.) %>% 
  mutate(Bioregion = "Global")

tidal_pairs_count <- as.data.frame(tidal_global %>% summarize(n = n(), cores = sum(cores))) %>% 
  mutate(Bioregion = "Global")

tidal_pairs_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "TidalClass_Bioregion") %>% 
  select(!NA.)


tidal_pairs_bioregion_count <- as.data.frame(tidal_bioregion %>% group_by(Bioregion) %>% summarize(n = n(), cores = sum(cores)))

tidal_pairs_data <- rbind(merge(tidal_pairs_coef, tidal_pairs_count), merge(tidal_pairs_bioregion_coef, tidal_pairs_bioregion_count))

write.xlsx(tidal_pairs_data, here("Output", "Tables", "Pairwise.xlsx"), sheetName = "TidalClass", append = TRUE)

#### #### ####



#### Hydrologic Pairs ####

hydrologic_pairs_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Hydrologic_Global") %>% 
  select(!NA.) %>% 
  mutate(Bioregion = "Global")

hydrologic_pairs_count <- as.data.frame(hydrologic_global %>% summarize(n = n(), cores = sum(cores))) %>% 
  mutate(Bioregion = "Global")


hydrologic_pairs_bioregion_coef <- read.xlsx(here("Output", "BayesianAnalysis", "emmeans_pairs", "emmeans_output.xlsx"), sheetName = "Hydrologic_Bioregion") %>% 
  select(!NA.)

hydrologic_pairs_bioregion_count <- as.data.frame(hydrologic_bioregion %>% group_by(Bioregion) %>% summarize(n = n(), cores = sum(cores)))

hydrologic_pairs_data <- rbind(merge(hydrologic_pairs_coef, hydrologic_pairs_count), merge(hydrologic_pairs_bioregion_coef, hydrologic_pairs_bioregion_count))

write.xlsx(hydrologic_pairs_data, here("Output", "Tables", "Pairwise.xlsx"), sheetName = "HydrologicSetting", append = TRUE)

#### #### ####

### Life-History across hydrologic settings ###







