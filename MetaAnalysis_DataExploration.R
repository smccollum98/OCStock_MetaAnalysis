
library(here)
library(dplyr)
library(stringr)

#### This code will be for the initial exploration of the dataset.

##Questions to answer:
    ## How many instances of each variable are reported?
    ## How many times is each species reported?
    ## What size are the datasets for interactive effect? What about at each bioregion level?

## Import data.

data_Import <- read.csv(here("Data", "MetaAnalysis_SedCorgStockData.csv"))


## Number of each species (Single species meadows), genus, and size 

head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Genus != "undefined" & Genus != "Multiple") %>% summarize(n = n(), cores = sum(cores)))
View(data_Import[!grepl("[,.]", data_Import$Species), ] %>% group_by(Species) %>% filter(Genus != "undefined" & Genus != "Multiple") %>% summarize(n = n(), cores = sum(cores)))
head(data_Import %>% group_by(Genus) %>% summarize(n = n()))
head(data_Import %>% group_by(Life_History) %>% summarize(n = n()))

## Species with fewer than 3 observations
    ## Halodule emarginata, Halophila Baillonii, Zostera Caulescens, Zostera chilensis

## Number of each bioregion

head(data_Import %>% group_by(Bioregion) %>% summarize(n = n()))

## Number of Hydrologic setting and Tidal class

head(data_Import %>% group_by(Hydrologic_Setting) %>% summarize(n = n()))
head(data_Import %>% group_by(Tidal_Class) %>% summarize(n = n()))

## Number of each predictor within each bioregion

head(data_Import %>% filter(Hydrologic_Setting != "") %>% group_by(Bioregion, Hydrologic_Setting) %>% summarize(n = n()))
head(data_Import %>% filter(Tidal_Class != "") %>% group_by(Bioregion, Tidal_Class) %>% summarize(n = n()))
head(data_Import %>% filter(Life_History != "undefined" & Life_History != "multiple") %>% group_by(Bioregion, Life_History) %>% summarize(n = n()))
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% group_by(Bioregion, Species) %>% filter(Genus != "undefined" & Genus != "Multiple") %>% summarize(n = n()))

## Number of observations containing all predictors (excluding species identity)

head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Tidal_Class != "" & Hydrologic_Setting != "") %>% summarize(n = n()))
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Tidal_Class != "" & Hydrologic_Setting != "") %>% group_by(Bioregion) %>% summarize(n = n()))

## Number of observations with both Tidal Class and Geomorphology

# Global
head(data_Import %>% filter(Hydrologic_Setting != "" & Tidal_Class != "") %>% summarize(n = n()))

# By Bioregion
head(data_Import %>% filter(Hydrologic_Setting != "" & Tidal_Class != "") %>% group_by(Bioregion) %>% summarize(n = n()))


## Number of observations with both Tidal Class and Species Life-History

# Global
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Tidal_Class != "") %>% summarize(n = n()))

# By Bioregion
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Tidal_Class != "") %>% group_by(Bioregion) %>% summarize(n = n()))

## Number of observations with both Hydrologic Setting and Species Life-History

# Global
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Hydrologic_Setting != "") %>% summarize(n = n()))

# By Bioregion
head(data_Import[!grepl("[,.]", data_Import$Species), ] %>% filter(Species != "Multiple" & Species != "undefined" & Genus != "Multiple" & Hydrologic_Setting != "") %>% group_by(Bioregion) %>% summarize(n = n()))










#### The below code adds and modifies columns to prepare the dataset for analysis. It will be saved in the 
   # project folder as the analysis datset.

  ## Needed changes:
    # Add a "paper" column. Used for random effect.
    # Give "OMIT" value to each row that needs to be omitted.
      # e.g. insufficient 


## Add publication column for the random effect input.
data_Export <- data_Import %>% 
  group_by(Citation) %>% 
  mutate(PAPER = cur_group_id()) %>% 
  ungroup()







