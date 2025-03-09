
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

## Add genus and species size to the dataset.



data_Species <- data_Import %>% filter()

## Number of each species, genus, and size

View(data_Import[!grepl("[,.]", data_Import$Species), ] %>% group_by(Species) %>% filter(Genus != "undefined" & Genus != "Multiple") %>% summarize(n = n()))
View(data_Import %>% group_by(Genus) %>% summarize(n = n()))
View(data_Import %>% group_by(Life_History) %>% summarize(n = n()))



## Make tables for each category of species number and if they're defined.

data_SingleSpecies <- data_Species %>% filter(Species != "" & Species != "Mixed" & Species != "Multiple")
data_SingleSpecies <- data_SingleSpecies[!grepl("[,.]", data_SingleSpecies$Species), ]

data_MultipleDefined <- data_Species %>% filter(Species != "" & Species != "Mixed" & Species != "Multiple")
data_MultipleDefined <- data_MultipleDefined[grepl("[,.]", data_MultipleDefined$Species), ]

data_MultipleUndefined <- data_Species %>% filter(Species == "Mixed" | Species == "Multiple")

data_SpeciesUndefined <- data_Species %>% filter(Species == "")

## Add a SpeciesSize column









dat_SingleSpecies <- dat %>% filter(Species != "" & Species != "Mixed" & Species != "Multiple") # Remove the rows with no species reported.

dat_SingleSpecies <- dat_SingleSpecies[!grepl(",", dat_SingleSpecies$Species), ]

dat_SingleSpecies$Genus <- word(dat_SingleSpecies$Species, 1)

dat_SingleSpecies <- dat_SingleSpecies[!grepl("spp", dat_SingleSpecies$Species), ]

dat_SingleSpecies$Life_History <- ifelse(dat_SingleSpecies$Genus == "Posidonia" | dat_SingleSpecies$Genus == "Thalassia" | dat_SingleSpecies$Genus == "Enhalus" | dat_SingleSpecies$Genus == "Thalassodendron" | dat_SingleSpecies$Genus == "Amphibolis", "Large", "Small")

View(dat_SingleSpecies %>% group_by(Species) %>% summarize(n = n()))
View(dat_SingleSpecies %>% group_by(Genus) %>% summarize(n = n()))
View(dat_SingleSpecies %>% group_by(Life_History) %>% summarize(n = n()))











#### The below code adds and modifies columns to prepare the dataset for analysis. It will be saved in the 
   # project folder as the analysis datset.

  ## Needed changes:
    # Add a "paper" column. Used for random effect.
    # Add a "SpeciesSize" column. Used as a predictor later.
    # Add a "SpeciesNumber" column. Used for easy filtering later in analysis. (NA = "NA", punctuation or "multiple" = "Multiple", else = "Single")










