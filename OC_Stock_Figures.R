
### Load Libraries ###

library(dplyr)
library(ggplot2)
library(shadowtext)

#### ####


### Set gg_options ###

gg_options <- function()  theme_bw()+theme(
  panel.grid=element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background= element_blank(),
  legend.box.background=element_blank(),
  legend.key= element_blank(),
  strip.background= element_blank())

#### ####

### Loading the datasets ###

## Estimates
bioregion <- read.xlsx(here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "Global_Bioregion")
species_bioregion <- read.xlsx(here("Output", "Tables", "SedCorgStockEstimates.xlsx"), sheetName = "Species_Bioregion")

## Pair-wise
life_history <- read.xlsx(here("Output", "Tables", "Pairwise.xlsx"), sheetName = "LifeHistory")
tidal_class <- read.xlsx(here("Output", "Tables", "Pairwise.xlsx"), sheetName = "TidalClass")
hydrologic_setting <- read.xlsx(here("Output", "Tables", "Pairwise.xlsx"), sheetName = "HydrologicSetting")

#### CREATING THE FIGURES ####


### Bioregion Sediment OC Stock Estimate ###

bioregion_figure <- bioregion %>% 
  filter(Parameter != "Global") %>%
  ggplot(mapping = aes(x = reorder(Parameter, -Estimate, na.rm = TRUE),
                       Estimate))+
  geom_hline(aes(yintercept = 34.7), linetype = 6, alpha = .6)+
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = 30), alpha = .1)+
  geom_hline(aes(yintercept = 60), alpha = .1)+
  geom_hline(aes(yintercept = 90), alpha = .1)+
  geom_errorbar(aes(ymin = Q10, 
                    ymax = Q90), 
                size = 3.5, width = 0, alpha = 1, col = "#4C5B5C",
                position = position_dodge(width = .65))+
  geom_errorbar(aes(ymin = Q5, 
                    ymax = Q95), 
                size = 2.5, width = 0, alpha = .7, col = "#4C5B5C",
                position = position_dodge(width = .65))+
  geom_errorbar(aes(ymin = Q2.5, 
                    ymax = Q97.5), 
                size = 1.5, width = 0, alpha = .5, col = "#4C5B5C",
                position = position_dodge(width = .65))+
  geom_point(aes(), 
             size = 3, 
             position = position_dodge(width = .65))+
  # geom_text(aes(x = Bioregion, y = mean, label = paste("n = ", N)), size = 5, nudge_x = -.15, angle = 90, color = 'black')+
  # geom_text(aes(x = Bioregion, y = hdi5-3, label = paste("n = ", N)))+
  geom_text(aes(x = Parameter, y = Q95+6, label = sig), size = 4)+ # label = sig
  geom_shadowtext(aes(x = Parameter, y = Q95+11,  label =  n), size = 3.5, col = "black", bg.color = "white")+
  # geom_shadowtext(aes(x = Bioregion, y = hdi95+15,  label = cores), size = 3.5, col = "black", bg.color = "white")+
  gg_options()+
  ylab(expression(paste("Sediment OC Stock (Mg ", ha^{-1}, ")")))+
  xlab("Bioregion")+
  theme(axis.title = element_text(size = 23))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 18))+
  theme(axis.title.y = element_text(vjust = 1.2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())

bioregion_figure

png(here("Output", "Figures", "Bioregion_Sediment_OC_Stock.png"),
    units = "in",
    res = 1200,
    height = 6,
    width = 8)

bioregion_figure

dev.off()

#### ####

### Species x Bioregion Sediment OC Stock Figure


species_bioregion_figure <- ggplot(species_bioregion, mapping = aes(x = reorder(species_bioregion$Species, -Estimate, na.rm = TRUE),
                                                         Estimate, fill = Bioregion))+
  geom_errorbar(aes(ymin = Q10, 
                    ymax = Q90,
                    col = Bioregion), 
                size = 3.5, width = 0, alpha = 1,
                position = position_dodge(width = .8))+
  geom_errorbar(aes(ymin = Q5, 
                    ymax = Q95,
                    col = Bioregion), 
                size = 2.5, width = 0, alpha = .7,
                position = position_dodge(width = .8))+
  geom_errorbar(aes(ymin = Q2.5, 
                    ymax = Q97.5,
                    col = Bioregion), 
                size = 1.5, width = 0, alpha = .5,
                position = position_dodge(width = .8))+
  geom_point(aes(), 
             size = 3, 
             position = position_dodge(width = .8))+
  # geom_text(aes(y = Q97.5+7.5, label = sig), size = 4.5,
  #           position = position_dodge(width = .8))+
  geom_text(aes(y = Q97.5+12, label = n), size = 3.5,
            position = position_dodge(width = .8))+
  gg_options()+
  geom_hline(aes(yintercept = 0))+
  geom_hline(aes(yintercept = 50), alpha = .1)+
  geom_hline(aes(yintercept = 100), alpha = .1)+
  geom_hline(aes(yintercept = 150), alpha = .1)+
  ylab(expression(paste("Sediment OC Stock (Mg ", ha^{-1}, ")")))+
  theme(axis.title = element_text(size = 23))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 18))+
  theme(axis.title.y = element_text(vjust = 1.2))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  scale_color_manual(values = c("#586994", 
                                "#D33F49", 
                                "#F0A202", 
                                "#BFCC94", 
                                "#FF8360", 
                                "#E0ACD5"))

species_bioregion_figure



png(here("Output", "Figures", "Species_Sediment_OC_Stock.png"),
    width = 16,
    height = 8,
    units = "in",
    res = 1200)

species_bioregion_figure

dev.off()

#### ####

### Life-History Figure ### 

life_history_figure <- life_history %>% 
  ggplot() +
  geom_point(aes(Bioregion, estimate), size = 3, col = "#000000")+
  geom_errorbar(aes(x = Bioregion, ymin = lower.HPD, ymax = upper.HPD),
                col = "#80AB82", size = 3.5, width = 0, alpha = 1)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi10, ymax = hdi90, col = var),
  #               col = "#80AB82", size = 2.5, width = 0, alpha = .7)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi5, ymax = hdi95, col = var),
  #               col = "#80AB82", size = 1.5, width = 0, alpha = .5)+
  geom_point(aes(x = reorder(Bioregion, -estimate, na.rm = TRUE), y = estimate), size = 3, col = "#000000")+
  # geom_text(aes(x = Bioregion, y = mean, label = paste(cores)), nudge_x = .2, size = 4)+
  # geom_hline(aes(yintercept = 0))+
  gg_options()+ 
  ylab(expression(paste("Difference in sediment OC stock (Mg ", ha^{-1}, ")")))+
  xlab("Bioregion")+
  theme(legend.position = "none")+
  # theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  # scale_color_manual(values= c("#465775", "#169873", "#984447", "#F6AA1C", "#78C3FB", "#E0ACD5"))+
  theme(axis.title = element_text(size = 22))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.title.y = element_text(vjust = 1.2))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()+
  scale_y_continuous(breaks = round(seq(min(-50), max(150), by = 25),1))+
  geom_hline(aes(yintercept = 0))+
  geom_shadowtext(aes(x = Bioregion, y = estimate, label = paste("n =", n)), nudge_x = .2, size = 4, col = "black", bg.color="white")


life_history_figure

png(here("Output", "Figures", "Life_History_Pairwise.png"),
    units = "in",
    res = 1200,
    height = 6,
    width = 8)

life_history_figure

dev.off()

#### ####

### Tidal Class ###

tidal_class_figure <- tidal_class %>% 
  ggplot() +
  # geom_violin(data = d.bioregion_td, aes(var, Est))+
  geom_point(aes(Bioregion, estimate), size = 3, col = "#000000")+
  geom_errorbar(aes(x = Bioregion, ymin = lower.HPD, ymax = upper.HPD, col = var),
                col = "#AD5D4E", size = 3.5, width = 0, alpha = 1)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi10, ymax = hdi90, col = var),
  #               col = "#AD5D4E", size = 2.5, width = 0, alpha = .7)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi5, ymax = hdi95, col = var),
  #               col = "#AD5D4E", size = 1.5, width = 0, alpha = .5)+
  geom_point(aes(x = reorder(Bioregion, -estimate, na.rm = TRUE), y = estimate), size = 3, col = "#000000")+
  # geom_shadowtext(aes(x = Bioregion, y = estimate, label = paste(cores)), nudge_x = .2, size = 4, col = "black", bg.color="white")+
  # stat_dots(quantiles = 100)+
  gg_options()+
  ylab(expression(paste("Difference in sediment OC stock (Mg ", ha^{-1}, ")")))+
  xlab("Bioregion")+
  theme(legend.position = "none")+
  # theme(axis.text.y = element_text(angle = 45, vjust = 0, hjust=1))+
  # scale_color_manual(values= c("#465775", "#169873", "#984447", "#F6AA1C", "#78C3FB", "#E0ACD5"))+
  theme(axis.title = element_text(size = 22))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.title.y = element_text(vjust = 1.2))+
  # theme(axis.text.y = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()+
  geom_hline(aes(yintercept = 0))+
  geom_shadowtext(aes(x = Bioregion, y = estimate, label = paste("n =", n)), nudge_x = .2, size = 4, col = "black", bg.color="white")


tidal_class_figure


png(here("Output", "Figures", "Tidal_Class_Pairwise.png"),
    units = "in",
    res = 1200,
    height = 6,
    width = 8)

tidal_class_figure

dev.off()

#### ####


### Hydrologic Setting Pairs ###

hydrologic_setting_figure <- hydrologic_setting %>% 
  ggplot() +
  geom_point(aes(Bioregion, estimate), size = 3, col = "#000000")+
  geom_errorbar(aes(x = Bioregion, ymin = lower.HPD, ymax = upper.HPD),
                col = "#D74E09", size = 3.5, width = 0, alpha = 1)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi10, ymax = hdi90),
  #               col = "#D74E09", size = 2.5, width = 0, alpha = .7)+
  # geom_errorbar(aes(x = Bioregion, ymin = hdi5, ymax = hdi95),
  #               col = "#D74E09", size = 1.5, width = 0, alpha = .5)+
  geom_point(aes(x = reorder(Bioregion, -estimate, na.rm = TRUE), y = estimate), size = 3, col = "#000000")+
  # geom_text(aes(x = Bioregion, y = mean, label = paste(cores)), nudge_x = .2, size = 4)+
  gg_options()+ 
  ylab(expression(paste("Difference in sediment OC stock (Mg ", ha^{-1}, ")")))+
  xlab("Bioregion")+
  theme(legend.position = "none")+
  theme(axis.title = element_text(size = 22))+
  theme(axis.text = element_text(size = 12))+
  theme(legend.text = element_text(size = 11))+
  theme(legend.title = element_text(size = 12))+
  theme(axis.title.y = element_text(vjust = 1.2))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  coord_flip()+
  geom_hline(aes(yintercept = 0))+
  geom_shadowtext(aes(x = Bioregion, y = estimate, label = paste("n =", n)), nudge_x = .2, size = 4, col = "black", bg.color="white")

hydrologic_setting_figure


png(here("Output", "Figures", "Hydrologic_Setting_Pairwise.png"), 
    units = "in",
    res = 1200,
    height = 6,
    width = 8)

hydrologic_setting_figure

dev.off()

#### ####

### Life X Hydrologic Figure -- How to go about this??? ###


















