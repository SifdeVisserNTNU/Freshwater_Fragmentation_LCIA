# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

"This file is for the case study application."

# Libraries ----
library(ggplot2)

## Fragmentation ----
turbined_water_glo <- 8.1e-1 # m3 RoW Ecoinvent
turbined_water_no <- 8.1e-1 # m3, Ecoinvent
CF_fragmentation_glo <- 3.89e-15
CF_fragmentation_no <- 1.0195329359071E-14
IS_fragmentation_glo <- turbined_water_glo * CF_fragmentation_glo
IS_fragmentation_no <- turbined_water_no * CF_fragmentation_no

## Water consumption ----
water_consumed_glo <- 0.029221678 # Ecoinvent, water air, unspecified
water_consumed_no <- 0.029221678 # Ecoinvent, water air, unspecified
#0.0016 # Dorber et al. (2019) 
# CFs from Pierrat et al. (2023) - CF_GLOB_A_m (for Norway without outliers)
CF_water_consumption_glo <-  4.39787936616676E-12 # PDF yr / m3
CF_water_consumption_no <- 1.94E-14 # PDF yr / m3
IS_water_consumption_glo <- water_consumed_glo * CF_water_consumption_glo
IS_water_consumption_no <- water_consumed_no * CF_water_consumption_no

## Global warming ----
# foreground data
#CO2eq_glo <- 0.007337042204306785# from Recipe 2016 v1.03, midpoint (H)      # 8.1569e-2	 for tropical reservoirs
CH4_glo <- 0.000014
CH4_no <- 0.000014
# 0.006649472621858932 # from Recipe 2016 v1.03, midpoint (H)
# CFs from De Visser et al. (2023), average, 100 yr, only global available
#CF_CO2_glo <- 1.49e-15 # PDF yr / kg  
CF_CH4_glo <- 0.0000000000000417 # PDF yr / kg  

IS_global_warming_CH4_glo <- CH4_glo * CF_CH4_glo
IS_global_warming_CH4_no <- CH4_no * CF_CH4_glo # Note: only global factor available

#IS_global_warming_CO2eq_glo <- CO2eq_glo * CF_CO2_glo
#IS_global_warming_CO2eq_no <- CO2eq_no * CF_CO2_glo # Note: only global factor available

### Change to CH4 rese

## Land inundation ----
land_inundated_glo <- 3.4500e-3 # m2*year, from Ecoinvent
land_inundated_no <- 3.4500e-3 # m2*year, from Ecoinvent
CF_land_inundation_glo <- 2.10e-15 # weighted average by area ecoregion from natural_to_water, Dorber et al. 2020
CF_land_inundation_no <- 4.646082E-16 # weighted average by area ecoregions in Norway natural_to_water
IS_land_inundation_glo <- land_inundated_glo * CF_land_inundation_glo
IS_land_inundation_no <- land_inundated_no * CF_land_inundation_no

## Barchart ----
Category <- rep(c("Global warming", "Land inundation", "Fragmentation", "Water consumption"),2)
Impact_scores <- c(IS_global_warming_CH4_glo, IS_land_inundation_glo, IS_fragmentation_glo, IS_water_consumption_glo,
                   IS_global_warming_CH4_no, IS_land_inundation_no, IS_fragmentation_no, IS_water_consumption_no)
Resolution <- c(rep("Global",4), rep("Norway",4))
Colour <- c(rep(c("No", "No", "Yes", "No"),2))
case_study <- data.frame(Category, Impact_scores, Resolution, Colour)

Fig_4 <-ggplot(case_study, aes(x = reorder(Category, Impact_scores), y = Impact_scores, fill = Colour)) +
  geom_bar(stat="identity") +
  facet_grid(~Resolution, scales="free") +
  scale_y_continuous(trans=scales::pseudo_log_trans(1e-20,10),
                     limits = c(0, 1e-12),
                     breaks = c(0e+00, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14, 1e-13, 1e-12)) +
  ylab(expression(paste("Impact scores [PDF\u00B7yr\u00B7",kWh^-1,"]"))) +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 10),
        legend.text= element_text(size=10),
        plot.margin=unit(c(0,0,0,0),units='cm'),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_line(),  
        axis.ticks.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  scale_fill_viridis_d(option = "G", begin = 0.2, end = 0.8, guide = "none")

### Save files ----
ggsave(path = "../visualisations", filename = "Fig_4.png", Fig_4, dpi = 600, 
       width = 15.92, height = 7, unit = "cm")
