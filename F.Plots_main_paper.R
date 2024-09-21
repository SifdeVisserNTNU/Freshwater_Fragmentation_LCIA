# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

" This file creates the plots for the main paper:
- violing plot of characterization factors
- map of characterization factors
- map of allocation facors
"

# Libraries ----
library(ggplot2); library(sf); library(dplyr); library(patchwork); 
library(scales); library(stringr); library(tidyr)  

# Paths ----
output <- "../data/output"
source("E.Plot_settings.R")

# Load datasets ----
CFs <- readRDS(file.path(output, "CFs.rds"))

### Violin plot -----

Type <- c(rep("Hydropower reg", 58811), rep("Hydropower glo", 58811), 
          rep("Irrigation reg", 58811), rep("Irrigation glo", 58811))
CF <- c(CFs$CF_hydro_reg, CFs$CF_hydro_glo, CFs$CF_irrig_reg, CFs$CF_irrig_glo)

violin_data <- data.frame(Type, CF) %>%
  filter(!is.na(CF)) %>%
  arrange(Type, CF, ascending = TRUE) %>%  # Sort the data by Type and CF
  mutate(Purp = str_extract(Type, "Hydropower|Irrigation"),
         Ext_risk = str_extract(Type, "glo|reg") %>%
           recode("reg" = "Regional", "glo" = "Global"))

# Calculate the mean
mean_CF <- violin_data %>% 
  group_by(Type) %>% 
  summarise(mean_CF = mean(CF, na.rm = TRUE)) %>%
  mutate(Purp = c(rep("Hydropower", 2), rep("Irrigation", 2)),
         Ext_risk = c(rep(c("Global", "Regional"),2)))

nbreaks <- 19 # very important!! number closest to zero
breaks <- c(0, 1e-18, 1e-16, 1e-14, 1e-12, 1e-10, 1e-08, 1e-06, 1e-04, 
            1e-02)

Fig_1 <- ggplot(violin_data, aes(x = Ext_risk, y = CF, fill = Purp), 
                      colour = NA, lwd = 0) +
  geom_violin() +
  geom_boxplot(width=0.05, colour = "black", fill = "grey90") +
  geom_point(data = mean_CF, aes(x = Ext_risk, y = mean_CF, fill = Purp), shape=23, size=2, colour = "red", fill = "red") +
  ylab(expression(paste("CF [PDF\u00B7yr\u00B7m"^{-3}, "]"))) +  
  scale_y_continuous(trans = pseudo_log_trans(sigma = 10^(-nbreaks), base = 10), 
                     breaks = breaks) +
  scale_fill_viridis_d(option = "G", direction = -1, begin = 0.2) +
  scale_x_discrete(labels = c("Global", "Regional")) +
  facet_grid(~ Purp) +
  theme_light()
  theme(plot.title = element_text(size = 10),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
        text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0, 0, 0, 0), units = 'cm'))

### CFs ----
CFs_wrap <- CFs %>% select("MAIN_BAS", "CF_hydro_reg", "CF_irrig_reg", 
                           "CF_hydro_glo", "CF_irrig_glo") %>%
  pivot_longer(cols = c("CF_hydro_reg", "CF_irrig_reg", "CF_hydro_glo",
                        "CF_irrig_glo"), 
                     names_to = "Type",
                     values_to = "CF") %>%
  mutate(Ext_risk = str_extract(Type, "reg|glo") %>%
           recode("reg" = "Regional", "glo" = "Global"),
         Purp = str_extract(Type, "hydro|irrig") %>%
           recode("hydro" = "Hydropower", "irrig" = "Irrigation"))

nbreaks_CF <- 19
breaks_CF <- c(0, 1e-18, 1e-16, 1e-14, 1e-12, 1e-10, 1e-08, 1e-06, 1e-04, 
               1e-02)
labels_CF <- c("0", "1e-18", "1e-16", "1e-14", "1e-12", "1e-10", "1e-08", 
               "1e-06", "1e-04", "1e-02")
limit_CF <- 1e-01
title_CF = expression(paste("CF [PDF\u00B7yr\u00B7m"^{-3}, "]"))

Fig_2 <- ggplot() +
  geom_sf(data = CFs_wrap, aes(geometry = geom, fill = CF),
          colour = NA, lwd = 0) +
  facet_grid(Ext_risk ~ Purp) +
  custom_fill_log(nbreaks_CF, breaks_CF, labels_CF, limit_CF, title_CF, 14, 0.4) +
  custom_theme("bottom", "horizontal")

### AF ----
AFs_wrap <- CFs %>% select("MAIN_BAS", "AF_Hydro", "AF_Irrig", "AF_Other") %>%
  pivot_longer(cols = c("AF_Hydro", "AF_Irrig", "AF_Other"), 
               names_to = "Type",
               values_to = "AF") %>%
  mutate(Purp = str_extract(Type, "Hydro|Irrig|Other") %>%
           recode("Hydro" = "Hydropower", "Irrig" = "Irrigation"))

Fig_3 <- ggplot() +
  geom_sf(data = AFs_wrap, aes(geometry = geom, fill = AF),
          colour = NA, lwd = 0) +
  facet_grid(Purp~.) +
  custom_fill_lin("AF [dimensionless]", 8, 0.4) +
  custom_theme("bottom", "horizontal")

### Save plots ----
ggsave(path = "../visualisations/", filename = "Fig_1.png", Fig_1, dpi = 600,
       width = 10, height = 6 , unit = "cm")
ggsave(path = "../visualisations/", filename = "Fig_2.png", Fig_2, dpi = 600, 
       width = 15.92, height = 10 , unit = "cm")
ggsave(path = "../visualisations/", filename = "Fig_3.png", Fig_3, dpi = 600, 
       width = 10, height = 15 , unit = "cm")
