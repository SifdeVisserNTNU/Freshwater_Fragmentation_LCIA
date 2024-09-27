# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

" This file creates plots for the supplementary information:
- bar chart with the top 5 highest values for hydropower and irrigation basin-level CFs
- map which indicates for each basin hich factors are a missing value
- map with basin-level fate factors
- map with basin-level effect factors
- map with basin-level global extinction probabilities
- regression of basin-level characterization factors vs the size of basins
- map with absolute species loss per basin
"

# Libraries ----
library(ggplot2); library(sf); library(dplyr); library(patchwork); 
library(scales); library(tidyr)

# Paths ----
input <- "../data/input"
output <- "../data/output"
source("E.Plot_settings.R")

# Load datasets ----
CFs <- readRDS(file.path(output, "CFs.rds"))
nondiadromous <- readRDS(file.path(input, "mbres_fwonly.rds"))
Contribution <- left_join(CFs, select(nondiadromous, MAIN_BAS, mb_area, occ, 
                                      PAFspec_cur), by = "MAIN_BAS")

### Top 5 min max CFs ----

# Slice dataset
Max_hydro <- Contribution %>% arrange(desc(CF_hydro_glo)) %>%
  slice_head(n=5)
Max_irrig <- Contribution %>% arrange(desc(CF_irrig_glo)) %>%
  slice_head(n=5)

# Maximum hydro inset
Df_maxhydro <- Max_hydro %>% 
  st_drop_geometry() %>%
  select('FF_hydro', 'EF', 'AF_Hydro', "GEP", "CF_hydro_glo") %>%
  rename(FF = FF_hydro, AF = AF_Hydro, CF = CF_hydro_glo) %>%
  pivot_longer(cols = c("FF", "EF", "AF", "GEP", "CF"), 
               names_to = "Type",
               values_to = "Value") %>%
  mutate(Basin = factor(rep(1:5, each = 5)),
         Stat = rep("Max", 25),
         Purp = rep("Hydropower", 25))

# Maximum irrig inset
Df_maxirrig <- Max_irrig %>% 
  st_drop_geometry() %>%
  select('FF_irrig', 'EF', 'AF_Irrig', "GEP", "CF_irrig_glo") %>%
  rename(FF = FF_irrig, AF = AF_Irrig, CF = CF_irrig_glo) %>%
  pivot_longer(cols = c("FF", "EF", "AF", "GEP", "CF"), 
               names_to = "Type",
               values_to = "Value") %>%
  mutate(Basin = factor(rep(1:5, each = 5)),
         Stat = rep("Max", 25),
         Purp = rep("Irrigation", 25))

# Combine 
DF_all <- rbind(Df_maxhydro, Df_maxirrig) %>%
  mutate(Type = factor(Type, levels = c("CF", "FF", "EF", "AF", "GEP")))

### Bar chart with 5 max values for irrig and hydro
nbreaks <- 16
breaks <- c(0e+00, 1e-14, 1e-12, 1e-10, 1e-08, 1e-06, 1e-04, 1e-02, 1e+00)

Fig_B2 <- ggplot(DF_all, aes(x=Basin, y=Value, fill=Type))+
  geom_bar(stat='identity', position = "dodge") +
  scale_y_continuous(trans = pseudo_log_trans(sigma = 10^(-nbreaks), base = 10),
                     breaks = breaks) +
  scale_fill_viridis_d(option = "G", begin = 0.2, end = 0.8) +
  facet_grid(~ Purp) +
  theme_light() +
  theme(plot.title = element_text(size = 10),       
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
        text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0, 0, 0, 0), units = 'cm'))

### Missing values -----
# For global CFs
NA_Hydropower <- ifelse(is.na(CFs$FF_hydro) & !is.na(CFs$EF) & !is.na(CFs$AF_Hydro) & !is.na(CFs$GEP), "FF",
                        ifelse(!is.na(CFs$FF_hydro) & is.na(CFs$EF) & !is.na(CFs$AF_Hydro) & !is.na(CFs$GEP), "EF",
                               ifelse(!is.na(CFs$FF_hydro) & !is.na(CFs$EF) & is.na(CFs$AF_Hydro) & !is.na(CFs$GEP), "AF",
                                      ifelse(!is.na(CFs$FF_hydro) & !is.na(CFs$EF) & !is.na(CFs$AF_Hydro) & is.na(CFs$GEP), "GEP",
                                             ifelse(!is.na(CFs$FF_hydro) & !is.na(CFs$EF) & !is.na(CFs$AF_Hydro) & !is.na(CFs$GEP),"Complete", "Combination")))))

NA_Irrigation <- ifelse(is.na(CFs$FF_irrig) & !is.na(CFs$EF) & !is.na(CFs$AF_Irrig) & !is.na(CFs$GEP), "FF",
                        ifelse(!is.na(CFs$FF_irrig) & is.na(CFs$EF) & !is.na(CFs$AF_Irrig) & !is.na(CFs$GEP), "EF",
                               ifelse(!is.na(CFs$FF_irrig) & !is.na(CFs$EF) & is.na(CFs$AF_Irrig) & !is.na(CFs$GEP), "AF",
                                      ifelse(!is.na(CFs$FF_irrig) & !is.na(CFs$EF) & !is.na(CFs$AF_Irrig) & is.na(CFs$GEP), "GEP",
                                             ifelse(!is.na(CFs$FF_irrig) & !is.na(CFs$EF) & !is.na(CFs$AF_Irrig) & !is.na(CFs$GEP),"Complete", "Combination")))))

Missing_data <- data.frame(geom = rep(CFs$geom, 2),
                           Missing = c(NA_Hydropower, NA_Irrigation),
                           Purp = c(rep("Hydropower", 58811), rep("Irrigation", 58811)))

Fig_B3 <- ggplot() +
  geom_sf(data = Missing_data, aes(geometry = geometry, fill = Missing), colour = NA) +
  custom_theme("bottom", "horizontal") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 10)) +
  scale_fill_manual(values = c("Combination" = "#342346FF", 
                               "Complete" = "grey", 
                               "EF" = "#357BA2FF",
                               "FF" = "#78D6AEFF")) +
  facet_grid(~Purp)
### FFs ----
FFs_wrap <- CFs %>% select("MAIN_BAS", "FF_hydro", "FF_irrig") %>%
  pivot_longer(cols = c("FF_hydro", "FF_irrig"), 
               names_to = "Purp",
               values_to = "FF") %>%
  mutate(Purp = str_extract(Purp, "hydro|irrig") %>%
           recode("hydro" = "Hydropower", "irrig" = "Irrigation"))

nbreaks_FF = 14
breaks_FF <- c(0e+00, 1e-12, 1e-10, 1e-08, 1e-06, 1e-04, 1e-02, 1e+00, 1e+02)
labels_FF <- c("0","1e-12", "1e-10", "1e-08", "1e-06", "1e-04", "1e-02",
               "1e+00", "1e+02") # to have "0" in legend
limit_FF <- 1e+02
title_FF = expression(paste("FF [m"^{-3}, "\u00B7yr]"))

Fig_B4 <- ggplot() +
  geom_sf(data = FFs_wrap, aes(geometry = geom, fill = FF),
          colour = NA, lwd = 0) +
  facet_grid(~Purp) +
  custom_fill_log(nbreaks_FF, breaks_FF, labels_FF, limit_FF, title_FF, 14, 0.4) +
  custom_theme("bottom", "horizontal")

### EF -----
Fig_B5 <- ggplot() +
  geom_sf(data = CFs, aes(geometry = geom, fill = EF), 
          colour = NA, lwd = 0) +
  custom_theme("right", "vertical") +
  custom_fill_lin("EF [PDF]", 0.4, 3)

### GEP ----
nbreaks_GEP <- 12
breaks_GEP <- c(0e+00, 1e-10, 1e-08, 1e-06, 1e-04, 1e-02, 1e+00)
labels_GEP <- c("0e+00", "1e-10", "1e-08", "1e-06", "1e-04", "1e-02", "1e+00")
limit_GEP <- 1e+00

Fig_B6 <- ggplot() +
  geom_sf(data = CFs, aes(geometry = geom, fill = EF), 
          colour = NA, lwd = 0) +
  custom_theme("right", "vertical") +
  custom_fill_log(nbreaks_GEP, breaks_GEP, labels_GEP, limit_GEP, "GEP [-]", 
                  0.4, 3)

### CFs vs size of basins ----
Area_wrap <- Contribution %>% select("MAIN_BAS", "CF_hydro_reg", "CF_irrig_reg",
                                     "CF_hydro_glo", "CF_irrig_glo", "mb_area") %>%
  pivot_longer(cols = c("CF_hydro_reg","CF_irrig_reg", "CF_hydro_glo", "CF_irrig_glo"), 
               names_to = "Type",
               values_to = "CF") %>%
  mutate(Purp = str_extract(Type, "hydro|irrig") %>%
           recode("hydro" = "Hydropower", "irrig" = "Irrigation"),
         Ext_risk = str_extract(Type, "reg|glo") %>%
           recode("reg" = "Regional", "glo" = "Global")) %>%
  filter(!is.na(CF), CF > 0)

Area_wrap_plot <- ggplot(data = Area_wrap, aes(x = mb_area, y = CF)) +
  geom_point(color = "#357BA2FF", size = 0.5) +
  scale_x_log10(breaks = c(1e+01, 1e+02, 1e+03, 1e+04, 1e+05, 1e+06), 
                labels = label_scientific()) +
  scale_y_log10(breaks = c(1e-20, 1e-18, 1e-16, 1e-14, 1e-12, 1e-10, 1e-08, 1e-06, 1e-04, 1e-02, 1e+00), 
                labels = label_scientific()) +
  facet_grid(Ext_risk ~ Purp, scales = "free") +
  xlab(expression(paste("Basin area [km"^{2},"]"))) +
  ylab(expression(paste("CF [PDF\u00B7yr\u00B7m"^{-3}, "]"))) +
  theme_light() +
  theme(plot.title = element_text(size = 10),
        text = element_text(size = 10),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), units = 'cm'))

Fig_B7 <- Area_wrap_plot + geom_smooth(method = 'lm', se = FALSE, color = "black")

# R2 values
regression_hydro <- Contribution %>%
  st_drop_geometry() %>%
  select("mb_area", "CF_hydro_reg") %>%
  filter(!is.na(CF_hydro_reg), CF_hydro_reg > 0)
rsq_hydro <- summary(lm(log(mb_area)~log(CF_hydro_reg), data= regression_hydro))$r.squared

regression_irrig <- Contribution %>%
  st_drop_geometry() %>%
  select("mb_area", "CF_irrig_reg") %>%
  filter(!is.na(CF_irrig_reg), CF_irrig_reg > 0)
rsq_irrig <- summary(lm(log(mb_area)~log(CF_irrig_reg), data= regression_irrig))$r.squared

### Absolute species loss ----
Species_loss <- Contribution %>%
  mutate(Loss = PAFspec_cur * occ,
         Loss_per_m2 = Loss / mb_area)

Fig_B8 <- ggplot() +
  geom_sf(data = Species_loss, aes(geometry = geom, fill = Loss), colour = NA, lwd = 0) +
  custom_theme("right", "vertical") +
  custom_fill_lin("Species loss [-]", 0.4, 3)

### Save files ----
ggsave(path = "../visualisations/", filename = "Fig_B2.png", Fig_B2, dpi = 1000,
       width = 5512, height = 2362 , unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B3.png", Fig_B3, dpi = 1000,
       width = 7480, height = 2756 , unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B4.png", Fig_B4, dpi = 1000,
       width = 7480, height = 2756, unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B5.png", Fig_B5, dpi = 1000, 
       width = 4724, height = 1969, unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B6.png", Fig_B6, dpi = 1000, 
       width = 4724, height = 1969, unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B7.png", Fig_B7, dpi = 1000, 
       width = 7480, height = 3937, unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B8.png", Fig_B8, dpi = 1000, 
       width = 5118, height = 1969, unit = "px")
