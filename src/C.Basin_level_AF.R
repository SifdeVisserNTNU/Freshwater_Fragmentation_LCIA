# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

"
This file creates averages the allocation factors per basin.
"

### Libraries  ----
library(sf); library(dplyr); library(stringr); library(ggplot2); library(tidyr);
library(patchwork); library(scales)

### Paths  ----
input <- "../data/input"
interim <- "../data/interim"
output <- "../data/output"

### Load datasets  ----
Dams <- readRDS(file.path(interim, "Allocation_Factors.rds")) # 24993
GDAT_cleaned <- readRDS(file.path(interim, "GDAT_cleaned.rds"))
AF_all <- readRDS(file.path(interim, "AF_all.rds"))
Main_Basins <- st_read(file.path(input, "GDAT_MainBasins.gpkg")) # 58811

### Spatial intersection ----

sf_use_s2(FALSE) # 0 NAs
joined_data <- st_join(Dams, Main_Basins, join = st_intersects) # 24993

### Calculating basin level AF ----

## Summarizing AF_dam to AF_basin ----
AF_Basin <- as.data.frame(joined_data) %>%
  filter(!is.na(MAIN_BAS)) %>% # 22733
  group_by(MAIN_BAS) %>%
  # add parameter to identify how many dams contributed to a certain purpose' share
  summarise(AF_Irrig_gt_0 = sum(!AF_Irrig == 0, na.rm = TRUE),
            AF_Hydro_gt_0 = sum(!AF_Hydro == 0, na.rm = TRUE),
            AF_Other_gt_0 = sum(!AF_Other == 0, na.rm = TRUE),
            n = n(),
            across(all_of(AF_all), mean, na.rm = TRUE)) # 1900
           
# Adding polygons of main basins
AF_per_basin <- left_join(x=Main_Basins, y=AF_Basin, by = "MAIN_BAS") %>% # 58811
  select(-mb_area, -X_errors, -layer, -path)

filter_mismatches <- joined_data %>% filter(!st_is_empty(geometry) & is.na(MAIN_BAS)) # 63

### Checking differences dam-level and basin-level ----

# Summaries 
lapply(Dams[, AF_all], summary)
lapply(AF_per_basin[, AF_all], summary)

# Finding explanations for difference basin and dam level
# How many AF > 0 does each category have?
# Dam
count_higher_than_0_irrig <- sum(Dams$AF_Irrig > 0, na.rm = TRUE) # 10433
count_higher_than_0_hydro <- sum(Dams$AF_Hydro > 0, na.rm = TRUE) # 7742
count_higher_than_0_other <- sum(Dams$AF_Other > 0, na.rm = TRUE) # 10450
# Basin
count_higher_than_0_irrig_basin <- sum(AF_per_basin$AF_Irrig > 0, na.rm = TRUE) # 811
count_higher_than_0_hydro_basin <- sum(AF_per_basin$AF_Hydro > 0, na.rm = TRUE) # 1052
count_higher_than_0_other_basin <- sum(AF_per_basin$AF_Other > 0, na.rm = TRUE) # 975
# There are more irrigation shares higher than > 0 in dam-level analysis than basin-level analysis

sum(AF_Basin$AF_Irrig_gt_0) # Irrigation wider spread
sum(AF_Basin$AF_Hydro_gt_0)
sum(AF_Basin$AF_Irrig)
sum(AF_Basin$AF_Hydro) # Higher sum for hydro

# Clean dataframe
AF_per_basin <- AF_per_basin %>% select(-c(frag_cur, AF_Irrig_gt_0, AF_Hydro_gt_0, AF_Other_gt_0))
suppl_AF <- AF_per_basin %>% filter(!is.na(AF_Irrig) | !is.na(AF_Hydro) | !is.na(AF_Other))

# Plot number of AFs > 0 summarized into basins
num_per_basin <- ggplot(AF_Basin, aes(x = factor(variable), y = value)) +
  geom_point() +
  labs(x = "Category", y = "Value")

AF_Basin_melted <- AF_Basin %>% rename(Hydropower = AF_Hydro_gt_0,
                                       Irrigation = AF_Irrig_gt_0,
                                       Other = AF_Other_gt_0) %>%
  gather(key = "Purpose", value = "Number_contributing", Hydropower, Irrigation, Other)

breaks <- c(0,10, 100, 1000)
y_limit <- c(0, 3000)

Fig_B10 <- ggplot(data = AF_Basin_melted, aes(x = Purpose, y = Number_contributing, color = Purpose)) +
  geom_point() +
  scale_color_viridis_d(option = "G", begin = 0.2, end = 0.9) +
  labs(y = "Count per basin") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), breaks=breaks, limits = y_limit) +
  theme_light() +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major.y = element_line(color = "gray"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin=unit(c(1,0,0,0),units='cm'),
        axis.ticks.x = element_blank()) +
  guides(color="none") +
  annotation_logticks(sides="l")

### Number of observations ----
obs_basins <- nrow(Main_Basins) # 58811
# Joined data where MAIN_BAS is identified
obs_nomainbas <- nrow(as.data.frame(joined_data) %>% filter(!is.na(MAIN_BAS))) # 22733
# ... empty geometries for dams
empties <- sum(st_is_empty(Dams)) # 2197
# ... mismatch basin and dam (which has geometry) # 63
mismatches <- sum(!st_is_empty(joined_data) & is.na(joined_data$MAIN_BAS))
# After summarizing dam-results per basin
obs_sumperbasin <- nrow(AF_Basin) # 1900

### Violinplots AFs per dam and per basin ----
AF_dam <- Dams %>% select("AF_Hydro", "AF_Irrig", "AF_Other") %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c("AF_Hydro", "AF_Irrig", "AF_Other"), 
                                     names_to = "Purp",
                                     values_to = "AF") %>%
  mutate(Purp = str_extract(Purp, "Hydro|Irrig|Other") %>%
           recode("Hydro" = "Hydropower", "Irrig" = "Irrigation"),
         Resolution = "Dam")

AF_basin <- AF_Basin %>% select("AF_Hydro", "AF_Irrig", "AF_Other") %>%
  pivot_longer(cols = c("AF_Hydro", "AF_Irrig", "AF_Other"), 
                                           names_to = "Purp",
                                           values_to = "AF") %>%
  mutate(Purp = str_extract(Purp, "Hydro|Irrig|Other") %>%
           recode("Hydro" = "Hydropower", "Irrig" = "Irrigation"),
         Resolution = "Basin")

AF_dam_basin <- rbind.data.frame(AF_dam, AF_basin)

Fig_B9 <- ggplot(AF_dam_basin, aes(x = Purp, y = AF, fill = Purp)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), color = "grey") +
  scale_fill_viridis_d(option = "G", direction = -1, begin = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  facet_grid(Resolution~Purp, scales = "free") +
  labs(y = "AF [-]") +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(size = 10),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.5))
  guides(fill="none") +
  scale_fill_viridis_d(option = "G", direction = -1, begin = 0.2, end = 0.8) 

### Save files ----
saveRDS(AF_per_basin, file.path(output, "AF_per_basin.rds"))
write_xlsx(suppl_AF, file.path(output, "AF_per_basin.xlsx"))
ggsave(path = "../visualisations/", filename = "Fig_B9.png", Fig_B9, dpi = 1000, 
       width = 7480, height =  3937, unit = "px")
ggsave(path = "../visualisations/", filename = "Fig_B10.png", Fig_B10, dpi = 1000, 
       width = 3534, height = 3534 , unit = "px")
