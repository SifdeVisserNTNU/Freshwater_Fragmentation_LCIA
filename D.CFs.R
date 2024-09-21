# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

" This file creates the characterization factors (CFs).
The CFs express either a regional PDF (reversible species loss) or global PDF 
(irreversible species loss).
CF_regionalPDF = FF x EF x AF
CF_globalPDF = FF x EF x AF x GEP
"

# Libraries ----
library(ggplot2); library(sf); library(dplyr); library(readxl); library(tidyr);
library(rworldmap); library(terra); library(writexl); library(Hmisc)

# Paths ----
input <- "../data/input"
interim <- "../data/interim"
output <- "../data/output"

# Source files ----

# Load datasets ----
nondiadromous <- readRDS(file.path(input, "mbres_fwonly.rds"))
CFs <- readRDS(file.path(output, "AF_per_basin.rds"))
turbined_water <- read_sf(file.path(input, "Turbined_water.shp"))
GEP <- read_excel(file.path(input, "GEP_GDAT.xlsx"))
Main_Basins <- st_read(file.path(input, "GDAT_MainBasins.gpkg")) # 58811
pWF = rast(file.path(input, "wf_prod_irrigated_blue_1990_2019.nc"))
water_consumption <- read_sf(file.path(interim, "Sum_water_consumption_basin.shp"))

### Fate factor ----

## Irrig
# average pWF over last ten years
selected_bands <- subset(pWF, c(21, 22, 23, 24, 25, 26, 27, 28, 29, 30)) 
averaged_pWF <- mean(selected_bands, na.rm = TRUE)
writeRaster(averaged_pWF, file.path(interim, "pWF_avg.tif"), overwrite = TRUE)

# GIS conversion from pWF_avg.tif to Sum_water_consumtion_basin.shp, where grid
# cells are summed up to basin shapefile
water_consumption <- water_consumption %>% 
  rename(Count = Water_cons,
         Summed_WC = Water_co_1)

# Correction: if count is 0, sum should be NA
water_consumption <- water_consumption %>%
  mutate(Summed_WC = ifelse(Count == 0, NA, Summed_WC)) %>%
   as.data.frame() %>%
  select(MAIN_BAS, Summed_WC) %>%
  rename(Elem_irrig = Summed_WC)

CFs <- left_join(CFs, water_consumption, by = "MAIN_BAS") %>%
  mutate(FF_irrig = n / Elem_irrig)

## Hydro
turbined_water <- as.data.frame(turbined_water) %>%
  select(NCWU_sum, MAIN_BAS) %>%
  rename(Elem_hydro = NCWU_sum)

CFs <- left_join(CFs, turbined_water, by = "MAIN_BAS") %>%
  mutate(FF_hydro = n / Elem_hydro)

### Effect factor per basin (average type) ----
c <- 0.5 # conversion PAF to PDF
CFs <- left_join(CFs, select(nondiadromous, MAIN_BAS, PAFspec_cur), by = "MAIN_BAS") %>%
  mutate(EF = c * PAFspec_cur / n) %>%
  select(-PAFspec_cur)

### Characterization factors -----
CFs <- left_join(CFs, GEP, by = "MAIN_BAS") %>%
  mutate(CF_hydro_reg = FF_hydro * EF * AF_Hydro,
         CF_irrig_reg = FF_irrig * EF * AF_Irrig,
         CF_hydro_glo = FF_hydro * EF * AF_Hydro * GEP,
         CF_irrig_glo = FF_irrig * EF * AF_Irrig * GEP)

### Weighted global average ----
# weighted by the elementary flows
Total_elem_hydro <- sum(CFs$Elem_hydro, na.rm = TRUE)
Total_elem_irrig <- sum(CFs$Elem_irrig, na.rm = TRUE)
reg_CF_hydro <- weighted.mean(CFs$CF_hydro_reg, CFs$Elem_hydro, na.rm = TRUE)
reg_CF_irrig <- weighted.mean(CFs$CF_irrig_reg, CFs$Elem_irrig, na.rm = TRUE)
glo_CF_hydro <- weighted.mean(CFs$CF_hydro_glo, CFs$Elem_hydro, na.rm = TRUE)
glo_CF_irrig <- weighted.mean(CFs$CF_irrig_glo, CFs$Elem_irrig, na.rm = TRUE)

# weighted standard deviations global resolution CFs
weighted_var <- wtd.var(CFs$CF_hydro_glo, CFs$Elem_hydro, na.rm = TRUE)
glo_CF_hydro_sd <- sqrt(weighted_var)
weighted_var2 <- wtd.var(CFs$CF_irrig_glo, CFs$Elem_irrig, na.rm = TRUE)
glo_CF_irrig_sd <-sqrt(weighted_var2)

# Cleaning file
CFs <- CFs %>% select(-n, -Elem_hydro, -Elem_irrig)

### Stats -----

stats <- CFs %>% st_drop_geometry() %>%
  relocate(FF_hydro, FF_irrig, EF, AF_Hydro, AF_Irrig,
          AF_Other, GEP, CF_hydro_reg, CF_irrig_reg, 
          CF_hydro_glo, CF_irrig_glo)
 
num_non_na <- apply(stats, 2, function(x) sum(!is.na(x)))
min <- lapply(stats, min, na.rm = T)
max <- lapply(stats, max, na.rm = T) 
mean <- lapply(stats, mean, na.rm = T) 
median <- lapply(stats, median, na.rm = T) 
sd <- lapply(stats, sd, na.rm = T)
cv <- mapply(function(m, s) (s / m) * 100, mean, sd)

stats <- data.frame(num = num_non_na,
                    min = unlist(min),
                    max = unlist(max),
                    mean = unlist(mean),
                    median = unlist(median),
                    sd = unlist(sd),
                    cv = unlist(cv))

# delete missing values from CFs
suppl <- CFs %>% filter(!is.na(CF_hydro_reg) | !is.na(CF_hydro_glo) | 
                          !is.na(CF_irrig_reg) | !is.na(CF_irrig_glo))

### Save files ----
saveRDS(CFs, file.path(output, "CFs.rds"))
# gpkg file with characterization factors for aggregation in GIS to country-level
gpkg_file <- CFs %>% select("MAIN_BAS", "CF_hydro_reg", "CF_irrig_reg", 
                            "CF_hydro_glo", "CF_irrig_glo")
st_write(gpkg_file, file.path(interim, "CFs.gpkg"), 
         driver = "GPKG")
write_xlsx(suppl, file.path(output, "CFs.xlsx"))
write_xlsx(stats, file.path(output, "Stats.xlsx"))
