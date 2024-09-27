# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

"
This file cleanes the GDAT dataset, which is then used to create allocation 
factors.
"

### Abbreviations ----
abbreviations <- c("Irrig", "Hydro", "WSupp", "FCont", "Navig", "Fishr", 
                   "Livst", "Recrn", "PCont", "WStor", "Other")
names(abbreviations) <- c("Irrigation", "Hydroelectricity", "Water_Supply", 
                          "Flood_Control", "Navigation", "Fisheries", 
                          "Livestock", "Recreation", "Pollution_Control", 
                          "Water_Storage", "Other")

### Libraries  ----
library(sf); library(dplyr); library(stringr)

### Paths  ----
input <- "../data/input"
interim <- "../data/interim"

### Load datasets  ----
GDAT <- read_sf(file.path(input, "GDAT/GDAT_v1_dams.shp")) %>%
  select("Feature_ID", "Dam_Name", "Admin0", "P_Irrig", "P_Hydro", "P_WSupp", "P_FCont",
         "P_Navig", "P_Fishr", "P_Livst", "P_Recrn", "P_PCont", "P_WStor",
         "P_Other", "Main_P", "Sec_P", "Mult_Dams") 

# Less missing values in Volume_Rep than Volume_Con
na_count2 <- sum(is.na(GDAT$Volume_Rep))/length(GDAT$Volume_Rep)*100 # 55%

### Numeric columns ----

# Columns P_Wsupp and P_FCont need to be converted to numeric
# Check why these are not numeric
unique(GDAT$P_WSupp)
unique(GDAT$P_FCont)
# x needs to be replaced by 1
GDAT$P_FCont[GDAT$P_FCont == 'x'] <- 1
GDAT$P_WSupp[GDAT$P_WSupp == 'x'] <- 1
# Converting to numeric
GDAT_cleaned <- GDAT %>%
  mutate(P_WSupp = as.numeric(as.character(P_WSupp)),
         P_FCont = as.numeric(as.character(P_FCont)))

### Fixing Main_P column ----
# 39 unique values in Main_P
replacements <- c("Flow Regulation" = "Other",
                  "Industrial" = "Other",
                  "Protection of the Environment" = "Other",
                  "Hydrolectricity" = "Hydroelectricity",
                  "Hydroelecricity" = "Hydroelectricity",
                  "Watersupply" = "Water_Supply",
                  "Water supply" = "Water_Supply",
                  "Water Supply" = "Water_Supply",
                  "Pollution control" = "Pollution_Control",
                  "Water storage" = "Water_Storage",
                  "Flood control" = "Flood_Control",
                  "Flood Control" = "Flood_Control")

GDAT_cleaned <- GDAT_cleaned %>%
  mutate(Main_P = na_if(Main_P, "Not Specified")) %>%
  mutate(Main_P = na_if(Main_P, "Unspecified")) %>%
  mutate(Main_P = na_if(Main_P, "Unspecified purpose")) %>%
  mutate(Main_P = na_if(Main_P, "Multiple purpose (unspecified)")) %>%
  mutate(Main_P = na_if(Main_P, "Unspecified (Other)")) %>%
  mutate(Main_P = na_if(Main_P, "Other/Not Specified")) %>%
  mutate(Main_P = na_if(Main_P, "Other purpose (unspecified)")) %>%
  mutate(Main_P = str_replace_all(Main_P, replacements)) %>%
  mutate(Main_P = str_replace(Main_P, "\\bHydro\\b", "Hydroelectricity")) 

# test <- data.frame(unique(GDAT_Main_P$Main_P))  

### Fix Sec_P column ----

replacements2 <- c("Water Supply" = "Water_Supply",
                   "Flood Control" = "Flood_Control")                  

GDAT_cleaned <- GDAT_cleaned %>%
  mutate(Sec_P = str_replace_all(Sec_P, replacements2)) 

### Split main purposes ----

for (purp in names(abbreviations)) {
  col_name <- paste("MP", abbreviations[purp], sep = "_")
    GDAT_cleaned <- GDAT_cleaned %>%
    mutate(!!col_name := if_else(str_detect(Main_P, purp), 1, 0))
}
# !! to unquote col_name, := to assign value

### Split secondary purposes ----

for (purp in names(abbreviations)) {
  col_name <- paste("SP", abbreviations[purp], sep = "_")
  GDAT_cleaned <- GDAT_cleaned %>%
    mutate(!!col_name := if_else(str_detect(Sec_P, purp), 1, 0))
}

### Update additional purposes ----
# not all purposes listed in the Main_P and Sec_P column are ticked, some are

components <- c(Irrigation="P_Irrig", Hydroelectricity="P_Hydro", Water_Supply="P_WSupp", 
                Flood_Control="P_FCont", Navigation="P_Navig", Fisheries="P_Fishr", 
                Livestock="P_Livst", Recreation="P_Recrn", Pollution_Control="P_PCont", 
                Water_Storage="P_WStor", Other="P_Other")

## Update based on Main_P ----
for (comp in names(components)) {
  col_name <- components[comp]
  GDAT_cleaned <- GDAT_cleaned %>%
    mutate(!!sym(col_name) := case_when(
      str_detect(Main_P, fixed(comp)) ~ 1, 
      TRUE ~ !!sym(col_name)
    ))
}

## Update based on Sec_P ----
for (comp in names(components)) {
  col_name <- components[comp]
  GDAT_cleaned <- GDAT_cleaned %>%
    mutate(!!sym(col_name) := case_when(
      str_detect(Sec_P, fixed(comp)) ~ 1, 
      TRUE ~ !!sym(col_name)
    ))
}

### Add num_purposes ----
P_cols <- paste0("P_", abbreviations)
MP_cols <- paste0("MP_", abbreviations)
SP_cols <- paste0("SP_", abbreviations)

GDAT_cleaned <- GDAT_cleaned %>%
  mutate(Num_purp = rowSums(cbind(!!!syms(P_cols)), na.rm = TRUE),
         Num_mainpurp = rowSums(cbind(!!!syms(MP_cols)), na.rm = TRUE),
         Num_secpurp = rowSums(cbind(!!!syms(SP_cols)), na.rm = TRUE))

### Clean NAs ----
# Convert to 0 in numeric columns

GDAT_cleaned <- GDAT_cleaned %>%
  mutate(across(all_of(c(P_cols,MP_cols,SP_cols)), ~replace(., is.na(.), 0)))

### Filling in secondary purpose ----
# If 1 main purpose is given and only two purposes in total # 2566 cases

update_Sec_P <- function(df, components) {
  for (comp in names(components)) {
    df <- df %>%
      mutate(Sec_P = ifelse(Num_purp == 2 & Num_mainpurp ==1 & Num_secpurp ==0 & 
                              get(components[comp]) == 1 & Main_P != comp, comp, Sec_P))
  }
  return(df)
}

GDAT_cleaned <- update_Sec_P(GDAT_cleaned, components)

update_SP_cols <- function(df, components) {
  for (comp in names(components)) {
    sp_col <- paste0("SP_", gsub("P_", "", components[comp]))
    df <- df %>%
      mutate(
        !!sp_col := ifelse(Num_purp == 2 & Num_mainpurp == 1 & Num_secpurp == 0 & 
                             get(components[comp]) == 1 & Main_P != comp, 1, get(sp_col))
      )
  }
  return(df)
}

GDAT_cleaned <- update_SP_cols(GDAT_cleaned, components) %>%
  mutate(Num_secpurp = rowSums(cbind(!!!syms(SP_cols)), na.rm = TRUE))

### Delete rows with no data on purposes ----
GDAT_cleaned <- GDAT_cleaned %>%
  filter(Num_purp != 0)

### Number of observations ----
# Initial dataset
obs_GDAT_initial <- nrow(GDAT) # 35140
# Filter rows with no purposes
obs_GDAT_nopurp <- obs_GDAT_initial - nrow(GDAT_cleaned) # 24993 | 10147

### Save file ----
saveRDS(GDAT_cleaned, file.path(interim, "GDAT_cleaned.rds"))
