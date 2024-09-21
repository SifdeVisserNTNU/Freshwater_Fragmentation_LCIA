# -*- coding: utf-8 -*-
# Author: sif.de.visser@ntnu.no
# Date: 20/09/2024

"
This file creates the allocation factors for each purpose (irrig, hydro and other) per dam
Other contains all purposes - irrig - hydro

Assumptions:
- Rank of Irrig and Hydro when not main or secondary purpose: 2 or 3
- Hydro and Irrig can have tied third rank
- After third rank, there are no tied ranks
"

### Libraries  ----
library(sf); library(dplyr); library(stringr); library(ggplot2)

### Paths  ----
interim <- "../data/interim"

### Load datasets  ----
GDAT_cleaned <- readRDS(file.path(interim, "GDAT_cleaned.rds"))

# How many dams multipurpose?
count_num_purp_multi <- sum(GDAT_cleaned$Num_purp > 1)/nrow(GDAT_cleaned)*100

### Splitting dataset according to method to be implemented ----

## 1 - Unranked_Purp ----
# No main or secondary purposes are listed
Unranked_Purp <- GDAT_cleaned %>% 
  filter(Num_mainpurp == 0 & Num_secpurp == 0 & Num_purp >= 1)

### Equal weight method ----
# All are equal: 1/Num_purp

AF_Unranked <- function(Purp, Num_purp) {
  if (Purp > 0) {AF <- 1/Num_purp
  }
  else {AF <- 0
  }
  return(AF)
}

Unranked_Purp$AF_Irrig <- mapply(AF_Unranked, Unranked_Purp$P_Irrig, Unranked_Purp$Num_purp)
Unranked_Purp$AF_Hydro <- mapply(AF_Unranked, Unranked_Purp$P_Hydro, Unranked_Purp$Num_purp)


## 2 - Ranked_Purp ----
# At least a main purpose or secondary purpose should be listed
Ranked_Purp <- GDAT_cleaned %>% 
  filter(Num_mainpurp >= 1 | Num_secpurp >= 1)


### Assigning ranks for irrigation & hydroelectricity to Ranked_Purp ----

# If no second purpose is given > assign 2nd purpose
# Num_purp > 2, Num_secpurp == 0

Ranked_Purp <- Ranked_Purp %>% 
  mutate(Rank_Irrig = ifelse(MP_Irrig == 1, 1,
                             ifelse(SP_Irrig == 1, 2, 
                                    ifelse(Num_secpurp == 0 & Num_purp - Num_mainpurp > 1 & P_Irrig == 1, 2,
                                           ifelse(P_Irrig == 1, 3, 0)))),
         Rank_Hydro = ifelse(MP_Hydro == 1, 1,
                             ifelse(SP_Hydro == 1, 2,
                                    ifelse(Num_secpurp == 0 & Num_purp - Num_mainpurp > 1 & P_Hydro == 1, 2,
                                           ifelse(P_Hydro == 1, 3, 0)))))

### Update number of secondary ranks (Num_secpurp) ----
# Fix 1 shared rank if Num_purp - Num_mainpurp > 1 & Rank_Irrig == 0 & Rank_Hydrp == 0

# Assign to Sec_P & recount Num_secpurp
abbreviations <- c("Irrig", "Hydro", "WSupp", "FCont", "Navig", "Fishr", 
                   "Livst", "Recrn", "PCont", "WStor", "Other")
SP_cols <- paste0("SP_", abbreviations)

Ranked_Purp <- Ranked_Purp %>% 
  mutate(SP_Irrig = ifelse(Rank_Irrig == 2, 1, SP_Irrig),
         SP_Hydro = ifelse(Rank_Hydro == 2, 1, SP_Hydro),
         Num_secpurp = rowSums(cbind(!!!syms(SP_cols))))

# Fix: second purp is unknown, but there should be one
Ranked_Purp <- Ranked_Purp %>%
  mutate(Num_secpurp = ifelse(Num_secpurp == 0 & Num_purp - Num_mainpurp > 0, 1, Num_secpurp))

# Num_purp =1, Num_mainpurp =1, Num_secpurp = 0, Num_terpurp = 1, Rank_Irrig = 1, Rank_Hydro = 0

## Add number of third ranks (Num_terpurp) ----
# Assumption: no tied 3rd rank unless due to Irrig and Hydro

Ranked_Purp <- Ranked_Purp %>%
  mutate(Num_terpurp = ifelse(Rank_Irrig == 3 & Rank_Hydro == 3, 2,
                              ifelse(Rank_Irrig == 3 | Rank_Hydro == 3, 1, 
                                     ifelse(Num_purp - Num_mainpurp - Num_secpurp > 0, 1, 0))))

### Function for dense ranking  1 1 2 3 ----

dense_ranking <- function(Num_mainpurp, Num_secpurp, Num_terpurp, Num_purp) {
  # seq1 represents 1's for the main purpose(s)
  seq1 <- rep(1, Num_mainpurp)
  # seq2 represents 2's for the secondary purpose(s)
  seq2 <- rep(2, Num_secpurp)
  # seq3 represents 3's for the tertiary purpose(s)
  seq3 <- rep(3, Num_terpurp)
  # Calculate the remaining purposes
  remaining_purposes <- Num_purp - Num_mainpurp - Num_secpurp - Num_terpurp
  # seq4 represents 4, 5, ...
  seq4 <- if (remaining_purposes > 0) {
    seq(from = 4, to = 4 + remaining_purposes - 1)
  } else {
    integer(0)  # No remaining purposes
  }
  dense_sequence <- c(seq1, seq2, seq3, seq4)
  return(dense_sequence)
}

### Rank Sum weight method ----
# AF = (n+1-r_p)/(sum_{i=1}^{n}(n+1-i))
# Formula doesn't handle r_p == 0 well, so condition added

calculate_AF <- function(ranking_function, n, r_p, Num_mainpurp, Num_secpurp, Num_terpurp) {
  if (r_p == 0) {
    return(0)
  }
  sequence <- ranking_function(Num_mainpurp, Num_secpurp, Num_terpurp, n)
  denominator <- sum(n+1-sequence)
  AF <- (n+1-r_p)/denominator
  return(AF)
}

### Calculate AFs ----

calculate_ranking <- function(calculation_function, ranking_function, rank_column) {
  mapply(calculation_function, 
         Ranked_Purp$Num_purp,
         Ranked_Purp[[rank_column]], 
         Ranked_Purp$Num_mainpurp,
         Ranked_Purp$Num_secpurp,
         Ranked_Purp$Num_terpurp,
         MoreArgs = list(ranking_function = ranking_function))
}

calculations <- list(RS = calculate_AF)
rankings <- list(Dense = dense_ranking)
rank_columns <- c("Irrig", "Hydro")

AF_all <- c()
# Loop over each combination and apply the calculate_ranking function
for (calc_short_name in names(calculations)) {
  for (rank_short_name in names(rankings)) {
    for (rank_column in rank_columns) {
      # Construct the new column name
      new_col_name <- paste("AF", rank_column, sep = "_")
      # Apply the calculate_ranking function and assign the result to the new column
      Ranked_Purp[[new_col_name]] <- calculate_ranking(
        calculations[[calc_short_name]], rankings[[rank_short_name]], paste0("Rank_", rank_column)
      )
      AF_all <- c(AF_all, new_col_name)
    }
  }
}

### Join dataframes ----

Allocation_Factors <- bind_rows(Ranked_Purp, Unranked_Purp) %>%
  select(Feature_ID, Dam_Name, geometry, all_of(AF_all))
 
# n_na <- Allocation_Factors %>% summarise(across(everything(), ~ sum(is.na(.))))

Allocation_Factors <- Allocation_Factors %>% mutate(AF_Other = 1 - AF_Irrig - AF_Hydro,)

### Global average AF
glo_avg_hydro <- mean(Allocation_Factors$AF_Hydro, na.rm=TRUE)
glo_avg_irrig <- mean(Allocation_Factors$AF_Irrig, na.rm=TRUE)

add <- c("AF_Other")
AF_all <- c(AF_all, add)

### Number of observations ----
obs_unranked <- nrow(Unranked_Purp) # 461
obs_ranked <- nrow(Ranked_Purp) # 24532

### SI
# Example of how allocations factors look like for different ranks and number of
# purposes

rank_1 <- c(calculate_AF(dense_ranking, 1, 1, 1, 0, 0), 
            calculate_AF(dense_ranking, 2, 1, 1, 1, 0),
            calculate_AF(dense_ranking, 3, 1, 1, 1, 1),
            calculate_AF(dense_ranking, 4, 1, 1, 1, 1),
            calculate_AF(dense_ranking, 5, 1, 1, 1, 1))

rank_2 <- c(NA, 
            calculate_AF(dense_ranking, 2, 2, 1, 1, 0),
            calculate_AF(dense_ranking, 3, 2, 1, 1, 1),
            calculate_AF(dense_ranking, 4, 2, 1, 1, 1),
            calculate_AF(dense_ranking, 5, 2, 1, 1, 1))

rank_3 <- c(NA, NA,
            calculate_AF(dense_ranking, 3, 3, 1, 1, 1),
            calculate_AF(dense_ranking, 4, 3, 1, 1, 1),
            calculate_AF(dense_ranking, 5, 3, 1, 1, 1))

rank_4 <- c(NA, NA, NA,
            calculate_AF(dense_ranking, 4, 4, 1, 1, 1),
            calculate_AF(dense_ranking, 5, 4, 1, 1, 1))

rank_5 <- c(NA, NA, NA, NA,
            calculate_AF(dense_ranking, 5, 5, 1, 1, 1))

example <- data.frame(num_purp = rep(1:5,5), AF = c(rank_1, rank_2, rank_3, rank_4, rank_5), rank = c(rep(1,5), rep(2,5), rep(3,5), rep(4,5), rep(5,5)))
example$num_purp <- as.factor(example$num_purp)

Fig_S1 <- ggplot(example, aes(x = rank, y = AF, color = num_purp, group = num_purp)) +
  geom_line() +
  geom_point() +
  theme_light() +
  theme(text = element_text(size = 10),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), units = 'cm')) +
  guides(color = guide_legend(title.position = "top")) +
  scale_color_viridis_d(option = "G", begin = 0.2, end = 0.8) +
  labs(color = "Number of purposes", x = "Rank [-]", y = "Allocation factor [-]")
  
### Save file ----
saveRDS(Allocation_Factors, file.path(interim, "Allocation_Factors.rds"))
saveRDS(AF_all, file.path(interim, "AF_all.rds"))
ggsave(path = "../visualisations/", filename = "Fig_S1.png", Fig_S1,
       dpi = 600, width = 10, height = 6 , units = "cm")
