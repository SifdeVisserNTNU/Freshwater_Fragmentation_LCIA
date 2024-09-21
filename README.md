# Supplementary methods for the paper "Characterization Factors for the Impact of Climate Change on Freshwater Fish Species"

The following files can be found in this repository (src folder):
- *A.Cleaning_data.R.* This file contains the code to clean the GDAT database in preparation for deriving the allocation factors.
- *B.Dam_level_AF.R.* This file contains the code to derive dam-level allocation factors from the cleaned GDAT database and Fig. S1.
- *C.Basin_level_AF.R.* This file contains the code to derive basin-level allocation factors from the dam-level allocation factors and Figs. S9 and S10.
- *D.CFs.R.* This file contains the code to derive the fate factors, effect factors, and global extinction probabilities and calculates basin-level and global-level characterization factors. In addition, it produces Table 1 and a gpkg file, which is used to derive country-level characterization factors.
- *E.Plot_settings.R.* This file contains the settings for the themes and the scale fill used for many plots.
- *F.Plots_main R.* This file creates the violin plot of the characterization factors and maps of the characterization and allocation factors, i.e., Figs. 1-3.
- *G.Plots_SI.R.* This file creates the supplementary Figs. S2-S8.
- *H. Case_study.R.* This file contains the code for the case study and Fig. 4.
