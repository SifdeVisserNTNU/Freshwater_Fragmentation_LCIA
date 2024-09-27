# Supplementary methods for the paper "Characterization Factors for the Impact of Climate Change on Freshwater Fish Species"

The following files can be found in this repository (src folder):
- *A.Cleaning_data.R.* This file contains the code to clean the GDAT database in preparation for deriving the allocation factors.
- *B.Dam_level_AF.R.* This file contains the code to derive dam-level allocation factors from the cleaned GDAT database and Fig. B1.
- *C.Basin_level_AF.R.* This file contains the code to derive basin-level allocation factors from the dam-level allocation factors and Figs. B9 and B10.
- *D.CFs.R.* This file contains the code to derive the fate factors, effect factors, and global extinction probabilities and calculates basin-level and global-level characterization factors. In addition, it produces Table 1 and a gpkg file, which is used to derive country-level characterization factors.
- *E.Plot_settings.R.* This file contains the settings for the themes and the scale fill used for many plots.
- *F.Plots_main R.* This file creates the violin plot of the characterization factors and maps of the characterization and allocation factors, i.e., Figs. 1-3.
- *G.Plots_SI.R.* This file creates the supplementary Figs. B2-B8.
- *H. Case_study.R.* This file contains the code for the case study and Fig. 4.

The input data can partially be found in the data folder. Other data needs to be downloaded from other sources.
- Download the GDAT shapefiles from Zhang, A.T., Gu, V.X., 2023. Global Dam Tracker: A database of more than 35,000 dams with location, catchment, and attribute information. Sci. Data 10, 111. https://doi.org/10.1038/s41597-023-02008-2 
- Download the irrigation blue water consumption data from Mialyk, O., Schyns, J.F., Booij, M.J., Su, H., Hogeboom, R.J., Berger, M., 2024. Water footprints and crop water use of 175 individual crops for 1990–2019 simulated with a global crop model. Sci. Data 11, 206. https://doi.org/10.1038/s41597-024-03051-3
 Some files cannot be uploaded due to size. Contact the author (sif.de.visser@ntnu.no) to request for files.
