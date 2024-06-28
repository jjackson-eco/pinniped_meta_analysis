# Analysis for
# *The global extent and severity of operational interactions between conflicting pinnipeds and fisheries*
#### John Jackson, William N. S. Arlidge, Rodrigo Oyanedel, & Katrina J. Davis
#### 2024-06-27
#### Repository created by John Jackson

[![DOI](https://zenodo.org/badge/820920701.svg)](https://zenodo.org/doi/10.5281/zenodo.12579909)

This repository accompanies the above titled publication, providing a fully reproducible analysis pipeline for all results presented. The archived version for continuity is found on Zenodo. You can find the paper [{insert DOI here when we receive it}](dsadasdas.com).

This repository is split into four subrepositories, `data/`, `wrangling/`, `analysis/`, and `output/`, which are organised for the progression of the analyses. The main meta-analysis, which forms the majority of the results can be found in `analysis/meta_analysis_2024.R`. Full file descriptions are as follows.

## `data/`

<details>
  <summary>Click here to expand</summary>

- `scoping_data_extraction_20240412.csv`: Raw meta-analysis data from retained studies, which forms the basis of tableS1
- `non_standardised_data_20230112.csv`: Raw meta-analysis data from studies that were not temporally standardised
- `op_interaction.RData`: Meta-analysis data as an RData file
- `survey_data_July2022.RData`: Old meta-anaylsis data from non-standardised studies, still used for study type naming.
- `study_names_unicode_20221122.csv`: Meta-analysis author names in unicode format for accents
- `study_names_unicode_20221121.csv`: Meta-analysis author names in unicode format for accents (old version but still used for some studies)
- `pinnrev_sf.RData`: Spatial `sf` data from retained studies
- `pinnrev_centroid_data.RData`: Spatial data from retained studies, where lat-lon centroids are specified rather than full polygon geometries.
- `potential_pinniped_conflict_allpin.tif`: Raster `tif` data for the potential for fishery-pinniped interactions, with all layers included
- `potential_pinniped_conflict_nogfw.tif`: Raster `tif` data for the potential for fishery-pinniped interactions, excluding Global Fishing Watch data for fishing effort from Kroodsma et al. 
- `potential_pinniped_conflict_noshore.tif`: Raster `tif` data for the potential for fishery-pinniped interactions, excluding data on distance from shore

</details>

## `wrangling/`

<details>
  <summary>Click here to expand</summary>
  
- `spaial_conflict_potential.R`: Creating index of potential for pinniped-fishery interactions from spatial layers.
- `wrangling_coordinates.R`: Extracting spatial information from retained studies.
- `meta_data_names.R`: Adjusting author names
- `iucn_wrangle.R`: IUCN occurence fata for pinniped species globally
- `gfw_wrangling.R`: Preparation of Global Fishing Watch data on fishing effort from Kroodsma et al.
- `raster_aggregation.R`: Aggregating raster data of distance to shore into correct format
- `raster_layer_preparation.R`: Bringing together raster layers for spatial conflict potential and ensuring spatial overlap is correct etc.

</details>

## `analysis/`

<details>
  <summary>Click here to expand</summary>
  
- `meta_analysis_2024.R`: Main meta-analysis script for study.
- `non_standardised_data_analysis.R`: Explorative analysis of studies that were not temporally standardized.
- `spatial_conflict_pontential_analysis.R`: Additional analysis of spatial conflict potential including overlap with retained studies.
- `underlying_raster_plots.R`: Spatial plotting of individual raster layers in the conflict potential index.
- `old/`: Contains initial iterations of scripts

</details>

## `output/`

<details>
  <summary>Click here to expand</summary>
  
Figures from analysis.

</details>

## Package Versions

<details>
  <summary>Click here to expand</summary>
  
R version 4.4.0 (2024-04-24)
Platform: aarch64-apple-darwin20
Running under: macOS Sonoma 14.4

other attached packages:

other attached packages:
 [1] metafor_4.6-0       numDeriv_2016.8-1.1 Matrix_1.7-0        meta_7.0-0         
 [5] metadat_1.2-0       flextable_0.9.6     rasterize_0.1       sf_1.0-16          
 [9] MetBrewer_0.2.0     terra_1.7-78        raster_3.6-26       sp_2.1-4           
[13] patchwork_1.2.0     lubridate_1.9.3     forcats_1.0.0       stringr_1.5.1      
[17] dplyr_1.1.4         purrr_1.0.2         readr_2.1.5         tidyr_1.3.1        
[21] tibble_3.2.1        ggplot2_3.5.1       tidyverse_2.0.0    

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1        fastmap_1.2.0           CompQuadForm_1.4.3     
 [4] fontquiver_0.2.1        mathjaxr_1.6-0          promises_1.3.0         
 [7] digest_0.6.35           timechange_0.3.0        mime_0.12              
[10] lifecycle_1.0.4         gfonts_0.2.0            magrittr_2.0.3         
[13] compiler_4.4.0          rlang_1.1.3             tools_4.4.0            
[16] utf8_1.2.4              data.table_1.15.4       knitr_1.47             
[19] askpass_1.2.0           classInt_0.4-10         curl_5.2.1             
[22] xml2_1.3.6              KernSmooth_2.23-22      httpcode_0.3.0         
[25] withr_3.0.0             fansi_1.0.6             gdtools_0.3.7          
[28] xtable_1.8-4            e1071_1.7-14            colorspace_2.1-0       
[31] MASS_7.3-60.2           scales_1.3.0            crul_1.4.2             
[34] cli_3.6.2               rmarkdown_2.27          crayon_1.5.2           
[37] ragg_1.3.2              generics_0.1.3          rstudioapi_0.16.0      
[40] tzdb_0.4.0              minqa_1.2.7             DBI_1.2.2              
[43] proxy_0.4-27            splines_4.4.0           vctrs_0.6.5            
[46] boot_1.3-30             jsonlite_1.8.8          fontBitstreamVera_0.1.1
[49] hms_1.1.3               systemfonts_1.1.0       units_0.8-5            
[52] glue_1.7.0              nloptr_2.1.1            codetools_0.2-20       
[55] stringi_1.8.4           gtable_0.3.5            later_1.3.2            
[58] lme4_1.1-35.4           munsell_0.5.1           pillar_1.9.0           
[61] htmltools_0.5.8.1       openssl_2.2.0           R6_2.5.1               
[64] textshaping_0.4.0       evaluate_0.23           shiny_1.8.1.1          
[67] lattice_0.22-6          png_0.1-8               fontLiberation_0.1.0   
[70] httpuv_1.6.15           class_7.3-22            Rcpp_1.0.12            
[73] zip_2.3.1               uuid_1.2-0              nlme_3.1-164           
[76] officer_0.6.6           xfun_0.44               pkgconfig_2.0.3

</details>

---