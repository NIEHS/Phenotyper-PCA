# --------------------------------------
# PCA
#
#
# Preparation
#
#---------------------------------------

options(rgl.useNULL = TRUE) # Suppress the separate window.

library(tidyverse)
library(readxl)
library(writexl)
library(caret) # For correlations
library(data.table)
library(broom)
library(purrr)
library(forcats) #for ordering PHnumber by another group
library(gt)
library(pheatmap)
library(rgl) # For 3D plot
library(graphics) # For 3D plot
library(shiny)
library(nlme)
library(irr)
library(multcomp)
library(lme4)
library(emmeans)
library(lmerTest)
library(pbkrtest)
library(janitor)
library(lsr)

aiv_filename = "analysis-input-variables.xlsx"

ResourcesPath = './'
inputs = read_excel(paste(ResourcesPath, aiv_filename, sep=''))

source(paste(ResourcesPath, "functions.R", sep = ""))

assign_input_variables()

# Used for ordering plots later
doseLevels = str_trim(unlist(strsplit(doseLevels, split = ",")))


# Session Information
# R version 4.5.3 (2026-03-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)

# Matrix products: default
# LAPACK version 3.12.1

# locale:
#   [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
#   [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
#   [5] LC_TIME=English_United States.utf8    

# time zone: America/New_York
# tzcode source: internal

# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#   [1] lsr_0.5.2           janitor_2.2.1       pbkrtest_0.5.5     
#   [4] lmerTest_3.2-1      emmeans_2.0.3       lme4_2.0-1         
#   [7] Matrix_1.7-4        multcomp_1.4-30     TH.data_1.1-5      
#   [10] MASS_7.3-65         survival_3.8-6      mvtnorm_1.3-6      
#   [13] irr_0.84.1          lpSolve_5.6.23      nlme_3.1-168       
#   [16] shiny_1.13.0        rgl_1.3.36          pheatmap_1.0.13    
#   [19] gt_1.3.0            broom_1.0.12        data.table_1.18.2.1
#   [22] caret_7.0-1         lattice_0.22-9      writexl_1.5.4      
#   [25] readxl_1.4.5        lubridate_1.9.5     forcats_1.0.1      
#   [28] stringr_1.6.0       dplyr_1.2.1         purrr_1.2.2        
#   [31] readr_2.2.0         tidyr_1.3.2         tibble_3.3.1       
#   [34] ggplot2_4.0.2       tidyverse_2.0.0    

#   loaded via a namespace (and not attached):
#   [1] Rdpack_2.6.6         pROC_1.19.0.1        sandwich_3.1-1      
#   [4] rlang_1.2.0          magrittr_2.0.5       snakecase_0.11.1    
#   [7] otel_0.2.0           compiler_4.5.3       vctrs_0.7.3         
#   [10] reshape2_1.4.5       pkgconfig_2.0.3      fastmap_1.2.0       
#   [13] backports_1.5.1      promises_1.5.0       rmarkdown_2.31      
#   [16] prodlim_2026.03.11   tzdb_0.5.0           nloptr_2.2.1        
#   [19] xfun_0.57            jsonlite_2.0.0       recipes_1.3.2       
#   [22] later_1.4.8          parallel_4.5.3       R6_2.6.1            
#   [25] stringi_1.8.7        RColorBrewer_1.1-3   boot_1.3-32         
#   [28] parallelly_1.46.1    rpart_4.1.24         numDeriv_2016.8-1.1 
#   [31] estimability_1.5.1   cellranger_1.1.0     Rcpp_1.1.1          
#   [34] iterators_1.0.14     knitr_1.51           future.apply_1.20.2 
#   [37] zoo_1.8-15           base64enc_0.1-6      httpuv_1.6.17       
#   [40] splines_4.5.3        nnet_7.3-20          timechange_0.4.0    
#   [43] tidyselect_1.2.1     rstudioapi_0.18.0    yaml_2.3.12         
#   [46] timeDate_4052.112    codetools_0.2-20     listenv_0.10.1      
#   [49] plyr_1.8.9           withr_3.0.2          S7_0.2.1            
#   [52] coda_0.19-4.1        evaluate_1.0.5       future_1.70.0       
#   [55] xml2_1.5.2           pillar_1.11.1        foreach_1.5.2       
#   [58] stats4_4.5.3         reformulas_0.4.4     generics_0.1.4      
#   [61] hms_1.1.4            scales_1.4.0         minqa_1.2.8         
#   [64] globals_0.19.1       xtable_1.8-8         class_7.3-23        
#   [67] glue_1.8.0           tools_4.5.3          ModelMetrics_1.2.2.2
#   [70] gower_1.0.2          fs_2.0.1             grid_4.5.3          
#   [73] rbibutils_2.4.1      ipred_0.9-15         cli_3.6.6           
#   [76] lava_1.9.0           gtable_0.3.6         digest_0.6.39       
#   [79] htmlwidgets_1.6.4    farver_2.1.2         htmltools_0.5.9     
#   [82] lifecycle_1.0.5      hardhat_1.4.3        mime_0.13   
