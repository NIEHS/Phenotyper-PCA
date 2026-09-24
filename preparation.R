# --------------------------------------
# PCA
#
#
# Preparation
#
# 15 May 2025
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

############### Session Information, 2026-09-24 ###############
# R version 4.5.3 (2026-03-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 22631)
# Matrix products: default
#   LAPACK version 3.12.1
# locale:
# [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C
# [5] LC_TIME=English_United States.utf8
# time zone: America/New_York
# tzcode source: internal
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
# other attached packages:
#  [1] janitor_2.2.1       pbkrtest_0.5.5      lmerTest_3.2-1      emmeans_2.0.3
#  [5] lme4_2.0-1          Matrix_1.7-4        multcomp_1.4-30     TH.data_1.1-5
#  [9] MASS_7.3-65         survival_3.8-6      mvtnorm_1.3-6       irr_0.84.1
# [13] lpSolve_5.6.23      nlme_3.1-168        shiny_1.13.0        rgl_1.3.36
# [17] pheatmap_1.0.13     gt_1.3.0            broom_1.0.12        data.table_1.18.2.1
# [21] caret_7.0-1         lattice_0.22-9      writexl_1.5.4       readxl_1.4.5
# [25] lubridate_1.9.5     forcats_1.0.1       stringr_1.6.0       dplyr_1.2.1
# [29] purrr_1.2.2         readr_2.2.0         tidyr_1.3.2         tibble_3.3.1
# [33] ggplot2_4.0.2       tidyverse_2.0.0
# loaded via a namespace (and not attached):
#  [1] Rdpack_2.6.6         pROC_1.19.0.1        sandwich_3.1-1       rlang_1.2.0
#  [5] magrittr_2.0.5       snakecase_0.11.1     otel_0.2.0           compiler_4.5.3
#  [9] vctrs_0.7.3          reshape2_1.4.5       pkgconfig_2.0.3      fastmap_1.2.0
# [13] backports_1.5.1      promises_1.5.0       rmarkdown_2.31       prodlim_2026.03.11
# [17] tzdb_0.5.0           nloptr_2.2.1         xfun_0.57            jsonlite_2.0.0
# [21] recipes_1.3.2        later_1.4.8          parallel_4.5.3       R6_2.6.1
# [25] stringi_1.8.7        RColorBrewer_1.1-3   boot_1.3-32          parallelly_1.46.1
# [29] rpart_4.1.24         numDeriv_2016.8-1.1  estimability_1.5.1   cellranger_1.1.0
# [33] Rcpp_1.1.1           iterators_1.0.14     knitr_1.51           future.apply_1.20.2
# [37] zoo_1.8-15           base64enc_0.1-6      httpuv_1.6.17        splines_4.5.3
# [41] nnet_7.3-20          timechange_0.4.0     tidyselect_1.2.1     rstudioapi_0.18.0
# [45] yaml_2.3.12          timeDate_4052.112    codetools_0.2-20     listenv_0.10.1
# [49] plyr_1.8.9           withr_3.0.2          S7_0.2.1             coda_0.19-4.1
# [53] evaluate_1.0.5       future_1.70.0        xml2_1.5.2           pillar_1.11.1
# [57] foreach_1.5.2        stats4_4.5.3         reformulas_0.4.4     generics_0.1.4
# [61] hms_1.1.4            scales_1.4.0         minqa_1.2.8          globals_0.19.1
# [65] xtable_1.8-8         class_7.3-23         glue_1.8.0           tools_4.5.3
# [69] ModelMetrics_1.2.2.2 gower_1.0.2          fs_2.0.1             grid_4.5.3
# [73] rbibutils_2.4.1      ipred_0.9-15         cli_3.6.6            lava_1.9.0
# [77] gtable_0.3.6         digest_0.6.39        htmlwidgets_1.6.4    farver_2.1.2
# [81] htmltools_0.5.9      lifecycle_1.0.5      hardhat_1.4.3        mime_0.13
