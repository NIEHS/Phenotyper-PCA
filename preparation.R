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