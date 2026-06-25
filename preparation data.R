# --------------------------------------
# Terbufos and Chlorpyrifos
#
#
# Preparation Data
#
# 30 July 2025
# --------------------------------------

# Dataset made by combining 
#   Data/Raw/AnimalList_Balanced.xlsx
#   and
#   Data/Raw/Stack of SBData_DLH

# Each variable was then visualized in JMP distributions, across the sexes and exposures.
# If it looked skewed, then it was noted as a variable with "Possible Skew"
# Variables noted in data/variables_with_possible_skew_or_outliers.txt

source("preparation.R")

exclude = str_trim(unlist(strsplit(exclude, split = ",")))

if(nchar(mapLitter)>0){
  selectList = c("PHnumber", "Sex", "Drug", "Parameter Type", "Parameter and Unit", "Value", "Litter")
} else {
  selectList = c("PHnumber", "Sex", "Drug", "Parameter Type", "Parameter and Unit", "Value")
}

full <- read_excel(dataPath) %>%
  mapNames() %>%
  dplyr::select(all_of(selectList)) %>%
  shortenParameters() %>%
  rowwise() %>%
  mutate(ex = any(sapply(exclude, grepl, `Parameter and Unit`))) %>%
  filter(ex != TRUE)
  #filter(! (`Parameter and Unit` %in% exclude))

if(nchar(mapLitter)>0){
  temp <- adjustValuesForLitter(full)[[1]] 
  percent_litter <- adjustValuesForLitter(full)[[2]]
  full <- temp
  # First indexed output is the dataset with adjusted values
  # Second indexed output is a dataset with the percent of variability explained by litter
  # This REPLACES the original values with values adjusted for the litter!
  litterMessage <- "This dataset has litter effects. To account for this, before any analysis or output is done, the values are adjusted to remove the litter effect. Raw data values are fit with sex, group, and their interaction, with a random effect for litter. If the random effect explains at least 1% of the variability, then the model's litter estiamte is subtracted from the raw values."
  litterMessageANOVA <- "ANOVA models done with a random effect for litter, though data going into model was pre-adjusted for litter."
  } else {
  litterMessage <- ""
  litterMessageANOVA <- ""
}

excludedVars <- read_excel(dataPath) %>%
  mapNames() %>%
  dplyr::select(all_of(selectList)) %>%
  shortenParameters() %>%
  rowwise() %>%
  mutate(ex = any(sapply(exclude, grepl, `Parameter and Unit`))) %>%
  filter(ex == TRUE) %>%
  dplyr::select(`Parameter and Unit`) %>%
  unique()


#---------------------------#
# Parameter Type Sort Order #
#---------------------------#

# This is merged below with the color data frames.
Parameter_Order = as.data.frame(rbind(
  c("Phase Transition Pattern", 5),
  c("Kinematic", 4),
  c("Activity Bouts", 1),
  c("Sheltering", 6),
  c("Habituation", 3),
  c("DarkLight Index", 2),
  c("Open Field", 7),
  c("Other Continuous", 8)
))
names(Parameter_Order) = c("Parameter Type", "Parameter_Order")
Parameter_Order <- Parameter_Order %>%
  mutate(Parameter_Order = as.numeric(Parameter_Order))

#-------------------#
# Color             #
#-------------------#

base_color = "grey"

#--------------------#
# Color - parameters #
#--------------------#

Parameter_Type_Color = as.data.frame(rbind(
  c("Phase Transition Pattern", "#FF9138"),
  c("Kinematic", "#AB08FC"),
  c("Activity Bouts", "#3671FD"),
  c("Sheltering", "#1BE5B1"),
  c("Habituation", "#787878"),
  c("DarkLight Index", "#A00922"),
  c("Open Field", "#FCD126"),
  c("Other Continuous", "#53B616")
))
names(Parameter_Type_Color) = c("Parameter Type", "Parameter_Type_Color")

Parameter_Type_Colors_colList = c("#FF9138", "#AB08FC", "#3671FD", "#1BE5B1", "#787878", "#A00922", "#FCD126", "#53B616")
Parameter_Type_Colors_nameList = c("Phase Transition Pattern", "Kinematic", "Activity Bouts", "Sheltering", "Habituation", "DarkLight Index", "Open Field", 'Other Continuous')
Parameter_Type_Colors_List = list("Parameter_Type_Color", Parameter_Type_Colors_colList, Parameter_Type_Colors_nameList)

#-------------------------#
# Color - ANOVA           #
#-------------------------#
# We want to be able to color the variables by whether or not they had significant drug, sex, or interaction effects
# To do that, we need to run an ANOVA on the whole dataset.

ANOVA_signif <- applyANOVA(data = full, 
                           covar1 = "Sex", 
                           covar2 = "Drug",  
                           response = "Value", 
                           group = "Parameter and Unit",
                           LITTER = nchar(mapLitter)>0) # Runs with litter effects if TRUE otherwise no litter effects

ANOVA_pairwise <- applyPairwise(full, ANOVA_signif, "Parameter and Unit", "Value")


ANOVA_Signif_Colors <- ANOVA_signif %>%
  mutate(Sex_Signif_Color = case_when(Sex_p <= 0.05 ~ "red",
                                      TRUE ~ base_color),
         Drug_Signif_Color = case_when(Drug_p <= 0.05 ~ "blue",
                                       TRUE ~ base_color),
         Sex_by_Drug_Signif_Color = case_when(Sex_by_Drug_p <= 0.05 ~ "purple",
                                              TRUE ~ base_color)) %>%
  dplyr::select(`Parameter and Unit`, Sex_Signif_Color, Drug_Signif_Color, Sex_by_Drug_Signif_Color)

Sex_Signif_Color_colList = c("red", base_color)
Sex_Signif_Color_nameList = c("Significant Sex Effect", "N.S.")
Sex_Signif_Color_List = list("Sex_Signif_Color", Sex_Signif_Color_colList, Sex_Signif_Color_nameList)

Drug_Signif_Color_colList = c("blue", base_color)
Drug_Signif_Color_nameList = c(paste("Significant", mapDrug, "Effect", sep = " "), "N.S.")
Drug_Signif_Color_List = list("Drug_Signif_Color", Drug_Signif_Color_colList, Drug_Signif_Color_nameList)

Sex_by_Drug_Signif_Color_colList = c("purple", base_color)
Sex_by_Drug_Signif_Color_nameList = c("Significant Interaction Effect", "N.S.")
Sex_by_Drug_Signif_Color_List = list("Sex_by_Drug_Signif_Color", Sex_by_Drug_Signif_Color_colList, Sex_by_Drug_Signif_Color_nameList)

#--------------#
# Colors - Sex #
#--------------#

Sex_Colors = as.data.frame(rbind(
  c("Male", "blue"),
  c("Female", "red")
))
names(Sex_Colors) = c("Sex", "Sex_Colors")

Sex_Colors_colList = c("blue", "red")
Sex_Colors_nameList = c("Male", "Female")
Sex_Colors_List = list("Sex_Colors", Sex_Colors_colList, Sex_Colors_nameList)

#----------------------------#
# Add colors to their tables #
#----------------------------#

# Now we will add the colors to the Parameter Metadata table or to the full dataset
Parameter_Metadata <- full %>%
  dplyr::select(`Parameter Type`, `Parameter and Unit`) %>%
  unique() %>%
  mutate(Base_Color = base_color) %>%
  merge(Parameter_Type_Color, by = "Parameter Type") %>%
  merge(ANOVA_Signif_Colors, by = "Parameter and Unit") %>%
  merge(Parameter_Order, by = "Parameter Type")

IncludedParameterTypes <- Parameter_Metadata %>%
  dplyr::select(`Parameter Type`) %>%
  unique()

ParameterTypesToInclude <- unlist(Parameter_Type_Colors_nameList) %in% unlist(IncludedParameterTypes)
Parameter_Type_Colors_nameList <- Parameter_Type_Colors_nameList[ParameterTypesToInclude]
Parameter_Type_Colors_colList <- Parameter_Type_Colors_colList[ParameterTypesToInclude]


if(nchar(mapLitter) > 0){
  litter_Metadata <- read_excel(dataPath) %>%
    mapNames() %>%
    dplyr::select(PHnumber, Sex, Drug, Litter) %>%
    unique()
  Animal_Metadata <- litter_Metadata %>%
    merge(Sex_Colors, by = "Sex")
} else {
  Animal_Metadata <- full %>%
    dplyr::select(PHnumber, Sex, Drug) %>%
    unique() %>%
    merge(Sex_Colors, by = "Sex")
}

Colors_List = list(
  list("Parameter_Type_Color", "Sex_Signif_Color", "Drug_Signif_Color", "Sex_by_Drug_Signif_Color", "Sex_Colors"),
  list(Parameter_Type_Colors_colList, Sex_Signif_Color_colList, Drug_Signif_Color_colList, Sex_by_Drug_Signif_Color_colList, Sex_Colors_colList),
  list(Parameter_Type_Colors_nameList, Sex_Signif_Color_nameList, Drug_Signif_Color_nameList, Sex_by_Drug_Signif_Color_nameList, Sex_Colors_nameList)
)

#-------------------------#
# Format for Correlations #
#-------------------------#

if(nchar(mapLitter)>0){
  selectList = c("Parameter Type", "Parameter Short", "Litter", "Adjustment")
} else {
  selectList = c("Parameter Type", "Parameter Short")
}

wide_logged_int <- full %>% 
  dplyr::select(-c(all_of(selectList))) %>%
  pivot_wider(names_from = `Parameter and Unit`,
              values_from = `Value`)
# This will need to be filtered to remove any variables that have missings.
# See the next section.

    #--------------------#
    # Check for Missings #
    #--------------------#

missing <-  apply(wide_logged_int, 2, function(col) any(is.na(col)))
#print(paste("There are", sum(missing), "columns that have a missing value and must be excluded from analysis."))

wide_logged <- wide_logged_int[,!missing]

start_index = ncol(full) - 4 + 1 
# From the full dataset, we remove Parameter Short, Parameter Type, Parameter and Unit, and Value. The latter two are widened. 
# The +1 is to grab the correct starting line.
end_index = ncol(wide_logged)
wide_logged_for_corr <- wide_logged[,start_index:end_index]

# Next step is to run the correlations


# --------- End of Code ---------- #