# Functions
applyANOVA <- function(data, covar1, covar2, response, group, LITTER = FALSE) {
  if(LITTER == FALSE){
    analysis_set = data %>%
      dplyr::select(all_of(c(covar1, covar2, response, group))) %>%
      rename("COVAR1" = all_of(covar1),
             "COVAR2" = all_of(covar2),
             "RESPONSE" = all_of(response),
             "GROUP" = all_of(group)) %>%
      group_by(GROUP) %>%
      nest() %>%
      mutate(int_model = map(data, ~anova(lm(RESPONSE ~ COVAR1 * COVAR2, data=.x))),
             add_model = map(data, ~anova(lm(RESPONSE ~ COVAR1 + COVAR2, data=.x)))) 
    x = 0 # Used for indexing anova output in fixed vs. random effect models
  } else if(LITTER == TRUE) {
    analysis_set = data %>%
      dplyr::select(all_of(c(covar1, covar2, response, group, "Litter"))) %>%
      rename("COVAR1" = all_of(covar1),
             "COVAR2" = all_of(covar2),
             "RESPONSE" = all_of(response),
             "GROUP" = all_of(group)) %>%
      group_by(GROUP) %>%
      nest() %>%
      mutate(int_model = map(data, ~anova(lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x))),
             add_model = map(data, ~anova(lme(RESPONSE ~ COVAR1 + COVAR2, random=~1|Litter, data=.x))),
             int_model_full = map(data, ~lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x)),
             add_model_full = map(data, ~lme(RESPONSE ~ COVAR1 + COVAR2, random=~1|Litter, data=.x)),
             var_int = map(int_model_full, ~VarCorr(.x)),
             var_add = map(add_model_full, ~VarCorr(.x)),
             perRandom_int = 100*as.numeric(var_int[[1]][1, "Variance"])/(as.numeric(var_int[[1]][1, "Variance"]) + as.numeric(var_int[[1]][2, "Variance"])),
             perRandom_add = 100*as.numeric(var_add[[1]][1, "Variance"])/(as.numeric(var_add[[1]][1, "Variance"]) + as.numeric(var_add[[1]][2, "Variance"]))
      ) 
    x = 1 # Used for indexing anova output in fixed vs. random effect models
  }
  # Pull out the interaction p-value
  p = c()
  for(i in 1:nrow(analysis_set)) {
    int_p = analysis_set[[3]][[i]][3+x,5-x] # This is the p-value of the interaction term
    p = c(p, int_p)
  }
  analysis_set$int_p = p
  
  # Pull out the significance for the terms - it uses the interaction model p-values or the additive, 
  #     depending on the significance of the interaction term p-value
  covar1_p = c()
  covar2_p = c()
  RE_percent = c()
  for (i in 1:nrow(analysis_set)) {
    if(analysis_set$int_p[i]<= 0.05){ # Interaction Model
      VAR1_p = analysis_set[[3]][[i]][1+x,5-x]
      VAR2_p = analysis_set[[3]][[i]][2+x,5-x]
      if(LITTER==TRUE){
        RE_per = analysis_set$perRandom_int[i]
      } else {
        RE_per = NA
      }
    } else {                          # Additive Model
      VAR1_p = analysis_set[[4]][[i]][1+x,5-x]
      VAR2_p = analysis_set[[4]][[i]][2+x,5-x]
      if(LITTER==TRUE){
        RE_per = analysis_set$perRandom_add[i]
      } else {
        RE_per = NA
      }
    }
    covar1_p = c(covar1_p, VAR1_p)
    covar2_p = c(covar2_p, VAR2_p)
    RE_percent = c(RE_percent, RE_per)
  }
  analysis_set$covar1_p = covar1_p
  analysis_set$covar2_p = covar2_p
  analysis_set$RE_percent = RE_percent
  
  
  name1 = paste(covar1, "_p", sep = "")
  name2 = paste(covar2, "_p", sep = "")
  nameInt = paste(covar1,"_by_", covar2, "_p", sep = "")
  output_set <- analysis_set %>%
    dplyr::select(GROUP, covar1_p, covar2_p, int_p, RE_percent) %>%
    mutate(int_p = case_when(int_p<=0.05 ~ int_p,
                             TRUE ~ NA))
  names(output_set) = c(group, name1, name2, nameInt, "RE_percent")
  
  return(output_set)
}



applyPairwise <- function(DATASET, TABLE, VAR_NAME, VALUE_NAME){
  addition <- c()
  for (i in 1:nrow(TABLE)){
    SUBSET <- DATASET %>% 
      filter(get(VAR_NAME) == as.character(TABLE[i, VAR_NAME])) %>%
      mutate(Value = get(VALUE_NAME),
             Combo = interaction(Sex, Drug),
             Drug_f = factor(Drug, levels = doseLevels),
             Sex_f = factor(Sex, levels = c("Male", "Female")))
    if(is.na(TABLE[i,"RE_percent"])){
      RANDOM = FALSE
    } else {
      RANDOM = TRUE
    }
    if(is.na(TABLE[i,"Sex_by_Drug_p"])){
      # Additive Model
      if(RANDOM == TRUE){
        model <- lmer(Value ~ Sex_f + Drug_f + (1|Litter), data = SUBSET)
      } else {
        model <- lm(Value ~ Sex_f + Drug_f, data = SUBSET)
      }
    } else {
      # Interaction Model
      if(RANDOM == TRUE){
        model <- lmer(Value ~ Sex_f * Drug_f + (1|Litter), data = SUBSET)
      } else {
        model <- lm(Value ~ Sex_f * Drug_f, data = SUBSET)
      }
    }
    # Now that you have the model you want, compute the contrasts
    
    # Drug within Sex
    emm_drug <- emmeans(model, ~ Drug_f | Sex_f)
    K_drug <- contrast(emm_drug, "pairwise")@linfct
    lab_d_df <- as.data.frame(contrast(emm_drug, "pairwise", adjust = "tukey"))
    labs_drug <- with(lab_d_df, paste(Sex_f, contrast, sep=": "))
    rownames(K_drug) <- labs_drug
    DrugWithinSex <- ProcessPairwise(summary(glht(model, linfct = K_drug)),
                                     labs_drug)
    
    # Drug within Drug
    emm_sex <- emmeans(model, ~ Sex_f | Drug_f)
    K_sex <- contrast(emm_sex, "pairwise")@linfct
    lab_s_df <- as.data.frame(contrast(emm_sex, "pairwise", adjust = "tukey"))
    labs_sex <- with(lab_s_df, paste(Drug_f, contrast, sep=": "))
    rownames(K_sex) <- labs_sex
    SexWithinDrug <- ProcessPairwise(summary(glht(model, linfct = K_sex)),
                                     labs_sex)
    
    addition_small <- cbind(SexWithinDrug, DrugWithinSex)
    addition <- rbind(addition, addition_small)
  }
  output <- as.data.frame(cbind(TABLE, addition))
  return(output)
}



compute_d <- function(DATA, G1, G2){
  x <- DATA %>% filter(Group == G1) %>% pull(all_of(Value))
  y <- DATA %>% filter(Group == G2) %>% pull(all_of(Value))
  
  d <- cohensD(x, y)
  
  tibble(group1 = G1,
         group2 = G2,
         cohensD = d)
}

cohensD_withinSexDrug <- function(DATA, VAR){
  
  int1 <- DATA %>%
    mutate(Group = case_when(Sex == "Male" ~ paste("Male ", Drug, sep = ""),
                             Sex == "Female" ~ paste("Female ", Drug, sep = ""))) %>%
    rename("VARNAME" = all_of(VAR))
  combo_int <- combn(unique(int1$Group), 2)
  # We want to pull only the ones that are within male or within female
  combo = c()
  for(i in 1:ncol(combo_int)){
    if(str_sub(combo_int[1,i], 1, 2) == str_sub(combo_int[2,i], 1, 2)){
      combo = cbind(combo, combo_int[,i])
    }
  }
  
  output1 <- c()
  for(i in 1:ncol(combo)){
    int2 <- int1 %>%
      group_by(VARNAME) %>%
      nest() %>%
      mutate(d = map(data, ~compute_d(.x, combo[1,i], combo[2,i]))) %>%
      unnest(d)
    output1 <- rbind(output1, int2)
  }
  
  output <- output1 %>%
    mutate(`Group Name` = paste(group1, group2, sep = " vs. ")) %>%
    dplyr::select(VARNAME, `Group Name`, cohensD) %>%
    pivot_wider(names_from = `Group Name`, values_from = cohensD) %>%
    rename({{VAR}} := VARNAME)
  
  return(output) 
}


ProcessPairwise <- function(TABLE, LABELS){
  df = as.data.frame(
    cbind(
      contrast = LABELS,
      estimate = as.numeric(TABLE$test$coefficients),
      SE = as.numeric(TABLE$test$sigma),
      pvalue = as.numeric(TABLE$test$pvalues)
    )
  ) %>%
    mutate(
      estimate = as.numeric(estimate),
      SE = as.numeric(SE),
      pvalue = round(as.numeric(pvalue), 3),
      pvalue_str = case_when(pvalue < 0.001 ~ "p<0.001",
                             TRUE ~ paste("p=",round(pvalue, 3), sep="")),
      CI95 = 1.96*SE,
      output_string = case_when(pvalue < 0.01 ~ paste(pvalue, "**"),
                                pvalue < 0.05 ~ paste(pvalue, "*"),
                                TRUE ~ paste(pvalue))
      #output_string = paste(round(estimate, 2), "+/-", round(CI95, 3), ", ", pvalue_str, sep = "")
      ) %>%
    dplyr::select(contrast, output_string) %>%
    pivot_wider(names_from = contrast, values_from = output_string)
  
  return(df)
}



assign_input_variables <- function(){
  titleName <<- getInput("titleName")
  dataPath <<- getInput("dataPath")
  outputPath <<- getInput("outputPath")
  correlationCutoff <<- as.numeric(getInput("correlationCutoff"))
  PCvariationThreshold <<- as.numeric(getInput("PCvariationThreshold"))
  exclude <<- getInput("exclusionVariables")
  introduction <<- getInput("introduction")
  doseLevels <<- getInput("doseLevels")
  mapID <<- getInput("mapID")
  mapSex <<- getInput("mapSex")
  mapDrug <<- getInput("mapDrug")
  mapType <<- getInput("mapType")
  mapVariable <<- getInput("mapVariable")
  mapValue <<- getInput("mapValue")
  mapLitter <<- getInput("mapLitter")
}

getInput <- function(variableName){
  ## This function is just for easy readability of the task of 
  ## assigning study-specific variables within the .Rmd that are needed for the 
  ## analysis. The source of the information is 
  ## the spreadsheet created by the analyst before a continuous-time vag cyt analysis
  ## is begun (at t.o.w. this spreadsheet is called 'analysis-input-variables.xlsx'. 
  ## 
  ## This function relies on that spreadsheet already having been read in
  ## to the Global Environment as a variable called "inputs"
  retval = inputs[which(inputs$Variable==variableName),]$Value
  if(is.na(retval)){retval = ""}
  return(retval)
}

mapNames <- function(data){
  output <- data %>%
    rename(PHnumber = mapID,
           Sex = mapSex,
           Drug = mapDrug,
           `Parameter Type` = mapType,
           `Parameter and Unit` = mapVariable,
           Value = mapValue)
  if(nchar(mapLitter)>0){
    output <- output %>%
      rename(Litter = mapLitter)
  }
  
  return(output)
}

# color_scheme is a variable whose values are colors
plotLoadings <- function(data, PC_num, color_scheme, title_string) {
  List_Index = which(unlist(lapply(Colors_List[[1]], function(x) x==color_scheme))) # Pulls the Index to use for the color legend labels
  color_fill = Colors_List[[2]][[List_Index]]
  color_names = Colors_List[[3]][[List_Index]]
  
  plot_set = data[,c(paste("PC", PC_num, sep = ""), "Variable", color_scheme)] 
  names(plot_set) = c("Loading", "Variable", color_scheme)
  plot_set %>%
    ggplot(aes(x = Loading, y = reorder(Variable, Loading), fill = get(color_scheme))) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Variable), size = 1.75, hjust = "inward") +
    scale_fill_identity(guide = "legend", labels = color_names) +
    guides(fill = guide_legend(
      title = "",
      override.aes = list(fill = c(color_fill))
    )) +
    theme(axis.text.y = element_blank(),
          legend.position = "bottom") + 
    labs(title = paste("Loading Plot for PC ", PC_num, ", which explains ", title_string, "% of variation", sep = ""),
         x = "Loading Magnitude",
         y = "Variable")
}

plotScores = function(data, PC_num, color_scheme, title_string){
  List_Index = which(unlist(lapply(Colors_List[[1]], function(x) x==color_scheme))) # Pulls the Index to use for the color legend labels
  color_fill = Colors_List[[2]][[List_Index]]
  color_names = Colors_List[[3]][[List_Index]]
  
  plot_set_int <- data %>%
    mutate(Drug = factor(Drug, levels = doseLevels)) 
  plot_set = plot_set_int[,c(paste("PC", PC_num, sep = ""), "Drug", "Sex", color_scheme)] 
  names(plot_set) = c("Score", "Drug", "Sex", color_scheme)
  
  plot_set %>%
    ggplot(aes(x = Drug, y = Score, fill = get(color_scheme))) +
    facet_wrap("Sex") +
    geom_abline(slope = 0, intercept = 0) +
    geom_boxplot() +
    geom_point() +
    scale_fill_identity(guide = "legend", labels = color_names) + 
    guides(fill = guide_legend(
      title = color_scheme,
      override.aes = list(fill = c(color_fill))
    )) +
    theme(legend.position = "bottom") +
    labs(title = paste("Score Plot for PC ", PC_num, ", which explains ", title_string, "% of variation", sep = ""),
         x = "",
         y = "Score")
}

plotScoresLitter = function(data, PC_num, title_string){
  
  plot_set = data[,c(paste("PC", PC_num, sep = ""), "Litter", "Sex", "Drug")] 
  names(plot_set) = c("Score", "Litter", "Sex", "Drug")
  
  plot_set %>%
    mutate(Litter = fct_reorder(Litter, desc(desc(Drug)))) %>%
    ggplot(aes(x = Litter, y = Score, color = Drug)) +
    geom_abline(slope = 0, intercept = 0) +
    geom_point(aes(shape = Sex), size = 3) + 
    theme(legend.position = "bottom") +
    labs(title = paste("Score Plot for PC ", PC_num, ", which explains ", title_string, "% of variation", sep = ""),
         subtitle = "Litter",
         x = "",
         y = "Score")
}

shortenParameters = function(data){
  output <- data %>%
    mutate(int1a = str_replace(`Parameter and Unit`, "\\(.*?\\)", ""),
           int1b = str_replace(int1a, "\\)", ""),
           int2 = str_replace(int1b, "Mean", "M."),
           int3 = str_replace(int2, "darklight", "DL"),
           int4 = str_replace(int3, "light", "L"),
           int5 = str_replace(int4, "dark", "D"),
           int6 = str_replace(int5, "habituation", "hab."),
           int7 = str_replace(int6, "number", "#"),
           int8 = str_replace(int7, "movement", "mvt."),
           int9 = str_replace(int8, "movement", "mvt."),
           int10 = str_replace(int9, "duration", "dur."),
           int11 = str_replace(int10, "of ", ""),
           int12 = str_replace(int11, "anticipation", "ant. of"),
           `Parameter Short` = str_trim(int12)
           ) %>%
    dplyr::select(-c(int1a, int1b, int2, int3, int4, int5, int6, int7, int8, int9, int10, int11, int12))
  
  return(output)
}


adjustValuesForLitter <- function(data){
  # Determine which model to use
  analysis_set <- data %>%
    dplyr::select(all_of(c("Sex", "Drug", "Value", "Parameter and Unit", "Litter"))) %>%
    rename("COVAR1" = all_of("Sex"),
           "COVAR2" = all_of("Drug"),
           "RESPONSE" = all_of("Value"),
           "GROUP" = all_of("Parameter and Unit")) %>%
    group_by(GROUP) %>%
    nest() %>%
    mutate(int_aov = map(data, ~anova(lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x))),
           int_model = map(data, ~lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x)),
           re_int = map(int_model, ~ as.data.frame(random.effects(.x)) %>% mutate(Litter = rownames(random.effects(.x)))),
           var_int = map(int_model, ~VarCorr(.x)),
           perRandom_int = 100*as.numeric(var_int[[1]][1, "Variance"])/(as.numeric(var_int[[1]][1, "Variance"]) + as.numeric(var_int[[1]][2, "Variance"]))
    )
  # Pull out the interaction p-value
  p = c()
  for(i in 1:nrow(analysis_set)) {
    int_p = analysis_set[[3]][[i]][4,4] # This is the p-value of the interaction term
    p = c(p, int_p)
  }
  analysis_set$int_p = p
  
  percent_litter <- analysis_set %>%
    dplyr::select(GROUP, perRandom_int)
  
  analysis_set_2 <- analysis_set %>%
    unnest(re_int) %>%
    dplyr::select(GROUP, `(Intercept)`, Litter, perRandom_int, int_p) %>%
    rename("Int_RE" = "(Intercept)"
    ) %>%
    # Use the interaction model adjustments if appropriate
    # Use the additive model adjustments otherwise
    # If the percent of variation explained by the random effect (from the appropriate model) is at least 1%, then use the adjustment. 
    # Otherwise, no adjustment (adjustment = 0)
    mutate(Adjustment = case_when(perRandom_int>1 ~ Int_RE,
                                  perRandom_int<=1 ~ 0)) %>%
    dplyr::select(GROUP, Litter, Adjustment)
  
  full_adj <- full %>%
    merge(analysis_set_2, by.x = c("Parameter and Unit", "Litter"), by.y = c("GROUP", "Litter"), all.x = TRUE) %>%
    mutate(Value_Adj = Value - Adjustment) %>%
    dplyr::select(-Value) %>%
    rename("Value" = "Value_Adj")
  
  return(list(full_adj, percent_litter))
  
}


adjustValuesForLitter_addAndInt <- function(data){
  # Determine which model to use
  analysis_set <- data %>%
    dplyr::select(all_of(c("Sex", "Drug", "Value", "Parameter and Unit", "Litter"))) %>%
    rename("COVAR1" = all_of("Sex"),
           "COVAR2" = all_of("Drug"),
           "RESPONSE" = all_of("Value"),
           "GROUP" = all_of("Parameter and Unit")) %>%
    group_by(GROUP) %>%
    nest() %>%
    mutate(int_aov = map(data, ~anova(lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x))),
           add_aov = map(data, ~anova(lme(RESPONSE ~ COVAR1 + COVAR2, random=~1|Litter, data=.x))),
           int_model = map(data, ~lme(RESPONSE ~ COVAR1 * COVAR2, random=~1|Litter, data=.x)),
           add_model = map(data, ~lme(RESPONSE ~ COVAR1 + COVAR2, random=~1|Litter, data=.x)),
           re_int = map(int_model, ~ as.data.frame(random.effects(.x)) %>% mutate(Litter = rownames(random.effects(.x)))),
           re_add = map(add_model, ~ as.data.frame(random.effects(.x)) %>% mutate(Litter = rownames(random.effects(.x)))),
           var_int = map(int_model, ~VarCorr(.x)),
           var_add = map(add_model, ~VarCorr(.x)),
           perRandom_int = 100*as.numeric(var_int[[1]][1, "Variance"])/(as.numeric(var_int[[1]][1, "Variance"]) + as.numeric(var_int[[1]][2, "Variance"])),
           perRandom_add = 100*as.numeric(var_add[[1]][1, "Variance"])/(as.numeric(var_add[[1]][1, "Variance"]) + as.numeric(var_add[[1]][2, "Variance"]))
    )
  # Pull out the interaction p-value
  p = c()
  for(i in 1:nrow(analysis_set)) {
    int_p = analysis_set[[3]][[i]][4,4] # This is the p-value of the interaction term
    p = c(p, int_p)
  }
  analysis_set$int_p = p
  
  analysis_set_2 <- analysis_set %>%
    unnest(re_int, re_add) %>%
    dplyr::select(GROUP, `(Intercept)`, Litter, `(Intercept)1`, perRandom_int, perRandom_add, int_p) %>%
    rename("Int_RE" = "(Intercept)",
           "Add_RE" = "(Intercept)1"
    ) %>%
    # Use the interaction model adjustments if appropriate
    # Use the additive model adjustments otherwise
    # If the percent of variation explained by the random effect (from the appropriate model) is at least 1%, then use the adjustment. 
    # Otherwise, no adjustment (adjustment = 0)
    mutate(Adjustment = case_when(int_p<0.05 & perRandom_int>1 ~ Int_RE,
                                  int_p<0.05 & perRandom_int<=1 ~ 0,
                                  int_p>=0.05 & perRandom_add>1 ~ Add_RE,
                                  int_p>= 0.05 & perRandom_add<=1 ~ 0)) %>%
    dplyr::select(GROUP, Litter, Adjustment)
  
  full_adj <- full %>%
    merge(analysis_set_2, by.x = c("Parameter and Unit", "Litter"), by.y = c("GROUP", "Litter"), all.x = TRUE) %>%
    mutate(Value_Adj = Value - Adjustment) %>%
    dplyr::select(-Value) %>%
    rename("Value" = "Value_Adj")
  
  return(full_adj)
  
}