
#-------------------------#
# PCA                     #
#-------------------------#

PC <- prcomp(PCA_set, scale = TRUE, center = TRUE)

#-------------------------#
# Percent of variability  #
#-------------------------#

PC.eigenvalues <- PC$sdev^2
var_explained <- PC.eigenvalues / sum(PC.eigenvalues)

PC.description <- data.frame(
  PC = paste0("PC",1:length(var_explained)),
  PC_sort = 1:length(var_explained),
  Percent_Var_Explained = 100*var_explained
)

#-------------------------#
# Loadings                #
#-------------------------#

PC.loadings <- as.data.frame(PC$rotation)
PC.loadings <- PC.loadings %>%
  mutate(Variable = row.names(PC.loadings)) %>% 
  merge(Parameter_Metadata, by.x = "Variable", by.y = "Parameter and Unit", all.x = TRUE)

#-------------------------#
# Scores and ANOVA        #
#-------------------------#

PC.scores <- as.data.frame(cbind(PCA_set_meta[,1:3], PC$x)) %>%
  merge(Animal_Metadata, by = c("PHnumber", "Sex", "Drug"), all.x = TRUE)
PC.scores_long <- PC.scores %>%
  pivot_longer(cols = PC1:PC48, names_to = "PC", values_to = "Score")

PC.ANOVA <- applyANOVA(PC.scores_long,
           "Sex",
           "Drug",
           "Score",
           "PC",
           nchar(mapLitter)>0
           )
PC.description <- PC.description %>% 
  merge(PC.ANOVA, by = "PC") %>%
  arrange(PC_sort)


PC.pairwise <- applyPairwise(PC.scores_long, PC.description, "PC", "Score")

