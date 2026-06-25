# --------------------------------------
#
# Correlations
#
# --------------------------------------


# Doing things together across sex and exposure
corr_matrix = cor(wide_logged_for_corr, use = "pairwise.complete.obs")
Hc = findCorrelation(corr_matrix, cutoff = correlationCutoff, verbose = FALSE)
Hc = sort(Hc)
if(ncol(wide_logged_for_corr[,Hc]) == 0){
  PCA_set = wide_logged_for_corr
} else {
  PCA_set = wide_logged_for_corr[,-c(Hc)]
}


#----------------------#
# Heatmap of variables #
#----------------------#

# For axis lines
n_parameter <- full %>%
  group_by(`Parameter and Unit`, `Parameter Type`) %>%
  summarize(n = n()) %>%
  group_by(`Parameter Type`) %>%
  summarize(n_per = n()) %>%
  ungroup() %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  arrange(Parameter_Order) %>%
  mutate(axis_mark = cumsum(n_per)+0.5)

# For axis lines
n_animals <- full %>%
  group_by(Sex, Drug, PHnumber) %>%
  summarize(n = n()) %>%
  group_by(Sex, Drug) %>%
  summarize(n_per = n()) %>%
  ungroup() %>%
  arrange(Sex, Drug) %>%
  mutate(axis_mark = cumsum(n_per) + 0.5)

Heatmap <- full %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  group_by(`Parameter and Unit`) %>%
  mutate(`Z-Score` = scale(Value, center = TRUE, scale = TRUE),
         Drug_PHnumber = paste(Drug, PHnumber, sep = "_"),
         Sex_Drug_PHnumber = case_when(Sex == "Male" ~ paste("M_", Drug_PHnumber, sep = ""),
                                  Sex == "Female" ~ paste("F_", Drug_PHnumber, sep = "")),
         Parameter_and_Unit_Ordered = paste(Parameter_Order, `Parameter Short`)
         ) %>%
  ungroup() %>%
  ggplot(aes(x = Parameter_and_Unit_Ordered, y = Sex_Drug_PHnumber, fill = `Z-Score`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "grey",
    high = "red",
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  geom_vline(xintercept = n_parameter$axis_mark, color = "#444444") +
  geom_hline(yintercept = n_animals$axis_mark, color = "#444444") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
    axis.text.y = element_text(size = 4)
  ) +
  labs(
    title = paste("Heatmap of ", titleName, sep = ""),
    x = "Parameter",
    y = "Animal"
  )

Heatmap_tilted <- full %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  group_by(`Parameter and Unit`) %>%
  mutate(`Z-Score` = scale(Value, center = TRUE, scale = TRUE),
         Drug_PHnumber = paste(Drug, PHnumber, sep = "_"),
         Sex_Drug_PHnumber = case_when(Sex == "Male" ~ paste("M_", Drug_PHnumber, sep = ""),
                                       Sex == "Female" ~ paste("F_", Drug_PHnumber, sep = "")),
         Parameter_and_Unit_Ordered = paste(Parameter_Order, `Parameter Short`)
  ) %>%
  ungroup() %>%
  ggplot(aes(y = Parameter_and_Unit_Ordered, x = Sex_Drug_PHnumber, fill = `Z-Score`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "grey",
    high = "red",
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  geom_hline(yintercept = n_parameter$axis_mark, color = "#444444") +
  geom_vline(xintercept = n_animals$axis_mark, color = "#444444") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
    axis.text.y = element_text(size = 5)
  ) +
  labs(
    title = paste("Heatmap of ", titleName, sep = ""),
    subtitle = "Females on the left, Males on the right",
    y = "Parameter",
    x = "Animal"
  )

#---------------------------------------------------#
# Heatmap of Average z-score of variables per group #
#---------------------------------------------------#

Heatmap_avg <- full %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  group_by(`Parameter and Unit`) %>%
  mutate(`Z-Score` = scale(Value, center = TRUE, scale = TRUE),
         Drug_PHnumber = paste(Drug, PHnumber, sep = "_"),
         Sex_Drug_PHnumber = case_when(Sex == "Male" ~ paste("M_", Drug_PHnumber, sep = ""),
                                       Sex == "Female" ~ paste("F_", Drug_PHnumber, sep = "")),
         Parameter_and_Unit_Ordered = paste(Parameter_Order, `Parameter Short`)
  ) %>%
  ungroup() %>%
  group_by(Sex, Drug, Parameter_and_Unit_Ordered) %>%
  summarize(`Avg. Z-Score` = mean(`Z-Score`)) %>%
  ungroup() %>%
  mutate(Group = case_when(Sex == "Male" ~ paste("Male ", Drug, sep = ""),
                           Sex == "Female" ~ paste("Female ", Drug, sep = ""))) %>%
  ggplot(aes(x = Parameter_and_Unit_Ordered, y = Group, fill = `Avg. Z-Score`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "grey",
    high = "red",
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  geom_vline(xintercept = n_parameter$axis_mark, color = "#444444") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
    axis.text.y = element_text(size = 4)
  ) +
  labs(
    title = paste("Heatmap of ", titleName, sep = ""),
    x = "Parameter",
    y = "Animal"
  )

Heatmap_avg_tilted <- full %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  group_by(`Parameter and Unit`) %>%
  mutate(`Z-Score` = scale(Value, center = TRUE, scale = TRUE),
         Drug_PHnumber = paste(Drug, PHnumber, sep = "_"),
         Sex_Drug_PHnumber = case_when(Sex == "Male" ~ paste("M_", Drug_PHnumber, sep = ""),
                                       Sex == "Female" ~ paste("F_", Drug_PHnumber, sep = "")),
         Parameter_and_Unit_Ordered = paste(Parameter_Order, `Parameter Short`)
  ) %>%
  ungroup() %>%
  group_by(Sex, Drug, Parameter_and_Unit_Ordered) %>%
  summarize(`Avg. Z-Score` = mean(`Z-Score`)) %>%
  ungroup() %>%
  mutate(Group = case_when(Sex == "Male" ~ paste("Male ", Drug, sep = ""),
                           Sex == "Female" ~ paste("Female ", Drug, sep = ""))) %>%
  ggplot(aes(y = Parameter_and_Unit_Ordered, x = Group, fill = `Avg. Z-Score`)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "grey",
    high = "red",
    midpoint = 0,
    limits = c(-3, 3)
  ) +
  geom_hline(yintercept = n_parameter$axis_mark, color = "#444444") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    axis.text.y = element_text(size = 5)
  ) +
  labs(
    title = paste("Heatmap of ", titleName, sep = ""),
    y = "Parameter",
    x = "Animal"
  )

#-------------------------#
# Heatmap with clustering #
#-------------------------#


for_heatmap <- t(wide_logged[,5:ncol(wide_logged)])
colnames(for_heatmap) = wide_logged$PHnumber

cols = colorRampPalette(c("blue", "white", "red"))(100)

# parameters will be the columns
param_df <- Parameter_Metadata %>%
  filter(`Parameter and Unit` %in% rownames(for_heatmap))
row.names(param_df) = param_df$`Parameter and Unit`
param_df <- param_df %>% dplyr::select(`Parameter Type`)

# sex/Drug group will be the rows
group_df = as.data.frame(paste0(paste(wide_logged$Sex, ""), wide_logged$Drug))
row.names(group_df) = colnames(for_heatmap)
colnames(group_df) = "Group"
group_sorted <- unique(group_df) %>% arrange(Group)
group_colors <- c(colorRampPalette(c("pink", "darkred"))(nrow(group_sorted)/2),
                  colorRampPalette(c("lightblue", "darkblue"))(nrow(group_sorted)/2)
                  
)
names(group_colors) <- group_sorted[[1]]

ann_colors <- list(
  Group = group_colors,
  `Parameter Type` = c("Phase Transition Pattern" = "#FF9138",
                       "Kinematic"="#AB08FC",
                       "Activity Bouts"= "#3671FD",
                       "Sheltering"= "#1BE5B1",
                       "Habituation"= "#787878",
                       "DarkLight Index"= "#A00922")
)

Cluster_Heatmap  <- pheatmap(for_heatmap,
                             cluster_rows = T, cluster_cols = T,
                             annotation_col = group_df,
                             annotation_row = param_df,
                             annotation_colors = ann_colors,
                             color = cols,
                             legend_breaks=c(-4, 0, 4),
                             scale="row",
                             fontsize_col=6,
                             fontsize_row=6,
                             border_color="grey60",
                             silent=TRUE)


###################
# Average Heatmap #
###################

cluster_set_int_avg <- full %>%
  merge(Parameter_Order, by = "Parameter Type", all.x = TRUE) %>%
  group_by(`Parameter and Unit`) %>%
  mutate(`Z-Score` = scale(Value, center = TRUE, scale = TRUE),
         Drug_PHnumber = paste(Drug, PHnumber, sep = "_"),
         Sex_Drug_PHnumber = case_when(Sex == "Male" ~ paste("M_", Drug_PHnumber, sep = ""),
                                       Sex == "Female" ~ paste("F_", Drug_PHnumber, sep = "")),
         Parameter_and_Unit_Ordered = paste(Parameter_Order, `Parameter Short`)
  ) %>%
  ungroup() %>%
  group_by(Sex, Drug, `Parameter and Unit`) %>%
  summarize(`Avg. Z-Score` = mean(`Z-Score`)) %>%
  ungroup() %>%
  mutate(Group = case_when(Sex == "Male" ~ paste("Male ", Drug, sep = ""),
                           Sex == "Female" ~ paste("Female ", Drug, sep = ""))) %>%
  dplyr::select(Group, `Parameter and Unit`, `Avg. Z-Score`) %>%
  pivot_wider(names_from = Group, values_from = `Avg. Z-Score`)


cluster_set_avg <- data.matrix(cluster_set_int_avg)
rownames(cluster_set_avg) <- cluster_set_int_avg$`Parameter and Unit`
cluster_set_avg <- cluster_set_avg[,2:ncol(cluster_set_avg)]


# parameters will be the columns
param_df2 <- Parameter_Metadata %>%
  filter(`Parameter and Unit` %in% rownames(cluster_set_avg))
row.names(param_df2) = param_df2$`Parameter and Unit`
param_df2 <- param_df2 %>% dplyr::select(`Parameter Type`)

row.names(group_sorted) = group_sorted$Group

Cluster_Heatmap_Avg  <- pheatmap::pheatmap(cluster_set_avg,
                                           color = cols,
                                           annotation_col = group_sorted,
                                           annotation_row = param_df2,
                                           annotation_colors = ann_colors,
                                           legend_breaks=c(-0.5, 0, 0.5),
                                           fontsize_col=6,
                                           fontsize_row=6,
                                           border_color=NA,
                                           #display_numbers = display_nums,
                                           #number_color = "black",
                                           #fontsize_number = 6,
                                           silent = TRUE)

# -------------------------- #
# Prepare dataset for export #
# -------------------------- #

metadata = wide_logged %>%
  dplyr::select(PHnumber, Sex, Drug)

PCA_set_meta <- cbind(metadata, PCA_set)

# --------------------------------------------- #
# Export correlation matrix and reduced dataset #
# --------------------------------------------- #

corr_matrix_export = as.data.frame(corr_matrix) %>%
  mutate(var2 = rownames(corr_matrix)) %>%
  relocate(var2)

corr_matrix_export_long = corr_matrix_export %>%
  pivot_longer(cols = !var2, names_to = "var1", values_to = "correlation")

corr_matrix_export_long_dark = corr_matrix_export_long %>%
  dplyr::filter(grepl("dark", var1), grepl("dark", var2))

corr_matrix_export_long_light = corr_matrix_export_long %>%
  dplyr::filter(grepl("light", var1), grepl("light", var2))

corr_matrix_wide_dark = corr_matrix_export_long_dark %>%
  pivot_wider(names_from = var2, values_from = correlation)

corr_matrix_wide_light = corr_matrix_export_long_light %>%
  pivot_wider(names_from = var2, values_from = correlation)

sheets = list(
  "Full Wide" = corr_matrix_export,
  "Full Long" = corr_matrix_export_long,
  "Dark Wide" = corr_matrix_wide_dark,
  "Dark Long" = corr_matrix_export_long_dark,
  "Light Wide" = corr_matrix_wide_light,
  "Light Long" = corr_matrix_export_long_light
)

write_xlsx(sheets, path = paste(outputPath, titleName, "_correlation_matrix_", Sys.Date(), ".xlsx", sep = ""))
# --------- End of Code ---------- #