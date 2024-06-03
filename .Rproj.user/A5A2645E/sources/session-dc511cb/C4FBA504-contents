library(tidyverse)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

load("output/Rdata/joined_all.Rdata")

load("output/Rdata/comparison_data.Rdata")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ F-UJI, FAIR Enough, FAIR-Checker overview
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
joined$repository[joined$repository == "BioImages"] <- "EMBL-EBI BioStudies"
joined$repository[joined$repository == "BioImage"] <- "EMBL-EBI BioStudies"
joined$repository[joined$repository == "EMDB"] <- "EMBL-EBI EMDB"
joined$repository[joined$repository == "EMPIAR"] <- "EMBL-EBI EMPIAR"
joined$repository[joined$repository == "ENA"] <- "EMBL-EBI ENA"
joined$repository[joined$repository == "ArrayExpress"] <- "EMBL-EBI ArrayExpress"
joined$repository[joined$repository == "GWAS Catalog"] <- "EMBL-EBI GWAS Catalog"
joined$repository[joined$repository == "MetaboLights"] <- "EMBL-EBI MetaboLights"
joined$repository[joined$repository == "PRoteomics IDEntifications Database"] <- "EMBL-EBI PRoteomics IDEntifications Database"
joined$repository[joined$repository == "SRA"] <- "NCBI Sequence Read Archive"
joined$repository[joined$repository == "NCBI Trace Archive"] <- "NCBI Sequence Read Archive"
joined$repository[joined$repository == "GEO"] <- "NCBI GEO"
joined$repository[joined$repository == "GenBank"] <- "NCBI GenBank"
joined$repository[joined$repository == "BioProject"] <- "NCBI BioProject"

fair_checker_summary_wide_full <- fair_checker_summary_wide_full |>
  rename(checker_F1A = F1A, 
         checker_F1B = F1B,
         checker_F2A = F2A, 
         checker_F2B = F2B,
         checker_A1.1 = A1.1,
         checker_A1.2 = A1.2,
         checker_I1 = I1, 
         checker_I2 = I2, 
         checker_I3 = I3, 
         checker_R1.1 = R1.1,
         checker_R1.2 = R1.2, 
         checker_R1.3 = R1.3)
summary <- results_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = round(sd(value), digits = 2),
            count = n())

fuji_missing_enough <- fuji_results_all[!fuji_results_all$best_identifier %in% fair_enough_df_full$ID,]
missing_checker <- sum(is.na(fair_checker_summary_wide_full$fair_checker_score)) #7

missing_all <- c("doi.org/10.17605/osf.io/gs2t5", # accessible
                 "https://flowrepository.org/id/FR-FCM-Z3G7", # accessible
                 "10.5281/zenodo.4454489", # accessible
                 "doi.org/10.17605/OSF.IO/BTWNQ", # accessible
                 "doi.org/10.11588/data/KXTQTZ", # accessible 
                 "10.5281/zenodo.4640140", # accessible 
                 "doi.org/10.5281/zenodo.4327115") # accessible

joined_cleaned <- joined[!joined$id %in% missing_all,] # 467
joined_cleaned_long <- joined_cleaned |>
  pivot_longer(cols = c("fuji_percent", "enough_score_metrics_percent", "fair_checker_score"),
               names_to = "assessment_tool",
               values_to = "score")

write.csv2(joined_cleaned, "./output/csv/df_all_tools_joined_cleaned.csv", row.names = F)

summary_cleaned <- joined_cleaned_long |>
  drop_na(score) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())

paired_palette = brewer.pal(n = 8, name = "Paired")
#"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"

dark_shades <- paired_palette[c(2, 6)] 
#"#1F78B4" "#E31A1C"

ggplot(joined_cleaned_long, aes(x = assessment_tool, 
                                y = score,
                                fill = assessment_tool)) +
  geom_violin(trim = FALSE, 
              colour = "#555555",
              draw_quantiles = c(0.25, 0.75)) +
  #geom_jitter(height = 0, width = 0.1, ) +
  stat_summary(fun = "mean",
               geom = "point", size = 3,
               aes(color = "Mean")) +
  stat_summary(fun = "median",
               geom = "point", size = 3,
               aes(color = "Median")) +
  scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
                      name = "") +
  labs(y = "Score in %", x = "") +
  scale_fill_manual(values = c(paired_palette[1], paired_palette[3], paired_palette[5]), 
                    guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "bottom") +
  scale_x_discrete(labels = c("fuji_percent" = "FUJI", 
                              "enough_score_metrics_percent" = "FAIR Enough", 
                              "fair_checker_score" = "FAIR Checker"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ General vs. discipline specific repos
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summary_grouped_repository <- joined_cleaned_long |>
  drop_na(score) |>
  group_by(assessment_tool, repository_type) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())

articles_general <- joined_cleaned |>
  filter(repository_type == "general-purpose repository") # 187

write.csv2(articles_general, "./output/csv/articles_general_all_tools.csv", row.names = F)

articles_specific <- joined_cleaned |>
  filter(repository_type == "disciplinary repository") # 280

write.csv2(articles_specific, "./output/csv/articles_disciplinary_all_tools.csv", row.names = F)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ General
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
articles_general_long <- joined_cleaned_long |>
  filter(repository_type == "general-purpose repository")

ggplot(articles_general_long, aes(x = assessment_tool, 
                                y = score,
                                fill = assessment_tool)) +
  geom_violin(trim = FALSE, 
              colour = "#555555",
              draw_quantiles = c(0.25, 0.75)) +
  #geom_jitter(height = 0, width = 0.1, ) +
  stat_summary(fun = "mean",
               geom = "point", size = 3,
               aes(color = "Mean")) +
  stat_summary(fun = "median",
               geom = "point", size = 3,
               aes(color = "Median")) +
  scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
                      name = "") +
  labs(y = "Score in %", x = "") +
  scale_fill_manual(values = c(paired_palette[1], paired_palette[3], paired_palette[5]), 
                    guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "bottom") +
  scale_x_discrete(labels = c("fuji_percent" = "FUJI", 
                              "enough_score_metrics_percent" = "FAIR Enough", 
                              "fair_checker_score" = "FAIR Checker"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Specific
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
articles_specific_long <- joined_cleaned_long |>
  filter(repository_type == "disciplinary repository") # 280

ggplot(articles_specific_long, aes(x = assessment_tool, 
                                  y = score,
                                  fill = assessment_tool)) +
  geom_violin(trim = FALSE, 
              colour = "#555555",
              draw_quantiles = c(0.25, 0.75)) +
  stat_summary(fun = "mean",
               geom = "point", size = 3,
               aes(color = "Mean")) +
  stat_summary(fun = "median",
               geom = "point", size = 3,
               aes(color = "Median")) +
  scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
                      name = "") +
  labs(y = "Score in %", x = "") +
  scale_fill_manual(values = c(paired_palette[1], paired_palette[3], paired_palette[5]), 
                    guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        legend.position = "bottom") +
  scale_x_discrete(labels = c("fuji_percent" = "FUJI", 
                              "enough_score_metrics_percent" = "FAIR Enough", 
                              "fair_checker_score" = "FAIR Checker"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Sample of articles
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
selected_articles <- joined_cleaned |>
  #filter(!is.na(fair_checker_score)) |>
  group_by(repository) |>
  sample_n(1) |>
  ungroup() # 44

save(selected_articles, file = "./output/csv/selected_articles_df.Rdata")
write.csv2(selected_articles, file = "./output/csv/selected_articles_df.csv", row.names = F)


fair_enough_overall_vs_indiv <- fair_enough_df_full[fair_enough_df_full$ID %in% selected_articles$id,]
fair_enough_overall_vs_indiv_compare <- fair_enough_overall_vs_indiv |>
  select(c(ID, enough_score_percent, enough_score_metrics_percent))

selected_long <-
  selected_articles |> pivot_longer(cols = c("fuji_percent", "enough_score_metrics_percent", "fair_checker_score"),
                         names_to = "assessment_tool",
                         values_to = "score")

summary_selected <- selected_long |>
  drop_na(score) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())

create_assessment_tool_plot <- function(tool_name, data) {
  plot <- ggplot(data, aes(x = repository_type, y = score, fill = repository_type)) +
    geom_violin(trim = FALSE, 
                colour = "#555555",
                draw_quantiles = c(0.25, 0.75)) +
    stat_summary(fun = "mean",
                 geom = "point", size = 3,
                 aes(color = "Mean")) +
    stat_summary(fun = "median",
                 geom = "point", size = 3,
                 aes(color = "Median")) +
    scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
                        name = "") +
    scale_fill_manual(values = c(paired_palette[1], paired_palette[3]), 
                      guide = "none") +
    labs(y = "", x = "", title = paste("FAIRness for", tool_name)) + 
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 1),
          legend.position = "none") + 
    facet_wrap(~assessment_tool, scales = "free_y", nrow = 1)  # Use fixed scale for y-axis
  return(plot)
}

plot_fuji <- create_assessment_tool_plot("FUJI", selected_long[selected_long$assessment_tool == "fuji_percent", ])
plot_enough <- create_assessment_tool_plot("FAIR Enough", selected_long[selected_long$assessment_tool == "enough_score_metrics_percent", ])
plot_checker <- create_assessment_tool_plot("FAIR Checker", selected_long[selected_long$assessment_tool == "fair_checker_score", ])

combined_plots <- (plot_fuji | plot_enough | plot_checker ) + plot_layout(ncol = 4) 
combined_plots

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Sample for general and spec
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sample_general <- selected_articles |>
  filter(repository_type == "general-purpose repository") # 15

sample_specific <- selected_articles |>
  filter(repository_type == "disciplinary repository") # 29



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Overlapping indicators
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# F1, F2, I1, I2, I3, R1.1

fuji_cleaned <- fuji_results_all[fuji_results_all$best_identifier %in% joined_cleaned$id,]

df_fuji_overlapping <- fuji_cleaned |>
  select(c(article, best_identifier, repository,repository_type, 
           fuji_percent_f1, fuji_percent_f2, fuji_percent_i1, fuji_percent_i2, fuji_percent_i3,
           `FsF-F1-01D`, `FsF-F1-02D`, `FsF-F2-01M`,
           `FsF-I1-01M`, `FsF-I2-01M`, `FsF-I3-01M`, `FsF-R1.1-01M`
            ))|>
  mutate(fuji_score_overlap = round(((`FsF-F1-01D`+ `FsF-F1-02D`+`FsF-F2-01M`+
                            `FsF-I1-01M`+`FsF-I2-01M`+`FsF-I3-01M`+`FsF-R1.1-01M`))/7, digits = 2))

enough_cleaned <- fair_enough_df_full[fair_enough_df_full$ID %in% joined_cleaned$id,]
df_enough_overlapping <- enough_cleaned |>
  select(c(ID,
           fair_enough_f1, fair_enough_f2, fair_enough_i1, fair_enough_i2, fair_enough_i3, fair_enough_r1.1,
           enough_Gen2_FM_F1A, enough_Gen2_FM_F1B_metadata, enough_Gen2_FM_F2A, enough_Gen2_FM_F2B,
           enough_Gen2_FM_I1A_metadata, enough_Gen2_FM_I1B_metadata, 
           enough_Gen2_FM_I1A_data, enough_Gen2_FM_I1B_data, enough_Gen2_FM_I2A, enough_Gen2_FM_I2B,
           enough_Gen2_FM_I3, enough_Gen2_FM_R1.1_metadata, enough_Gen2_FM_R1.1_data)) |>
  mutate(enough_score = round((enough_Gen2_FM_F1B_metadata+enough_Gen2_FM_F1A + 
                                 enough_Gen2_FM_F2A + enough_Gen2_FM_F2B +
                                 enough_Gen2_FM_I1A_metadata+ enough_Gen2_FM_I1B_metadata+
                                 enough_Gen2_FM_I1A_data+ enough_Gen2_FM_I1B_data+ 
                                 enough_Gen2_FM_I2A+ enough_Gen2_FM_I2B+
                                 enough_Gen2_FM_I3+ enough_Gen2_FM_R1.1_metadata+ 
                                 enough_Gen2_FM_R1.1_data)/13*100, digits = 2))

checker_cleaned <- fair_checker_summary_wide_full[fair_checker_summary_wide_full$target_uri %in% joined_cleaned$id,]
df_checker_overlapping <- checker_cleaned |>
  select(c(fair_checker_f1, fair_checker_f2, fair_checker_i1, fair_checker_i2, fair_checker_i3, fair_checker_r1.1,
           checker_F1A, checker_F1B, checker_F2A, checker_F2B, checker_I1, checker_I2, checker_I3, checker_R1.1)) |>
  mutate(checker_score = round((checker_F1A+checker_F1B+
                                checker_F2A+checker_F2B+
                                checker_I1+checker_I2+checker_I3+
                                checker_R1.1)/16*100, digits = 2))


df_overlapping <- bind_cols(df_fuji_overlapping, 
                            df_enough_overlapping, 
                            df_checker_overlapping)

overlapping_long <-
  df_overlapping |> pivot_longer(cols = c("fuji_score_overlap", "enough_score", "checker_score"),
                                 names_to = "assessment_tool",
                                 values_to = "score")
save(overlapping_long, file = "output/Rdata/all_overlapping_indicators_long.Rdata")

overlapping_summary_all <- overlapping_long |>
  drop_na(score) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())

# plot_overlap_fuji <- plot_ly(df_overlapping, y = ~fuji_score, type = "box", name = "FUJI") |>
#   layout(title = "All data sets with overlapping indicators", yaxis = list(title = "Score in %"),showlegend = FALSE)
# 
# plot_overlap_enough <- plot_ly(df_overlapping, y = ~enough_score, type = "box", name = "FAIR Enough") |>
#   layout(title = "All data sets with overlapping indicators", yaxis = list(title = "Score in %"),showlegend = FALSE)
# 
# plot_overlap_checker <- plot_ly(df_overlapping, y = ~checker_score, type = "box", name = "FAIR Checker") |>
#   layout(title = "All data sets with overlapping indicators", yaxis = list(title = "Score in %"),showlegend = FALSE)
# 
# subplot(plot_overlap_fuji, plot_overlap_enough, plot_overlap_checker,shareY = T) # plot_overlap_eva, 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Sample overlapping F-UJI, FAIR Enough, FAIR-Checker
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_overlapping$repository[df_overlapping$repository == "BioImages"] <- "EMBL-EBI BioStudies"
df_overlapping$repository[df_overlapping$repository == "BioImage"] <- "EMBL-EBI BioStudies"
df_overlapping$repository[df_overlapping$repository == "EMDB"] <- "EMBL-EBI EMDB"
df_overlapping$repository[df_overlapping$repository == "EMPIAR"] <- "EMBL-EBI EMPIAR"
df_overlapping$repository[df_overlapping$repository == "ENA"] <- "EMBL-EBI ENA"
df_overlapping$repository[df_overlapping$repository == "ArrayExpress"] <- "EMBL-EBI ArrayExpress"
df_overlapping$repository[df_overlapping$repository == "GWAS Catalog"] <- "EMBL-EBI GWAS Catalog"
df_overlapping$repository[df_overlapping$repository == "MetaboLights"] <- "EMBL-EBI MetaboLights"
df_overlapping$repository[df_overlapping$repository == "PRoteomics IDEntifications Database"] <- "EMBL-EBI PRoteomics IDEntifications Database"
df_overlapping$repository[df_overlapping$repository == "SRA"] <- "NCBI Sequence Read Archive"
df_overlapping$repository[df_overlapping$repository == "NCBI Trace Archive"] <- "NCBI Sequence Read Archive"
df_overlapping$repository[df_overlapping$repository == "GEO"] <- "NCBI GEO"
df_overlapping$repository[df_overlapping$repository == "GenBank"] <- "NCBI GenBank"
df_overlapping$repository[df_overlapping$repository == "BioProject"] <- "NCBI BioProject"

selected_articles_overlap <- df_overlapping |>
  group_by(repository) |>
  sample_n(1) |>
  ungroup() # 44

save(selected_articles_overlap, file="./output/csv/selected_articles_overlapping_indicators.Rdata")
write.csv2(selected_articles_overlap, file="./output/csv/selected_articles_overlapping_indicators.csv",
           row.names = F)

intersect_overlap_sample <- selected_articles_overlap |>
  mutate(all_equal = (fuji_score_overlap == checker_score) & (checker_score == enough_score),#14
         fuji_enough_egual =  (fuji_score_overlap == checker_score), # 38
         enough_checker_equal = (checker_score == enough_score)) # 19

overlapping_selected_long <-
  selected_articles_overlap |> pivot_longer(cols = c("fuji_score_overlap", "enough_score", "checker_score"),
                                 names_to = "assessment_tool",
                                 values_to = "score")

save(overlapping_selected_long,file="output/Rdata/selected_articles_overlapping_indicators_long.Rdata")

overlapping_selected_summary <- overlapping_selected_long |>
  drop_na(score) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())


ggplot(overlapping_selected_long, aes(x = assessment_tool, 
                                y = score,
                                fill = assessment_tool)) +
  geom_violin(trim = FALSE, 
              colour = "#555555",
              draw_quantiles = c(0.25, 0.75)) +
  stat_summary(fun = "mean",
               geom = "point", size = 3,
               aes(color = "Mean")) +
  stat_summary(fun = "median",
               geom = "point", size = 3,
               aes(color = "Median")) +
  scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
                      name = "") +
  labs(y = "Score in %", x = "") +
  scale_fill_manual(values = c(paired_palette[1], paired_palette[3], paired_palette[5]), 
                    guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_x_discrete(labels = c("fuji_score_overlap"="FUJI", 
                              "enough_score" = "FAIR Enough", 
                              "checker_score" = "FAIR-Checker"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Sample overlapping indicators
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sample_overl_indic_general <- selected_articles_overlap |>
  filter(repository_type == "general-purpose repository") # 15
overlapping_gen_selected_long <-
  sample_overl_indic_general |> pivot_longer(cols = c("fuji_score_overlap", "enough_score", "checker_score"),
                                            names_to = "assessment_tool")
overlapping_gen_selected_summary <- overlapping_gen_selected_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = sd(value),
            count = n())

plot_overlap_gen_fuji <- plot_ly(sample_overl_indic_general, y = ~fuji_score, type = "box", name = "FUJI") |>
  layout(title = "Selected data sets with overlapping indicators (general repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_gen_enough <- plot_ly(sample_overl_indic_general, y = ~enough_score, type = "box", name = "FAIR Enough") |>
  layout(title = "Selected data sets with overlapping indicators (general repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_gen_checker <- plot_ly(sample_overl_indic_general, y = ~checker_score, type = "box", name = "FAIR Checker") |>
  layout(title = "Selected data sets with overlapping indicators (general repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

subplot(plot_overlap_gen_fuji, plot_overlap_gen_enough, plot_overlap_gen_checker, shareY = T)


sample_overl_indic_specific <- selected_articles_overlap |>
  filter(repository_type == "disciplinary repository") # 29
overlapping_disc_selected_long <-
  sample_overl_indic_specific|> pivot_longer(cols = c("fuji_score", "enough_score", "checker_score"),
                                             names_to = "assessment_tool")
overlapping_disc_selected_summary <- overlapping_disc_selected_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = sd(value),
            count = n())

plot_overlap_disc_fuji <- plot_ly(sample_overl_indic_specific, y = ~fuji_score, type = "box", name = "FUJI") |>
  layout(title = "Selected data sets with overlapping indicators (discipline repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_disc_enough <- plot_ly(sample_overl_indic_specific, y = ~enough_score, type = "box", name = "FAIR Enough") |>
  layout(title = "Selected data sets with overlapping indicators (discipline repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_disc_checker <- plot_ly(sample_overl_indic_specific, y = ~checker_score, type = "box", name = "FAIR Checker") |>
  layout(title = "Selected data sets with overlapping indicators (discipline repositories)", yaxis = list(title = "Score in %"),showlegend = FALSE)

subplot(plot_overlap_disc_fuji, plot_overlap_disc_enough, plot_overlap_disc_checker, shareY = T)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Overlapping F-UJI and FAIR Enough
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# F1, F2, I1, I2, I3, R1.1
# F3, F4, A2 - not available
df_fuji <- fuji_cleaned |>
  select(c(article, best_identifier, repository,repository_type, 
           #fuji_percent_f1, fuji_percent_f2, fuji_percent_i1, fuji_percent_i2, fuji_percent_i3,
           fuji_percent_f3, fuji_percent_f4,
           #`FsF-F1-01D`, `FsF-F1-02D`, `FsF-F2-01M`,`FsF-I1-01M`, `FsF-I2-01M`, `FsF-I3-01M`, `FsF-R1.1-01M`,
           `FsF-F3-01M`, `FsF-F4-01M`,
  ))|>
  mutate(fuji_score = round(((#`FsF-F1-01D`+ `FsF-F1-02D`+ `FsF-F2-01M`+`FsF-I1-01M`+`FsF-I2-01M`+`FsF-I3-01M`+`FsF-R1.1-01M`
                              `FsF-F3-01M`+`FsF-F4-01M`))/2, digits = 2))

df_enough <- enough_cleaned |>
  select(c(ID,
           #fair_enough_f1, fair_enough_f2, fair_enough_i1, fair_enough_i2, fair_enough_i3, fair_enough_r1.1,
           fair_enough_f3, fair_enough_f4,
           #enough_Gen2_FM_F1B_data, enough_Gen2_FM_F1B_metadata, enough_Gen2_FM_F1A, enough_Gen2_FM_F2A, enough_Gen2_FM_F2B,
           #enough_Gen2_FM_I1A_metadata, enough_Gen2_FM_I1B_metadata, 
           #enough_Gen2_FM_I1A_data, enough_Gen2_FM_I1B_data, enough_Gen2_FM_I2A, enough_Gen2_FM_I2B,
           #enough_Gen2_FM_I3, enough_Gen2_FM_R1.1_metadata, enough_Gen2_FM_R1.1_data,
           enough_Gen2_FM_F3_data, enough_Gen2_FM_F3_metadata, enough_Gen2_FM_F4)) |>
  mutate(enough_score = round((#enough_Gen2_FM_F1B_data + enough_Gen2_FM_F1A + enough_Gen2_FM_F1B_metadata +
                               #enough_Gen2_FM_F2A + enough_Gen2_FM_F2B+
                                 #enough_Gen2_FM_I1A_metadata+ enough_Gen2_FM_I1B_metadata+
                                 #enough_Gen2_FM_I1A_data+ enough_Gen2_FM_I1B_data+ enough_Gen2_FM_I2A+ enough_Gen2_FM_I2B+
                                # enough_Gen2_FM_I3+ enough_Gen2_FM_R1.1_metadata+ enough_Gen2_FM_R1.1_data
                                 enough_Gen2_FM_F3_data+ enough_Gen2_FM_F3_metadata+ enough_Gen2_FM_F4)/3*100, digits = 2))


df_fuji_enough <- bind_cols(df_fuji,
                            df_enough)
fuji_enough_selected_articles <- df_fuji_enough |>
  group_by(repository) |>
  sample_n(1) |>
  ungroup() # 44

fuji_enough_long <-
  fuji_enough_selected_articles |> pivot_longer(cols = c("fuji_score", "enough_score"), 
                                 names_to = "assessment_tool")

fuji_enough_summary_all_2 <- fuji_enough_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = sd(value),
            count = n())
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Overlapping F-UJI and FAIR-Checker
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# F1, F2, I1, I2, I3, R1.1, R1.2, R1.3
df_fuji_2 <- fuji_cleaned |>
  select(c(article, best_identifier, repository,repository_type, 
           #fuji_percent_f1, fuji_percent_f2, fuji_percent_i1, fuji_percent_i2, fuji_percent_i3,
           fuji_percent_r1_2, fuji_percent_r1_3,
          # `FsF-F1-01D`, `FsF-F1-02D`, `FsF-F2-01M`, `FsF-I1-01M`, `FsF-I2-01M`,`FsF-I3-01M`, `FsF-R1.1-01M`, 
          `FsF-R1.2-01M`, `FsF-R1.3-01M`, `FsF-R1.3-02D`
  ))|>
  mutate(fuji_score = round(((#`FsF-F1-01D`+ `FsF-F1-02D`+ `FsF-F2-01M`+
                               # `FsF-I1-01M`+`FsF-I2-01M`+`FsF-I3-01M`+`FsF-R1.1-01M`+
                                `FsF-R1.2-01M`+ `FsF-R1.3-01M`+ `FsF-R1.3-02D`))/3, digits = 2))

df_checker <- checker_cleaned |>
  select(c(#fair_checker_f1, fair_checker_f2, fair_checker_i1, fair_checker_i2, fair_checker_i3, fair_checker_r1.1,
           fair_checker_r1.2, fair_checker_r1.3,
           #checker_F1A, checker_F1B, checker_F2A, checker_F2B, checker_I1, checker_I2, checker_I3, checker_R1.1,
           checker_R1.2, checker_R1.3)) |>
  mutate(checker_score = round((#checker_F1A+checker_F1B+checker_F2A+checker_F2B+
                                 # checker_I1+checker_I2+checker_I3+checker_R1.1+
                                  checker_R1.2+ checker_R1.3)/4*100, digits = 2))

df_fuji_checker <- bind_cols(df_fuji_2,
                            df_checker)
fuji_checker_selected_articles <- df_fuji_checker |>
  group_by(repository) |>
  sample_n(1) |>
  ungroup() # 44

fuji_checker_long <-
  fuji_checker_selected_articles |> pivot_longer(cols = c("fuji_score", "checker_score"), 
                                 names_to = "assessment_tool")

fuji_checher_summary_all_2 <- fuji_checker_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = sd(value),
            count = n())
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+ Overlapping FAIR Enough and FAIR-Checker
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# F1, F2, I1, I2, I3, R1.1, A1.1, A1.2

df_enough_2 <- enough_cleaned |>
  select(c(ID,
           #fair_enough_f1, fair_enough_f2, fair_enough_i1, fair_enough_i2, fair_enough_i3, fair_enough_r1.1,
           fair_enough_a1.1, fair_enough_a1.2,
           #enough_Gen2_FM_F1B_data, enough_Gen2_FM_F1B_metadata,enough_Gen2_FM_F1A, enough_Gen2_FM_F2A, enough_Gen2_FM_F2B,
           #enough_Gen2_FM_I1A_metadata, enough_Gen2_FM_I1B_metadata, 
           #enough_Gen2_FM_I1A_data, enough_Gen2_FM_I1B_data, enough_Gen2_FM_I2A, enough_Gen2_FM_I2B,
           #enough_Gen2_FM_I3, enough_Gen2_FM_R1.1_metadata, enough_Gen2_FM_R1.1_data,
           enough_Gen2_FM_A1.1_data, enough_Gen2_FM_A1.1_metadata, enough_Gen2_FM_A1.2_data, enough_Gen2_FM_A1.2_metadata)) |>
  mutate(enough_score = round((#enough_Gen2_FM_F1B_data + enough_Gen2_FM_F1B_metadata +enough_Gen2_FM_F1A + enough_Gen2_FM_F2A + enough_Gen2_FM_F2B+
    #enough_Gen2_FM_I1A_metadata+ enough_Gen2_FM_I1B_metadata+
    #enough_Gen2_FM_I1A_data+ enough_Gen2_FM_I1B_data+ enough_Gen2_FM_I2A+ enough_Gen2_FM_I2B+
    # enough_Gen2_FM_I3+ enough_Gen2_FM_R1.1_metadata+ enough_Gen2_FM_R1.1_data
    enough_Gen2_FM_A1.1_data+ enough_Gen2_FM_A1.1_metadata+ enough_Gen2_FM_A1.2_data+ enough_Gen2_FM_A1.2_metadata)/4*100, digits = 2))

df_checker_2 <- checker_cleaned |>
  select(c(#fair_checker_f1, fair_checker_f2, fair_checker_i1, fair_checker_i2, fair_checker_i3, fair_checker_r1.1,
    fair_checker_a1.1, fair_checker_a1.2,
    #checker_F1A, checker_F1B, checker_F2A, checker_F2B, checker_I1, checker_I2, checker_I3, checker_R1.1,
    A1.1, A1.2)) |>
  mutate(checker_score = round((#checker_F1A+checker_F1B+checker_F2A+checker_F2B+
    # checker_I1+checker_I2+checker_I3+checker_R1.1+
    A1.1 + A1.2)/4*100, digits = 2))

df_enough_checker <- bind_cols(df_enough_2,
                             df_checker_2)
enough_checker_selected_articles <- df_enough_checker[df_enough_checker$ID %in% fuji_checker_selected_articles$best_identifier,]

enough_checker_long <-
  enough_checker_selected_articles |> pivot_longer(cols = c("enough_score", "checker_score"), 
                                                 names_to = "assessment_tool")

enough_checher_summary_all <- enough_checker_long |>
  drop_na(value) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(value), 2),
            median = median(value),
            min = min(value),
            max = max(value),
            sd = sd(value),
            count = n())

save.image("output/Rdata/comparison_data.Rdata")
