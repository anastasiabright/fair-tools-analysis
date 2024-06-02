library(dplyr)
library(plotly)

load("./output/Rdata/eva data/eva_df_zenodo.Rdata")
load("./output/Rdata/fuji data/aggregated_fuji_results_all_2021_2024_05_11.Rdata") # 474
load("./output/Rdata/fair-enough data/fair_enough_df_combined_2024_05_11.Rdata") # 474
load("./output/Rdata/fair checker data/fair_checker_df_full_2024_05_11.Rdata")

load("zenodo_analysis.Rdata")

# 26 instead of 39
enough_zenodo <- fair_enough_df_full[fair_enough_df_full$guid %in% eva_zenodo$guid & 
                                       !is.na(fair_enough_df_full$enough_score_metrics_count),]
fuji_zenodo <- fuji_results_all[fuji_results_all$best_identifier %in% enough_zenodo$guid,]
checker_zenodo <- fair_checker_summary_wide_full[fair_checker_summary_wide_full$target_uri %in%
                                                   enough_zenodo$guid,]
eva_zenodo <- eva_zenodo[eva_zenodo$guid %in% enough_zenodo$guid,]


all <- bind_cols(fuji_zenodo, enough_zenodo, checker_zenodo, eva_zenodo)

all <- all |>
  select(-c(`article...108`)) |>
  rename(article = `article...1`)

all_overlapping <- all |>
  select(c(article, best_identifier, repository, repository_type, fuji_percent, 
           fuji_percent_f,fuji_percent_f1, fuji_percent_f2, 
           fuji_percent_i1, fuji_percent_i2, fuji_percent_i3, 
           fuji_percent_r1_1, enough_score_metrics_percent, enough_Gen2_FM_F1A, 
           fair_enough_f1, fair_enough_f2, fair_enough_i1, fair_enough_i2, 
           fair_enough_i3, fair_enough_r1.1,
           fair_checker_f1, fair_checker_f2, fair_checker_i1, fair_checker_i2, 
           fair_checker_i3, fair_checker_r1.1, fair_checker_score,
           eva_f1, eva_f2, eva_i1, eva_i2, eva_i3, eva_r1.1, total_score
            ))

all_long <- all_overlapping |>
  pivot_longer(cols = c("fuji_percent", "enough_score_metrics_percent", "fair_checker_score", "total_score"),
               names_to = "assessment_tool",
               values_to = "score")



summary_all <- all_long |>
  drop_na(score) |>
  group_by(assessment_tool) |>
  summarize(mean = round(mean(score), 2),
            median = median(score),
            min = min(score),
            max = max(score),
            sd = round(sd(score), digits = 2),
            count = n())


plot_overlap_fuji <- plot_ly(all_overlapping, y = ~fuji_percent, type = "violin", name = "FUJI") |>
  layout(#title = "Zenodo repository results on overlapping indicators", 
         yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_enough <- plot_ly(all_overlapping, y = ~enough_score_metrics_percent, type = "violin", name = "FAIR Enough") |>
  layout(#title = "Zenodo repository results on overlapping indicators", 
         yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_checker <- plot_ly(all_overlapping, y = ~fair_checker_score, type = "violin", name = "FAIR-Checker") |>
  layout(#title = "Zenodo repository results on overlapping indicators", 
         yaxis = list(title = "Score in %"),showlegend = FALSE)

plot_overlap_eva <- plot_ly(all_overlapping, y = ~total_score, type = "violin", name = "FAIR EVA") |>
  layout(#title = "Zenodo repository results on overlapping indicators", 
         yaxis = list(title = "Score in %"), showlegend = FALSE)

subplot(plot_overlap_fuji, plot_overlap_enough, plot_overlap_checker, plot_overlap_eva, shareY = T) 

# paired_palette = brewer.pal(n = 8, name = "Paired")
# #"#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FB9A99" "#E31A1C" "#FDBF6F" "#FF7F00"
# 
# dark_shades <- paired_palette[c(2, 6)] 
# #"#1F78B4" "#E31A1C"
# 
# ggplot(all_long, aes(x = assessment_tool, 
#                      y = score,
#                      fill = assessment_tool)) +
#   geom_violin(trim = FALSE, 
#               colour = "#555555",
#               draw_quantiles = c(0.25, 0.75)) +
#   #geom_jitter(height = 0, width = 0.1, ) +
#   stat_summary(fun = "mean",
#                geom = "point", size = 3,
#                aes(color = "Mean")) +
#   stat_summary(fun = "median",
#                geom = "point", size = 3,
#                aes(color = "Median")) +
#   scale_colour_manual(values = c(Mean = "#1F78B4", Median = "#E31A1C"), 
#                       name = "") +
#   #geom_point(data = summary_cleaned, aes(x = assessment_tool, y = mean), color = dark_shades[2], size = 3) +
#   #geom_point(data = summary_cleaned, aes(x = assessment_tool, y = median), color = dark_shades[1], size = 3) +
#   labs(y = "Score in %", x = "") +
#   #scale_fill_brewer(palette = "Accent"), guide = "none") +
#   scale_fill_manual(values = c(paired_palette[1], paired_palette[3], paired_palette[5], paired_palette[7]), 
#                     guide = "none") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(hjust = 1),
#         legend.position = "bottom") +
#   scale_x_discrete(labels = c("fuji_percent" = "FUJI", 
#                               "enough_score_metrics_percent" = "FAIR Enough", 
#                               "fair_checker_score" = "FAIR Checker",
#                               "total_score" = "FAIR EVA"))