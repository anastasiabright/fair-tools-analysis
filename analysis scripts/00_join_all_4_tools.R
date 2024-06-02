library(dplyr)

source("R/01_rdm_ids_2021.R") # 474

load("./output/Rdata/fuji data/aggregated_fuji_results_all_2021_2024_05_11.Rdata") # 474
load("./output/Rdata/fair-enough data/fair_enough_df_combined_2024_05_11.Rdata") # 474
load("./output/Rdata/fair checker data/fair_checker_df_full_2024_05_11.Rdata") # 474
#load("./output/Rdata/eva data/eva_df_2021_2024_05_10.Rdata") # 474

load("./output/Rdata/join_all_4.Rdata")

#fuji_to_add <- fuji_summary_results_all[, c("article","best_identifier", "repository", "repository_type", "fuji_percent")]
# enough_to_add <- fair_enough_df_full[, c("score_percent", "score_metrics_percent")]
# eva_to_add <- as.data.frame(result_eva[, ("total_score")])
# checker_to_add <- as.data.frame(fair_checker_summary_wide[, "fair_checker_score"])
# 
# joined_df <- bind_cols(fuji_to_add, enough_to_add, eva_to_add, checker_to_add)

joined <- bind_cols(fuji_results_all, fair_enough_df_full, fair_checker_summary_wide_full)
joined <- joined |>
  select(-c(ID, target_uri)) |>
  rename(id = best_identifier)

#overlap <- joined$best_identifier == joined$target_uri

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

results_long <-
  joined |> pivot_longer(cols = c("fuji_percent", "enough_score_metrics_percent", "fair_checker_score"),
                                        names_to = "assessment_tool")

#save_data_xlsx(list(summary, summary_grouped_repository), name= c("summary", "summary_repository"))

save(results_long, file="./output/Rdata/joined_results_long.Rdata")

#############################################################################

#diff_rows <- abs(nrow(fair_enough_df_full) - nrow(fuji_results_all))

#468
#df_fuji <- fuji_results_all[fuji_results_all$best_identifier %in% fair_enough_df_full$ID,]

save.image("./output/Rdata/join_all_4.Rdata")
