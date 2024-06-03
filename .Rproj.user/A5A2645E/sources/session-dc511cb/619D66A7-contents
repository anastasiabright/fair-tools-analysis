library(dplyr)

source("R/01_rdm_ids_2021.R") # 474

load("./output/Rdata/fuji/aggregated_fuji_results_all_2021_2024_05_11.Rdata") # 474
load("./output/Rdata/fair enough/fair_enough_df_combined_2024_05_11.Rdata") # 474
load("./output/Rdata/fair checker/fair_checker_df_2024_05_11.Rdata") # 474

#load("./output/Rdata/join_all_4.Rdata")

joined <- bind_cols(fuji_results_all, fair_enough_df_full, fair_checker_summary_wide_full)
joined <- joined |>
  select(-c(ID, target_uri)) |>
  rename(id = best_identifier)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

results_long <-
  joined |> pivot_longer(cols = c("fuji_percent", "enough_score_metrics_percent", "fair_checker_score"),
                                        names_to = "assessment_tool")

save(results_long, file="./output/Rdata/joined_results_long.Rdata")

save.image("./output/Rdata/joined_all.Rdata")
