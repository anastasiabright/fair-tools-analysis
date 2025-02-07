#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from FUJI Assessment ----
# Current code owner Anastasiia Iarkaeva (jaatnew@gmail.com)
# In acknowledgement to the original code owner Jan Taubitz
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(purrr)
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)

# Load data
source("code/01_rdm_ids_2021.R")
load("./output/Rdata/fuji/fuji_local_list_2021_2024_02_18.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FUJI data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check depth of nested fuji_local_list_2021
pluck_depth(fuji_local_list_2021) # 10

# Extract long data from API results
fuji_summary <-
  map_dfr(
    seq_along(fuji_local_list_2021), 
    ~ bind_rows(fuji_local_list_2021[[.x]][["summary"]], .id = "id") |>
      mutate(rd_id = fuji_local_list_2021[[.x]][["request"]][["object_identifier"]], 
             .before = id))

# Extract only score_percent and FAIR
fuji_summary_percent <- fuji_summary |>
  filter(id == "score_percent") |>
  select(1, 10) |>
  rename(fuji_percent = `FAIR`)

# Extract score_percent for each metric
fuji_summary_results_all <- fuji_summary |>
  filter(id == "score_percent") |>
  select(rd_id, id, 
         fuji_percent = FAIR, 
         fuji_percent_f = `F`, 
         fuji_percent_a = `A`, 
         fuji_percent_i = `I`, 
         fuji_percent_r = `R`,
         fuji_percent_f1 = `F1`,
         fuji_percent_f2 = `F2`,
         fuji_percent_f3 = `F3`,
         fuji_percent_f4 = `F4`,
         fuji_percent_a1 = `A1`,
         fuji_percent_i1 = `I1`,
         fuji_percent_i2 = `I2`,
         fuji_percent_i3 = `I3`,
         fuji_percent_r1 = `R1`,
         fuji_percent_r1_1 = `R1.1`,
         fuji_percent_r1_2 = `R1.2`,
         fuji_percent_r1_3 = `R1.3`)


fuji_mean_median_max <- fuji_summary_results_all |>
  #  group_by(id) |>
  summarise_if(is.numeric, list(mean, median, max), na.rm = TRUE) |>
  mutate_if(is.numeric, round, digits = 2) |>
  mutate(assessment_tool = "FUJI", .before = 1) |>
  rename_all(~ (str_replace_all(., c(fn1 = "mean", fn2 = "median", fn3 = "max"))))


# Get all FsF metrics scores
fuji_metrics_list <- list()

for (i in seq_along(fuji_local_list_2021)) {
  
  fuji_metrics_df <-
    map_dfr(
      seq_along(1:16), 
      ~ bind_cols(metric_identifier = fuji_local_list_2021[[i]][["results"]][[.x]][["metric_identifier"]]) |>
        mutate(metric_name = fuji_local_list_2021[[i]][["results"]][[.x]][["metric_name"]]) |>
        mutate(score_total = fuji_local_list_2021[[i]][["results"]][[.x]][["score"]][["total"]]) |>
        mutate(score_earned = fuji_local_list_2021[[i]][["results"]][[.x]][["score"]][["earned"]]) |>
        mutate(guid = fuji_local_list_2021[[i]][["request"]][["object_identifier"]], .before = metric_identifier))
  
  fuji_metrics_list[[i]] <- fuji_metrics_df
  
}

fuji_metrics_tests <- bind_rows(fuji_metrics_list) 

save(fuji_metrics_tests, file = "./output/Rdata/fuji/fuji_metrics_tests_2021_2024_05_11.Rdata")

# Make FsF metrics scores wider for join with F-UJI summary df
fuji_metrics_tests_long <- fuji_metrics_tests |>
  mutate(fuji_score_earned = score_earned / score_total * 100) |>
  mutate(fuji_metric_identifier = factor(metric_identifier, levels = c(
    "FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
    "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D", 
    "FsF-I1-01M", "FsF-I2-01M", "FsF-I3-01M", # "FsF-I1-02M",
    "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"
  ))) |>
  arrange(fuji_metric_identifier) |>
  select(guid, fuji_metric_identifier, score_earned) |>
  pivot_wider(names_from = fuji_metric_identifier, values_from = score_earned) |>
  rename(fuji_FsF_F1_01D = "FsF-F1-01D", 
         fuji_FsF_F1_02D = "FsF-F1-02D", 
         fuji_FsF_F2_01M = "FsF-F2-01M", 
         fuji_FsF_F3_01M = "FsF-F3-01M", 
         fuji_FsF_F4_01M = "FsF-F4-01M",
         fuji_FsF_A1_01M = "FsF-A1-01M", 
         fuji_FsF_A1_02M = "FsF-A1-02M", 
         fuji_FsF_A1_03D = "FsF-A1-03D", 
         fuji_FsF_I1_01M = "FsF-I1-01M", 
         # fuji_FsF_I1_02M = "FsF-I1-02M", 
         fuji_FsF_I2_01M = "FsF-I2-01M", 
         fuji_FsF_I3_01M = "FsF-I3-01M",
         fuji_FsF_R1_01MD = "FsF-R1-01MD", 
         fuji_FsF_R1.1_01M = "FsF-R1.1-01M", 
         fuji_FsF_R1.2_01M = "FsF-R1.2-01M", 
         fuji_FsF_R1.3_01M = "FsF-R1.3-01M", 
         fuji_FsF_R1.3_02D = "FsF-R1.3-02D")


fuji_summary_results_all <- fuji_summary_results_all |> 
  left_join(fuji_metrics_tests_long, by = c("rd_id" = "guid"))


# Summary over all metrics and datasets
df_sum <- fuji_metrics_tests |>
  group_by(metric_identifier, metric_name, score_total) |>
  summarise(mean_score_earned = mean(score_earned)) |>
  mutate(mean_score_earned_perc = mean_score_earned / score_total * 100) |>
  mutate(indicator = str_extract(metric_identifier, "(?<=-)(.*)(?=-)")) |>
  # rename(name = metric_identifier,
  #        value = mean_score_earned_perc,
  #        parent = parent,
  #        score = score_total,
  #        principle = metric_name) |>
  # select(-mean_score_earned) |>
  mutate(metric_identifier = factor(metric_identifier, levels = 
                         c("FsF-F1-01D", "FsF-F1-02D", "FsF-F2-01M", "FsF-F3-01M", "FsF-F4-01M",
                           "FsF-A1-01M", "FsF-A1-02M", "FsF-A1-03D", "FsF-I1-01M", "FsF-I1-02M", "FsF-I2-01M", "FsF-I3-01M",
                           "FsF-R1-01MD", "FsF-R1.1-01M", "FsF-R1.2-01M", "FsF-R1.3-01M", "FsF-R1.3-02D"))) |>
  arrange(metric_identifier)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Extract guid_scheme ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# fuji_guid_scheme <-
#   map_dfr(
#     seq_along(fuji_local_list_2021),
#     ~ bind_rows(fuji_local_list_2021[[.x]][["request"]]) |>
#       mutate(guid_scheme = fuji_local_list_2021[[.x]][["results"]][[1]][["output"]][["guid_scheme"]])) |>
#   select(rd_id = object_identifier, guid_scheme) |>
#   mutate_all(str_trim)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fuji_results_all <- charite_rd_2021 |>
  left_join(fuji_summary_results_all,
            by = c("best_identifier" = "rd_id")) |>
#  left_join(fuji_guid_scheme, by = c("best_identifier" = "rd_id")) |>
  select(-id)
   
save(fuji_results_all, file = "./output/Rdata/fuji/aggregated_fuji_results_all_2021_2024_11_02.Rdata")
write.csv2(fuji_results_all, 
           file = "./output/csv/fuji_df_2021_2024_11_02.csv", row.names = F
           )

save.image(file = "./output/Rdata/fuji/fuji_all_aggregations_2021_2024_11_02.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

