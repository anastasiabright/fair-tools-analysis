#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from Fair Checker Assessment ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(purrr)
library(dplyr)
library(stringr)
library(tidyr)

# Load data
#source("R/01_rdm_ids_2021.R")

#load("./output/Rdata/fair checker data/fair_checker_api_list.Rdata")
load("./output/Rdata/fair checker data/aggregated_fair_checker_api_list.Rdata")


# Check depth of nested fuji_local_list
pluck_depth(fair_checker_list) # 4


fair_checker_summary <- map_dfr(
  seq_along(fair_checker_list),
  ~ bind_rows(fair_checker_list[[.x]]))


fair_checker_summary_wide <- fair_checker_summary |>
  select(target_uri, metric, score) |> #, recommendation, comment
  pivot_wider(names_from = metric, values_from = score, values_fn = list) |>
  unnest_longer(c('F1A':'R1.3')) |>
  mutate(across(F1A:R1.3, as.numeric))

# fair_checker_summary_wide <- fair_checker_summary %>%
#   select(target_uri, metric, score, recommendation, comment) %>%
#   pivot_wider(names_from = metric, values_from = score, values_fn = list)
# 
# fair_checker_summary_wide <- fair_checker_summary_wide %>%
#   unnest(cols = c(F1A, F1B, F2A, F2B, A1.1, A1.2, I1, I2, I3, R1.1, R1.2, R1.3)) %>%
#   mutate(across(c(F1A, F1B, F2A, F2B, A1.1, A1.2,I1, I2, I3, R1.1, R1.2, R1.3), as.numeric))

  
not_accessible <- charite_rd_2021[!charite_rd_2021$best_identifier %in% fair_checker_summary_wide$target_uri,]
not_accessible <- not_accessible$best_identifier
df <- data.frame(
  target_uri = rep(NA, length(not_accessible)),
  F1A = rep(NA_real_, length(not_accessible)),
  F1B = rep(NA_real_, length(not_accessible)),
  F2A = rep(NA_real_, length(not_accessible)),
  F2B = rep(NA_real_, length(not_accessible)),
  A1.1 = rep(NA_real_, length(not_accessible)),
  A1.2 = rep(NA_real_, length(not_accessible)),
  I1 = rep(NA_real_, length(not_accessible)),
  I2 = rep(NA_real_, length(not_accessible)),
  I3 = rep(NA_real_, length(not_accessible)),
  R1.1 = rep(NA_real_, length(not_accessible)),
  R1.2 = rep(NA_real_, length(not_accessible)),
  R1.3 = rep(NA_real_, length(not_accessible))
)

df$target_uri <- not_accessible

# Concatenate the original fair_summary_wide with the new data frame
#fair_checker_summary_wide <- bind_rows(fair_checker_summary_wide, df)

# 222, 304, 329, 332, 342, 345, 448
part1 <- slice(fair_checker_summary_wide, 1:221)
part2 <- slice(fair_checker_summary_wide, 222:nrow(fair_checker_summary_wide))
combined_df1 <- bind_rows(part1, df[1,], part2)

part3 <- slice(combined_df1, 1:303)
part4 <- slice(combined_df1, 304:nrow(combined_df1))
combined_df2 <- bind_rows(part3, df[2,], part4)

part5 <- slice(combined_df2, 1:328)
part6 <- slice(combined_df2, 329:nrow(combined_df2))
combined_df3 <- bind_rows(part5, df[3,], part6)

part7 <- slice(combined_df3, 1:331)
part8 <- slice(combined_df3, 332:nrow(combined_df3))
combined_df4 <- bind_rows(part7, df[4,], part8)

part9 <- slice(combined_df4, 1:341)
part10 <- slice(combined_df4, 342:nrow(combined_df4))
combined_df5 <- bind_rows(part9, df[5,], part10)

part11 <- slice(combined_df5, 1:344)
part12 <- slice(combined_df5, 345:nrow(combined_df5))
combined_df6 <- bind_rows(part11, df[6,], part12)

part13 <- slice(combined_df6, 1:447)
part14 <- slice(combined_df6, 448:nrow(combined_df6))
fair_checker_summary_wide_full <- bind_rows(part13, df[7,], part14)

        
fair_checker_summary_wide_full <- fair_checker_summary_wide_full |>  
  mutate_at(vars(2:13), as.numeric) |>
  select(-'NA') |>
  rowwise() |> 
  mutate(fair_checker_score = round(sum(c_across(2:13)/24*100), digits=2), .after = target_uri) |>
  mutate(fair_checker_f1 = round((F1A+F1B)/(2*2)*100, 2),
         fair_checker_f2 = round((F2A+F2B)/(2*2)*100, 2),
         #fair_checker_f3 = round(()/2*100, 2),
         #fair_checker_f4 = round(()*100, 2),
         fair_checker_a1.1 = A1.1/2*100,
         fair_checker_a1.2 = A1.2/2*100,
         #fair_checker_a2 = round(()*100, 2),
         fair_checker_i1 = I1/2*100,
         fair_checker_i2 = I2/2*100,
         fair_checker_i3 = I3/2*100,
         fair_checker_r1.1 = R1.1/2*100, 
         fair_checker_r1.2 = R1.2/2*100, 
         fair_checker_r1.3 = R1.3/2*100, .after = fair_checker_score) |>
  mutate(checker_percent_f = round((F1A+F1B+F2A+F2B)/(4*2)*100, 2), 
         checker_percent_a = round((A1.1+A1.2)/(2*2)*100, 2), 
         checker_percent_i = round((I1+I2+I3)/(3*2)*100, 2), 
         checker_percent_r = round((R1.1+R1.2+R1.3)/(3*2)*100, 2), .after = fair_checker_score)

# No overall score
# 0, 1, 2
# 12 metrics

save(fair_checker_summary_wide_full, file= "output/Rdata/fair checker data/fair_checker_df_full_2024_05_11.Rdata")
save.image("./output/Rdata/fair checker data/aggregated_fair_checker_api_list.Rdata")
