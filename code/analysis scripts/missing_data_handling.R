library(dplyr)

load("./output/Rdata/joined_all.Rdata")

average_score_removed <- joined_cleaned %>%
  rowwise() %>%
  mutate(total_score = sum(c_across(starts_with("fuji_percent")), 
                           c_across(starts_with("enough_score_metrics_percent")),
                           c_across(starts_with("fair_checker_score")),
                           c_across(starts_with("eva_total_score")))) %>%
  summarise(average_total_score = mean(total_score))

# average_score_imputed <- joined_imputed %>%
#   rowwise() %>%
#   mutate(total_score = sum(c_across(starts_with("fuji_percent")),
#                            c_across(starts_with("enough_score_metrics_percent")),
#                            c_across(starts_with("fair_checker_score")),
#                            c_across(starts_with("eva_total_score")))) %>%
#   summarise(average_total_score = mean(total_score))
# 
# average_score_weighted <- data_weighted %>%
#   summarise(average_total_weighted_score = mean(total_weighted_score))
# 
# comparison <- data.frame(
#   Scenario = c("Removed", "Imputed", "Weighted"),
#   Average_Score = c(average_score_removed$average_total_score,
#                     average_score_imputed$average_total_score,
#                     average_score_weighted$average_total_weighted_score)
# )
# summary_comparison <- comparison %>%
#   group_by(Scenario) %>%
#   summarise(
#     Mean_Average_Score = mean(Average_Score, na.rm = TRUE),
#     Median_Average_Score = median(Average_Score, na.rm = TRUE),
#     Std_Dev_Average_Score = sd(Average_Score, na.rm = TRUE)
#   )
