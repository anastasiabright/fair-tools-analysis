#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from EOSC FAIR EVA Assessment ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(purrr)
library(dplyr)
library(jsonlite)
library(tidyr)

# Load data
source("R/01_rdm_ids_2021.R")
load("output/Rdata/fair eva/fair_eva_local_list_2021_zenodo.Rdata")

# Select data sets from Zenodo repository
charite_zenodo <- charite_rd_2021[charite_rd_2021$best_identifier %in% zenodo_ds$best_identifier,]
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FAIR EVA data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pluck_depth(zenodo_results) # 7

extract_values_f <- function(entry) {
  if ("findable" %in% names(entry)) {
    rda_keys <- names(entry[["findable"]])[grepl("^rda_f\\d+_\\d+", names(entry[["findable"]]))]
    
    scores_list <- vector("list", length(rda_keys))
    
    for (i in seq_along(rda_keys)) {
      if ("score" %in% names(entry[["findable"]][[rda_keys[i]]])) {
        scores_list[[i]] <- entry[["findable"]][[rda_keys[i]]][["score"]][["earned"]]
      } else {
        scores_list[[i]] <- NA
      }
    }
    
    result_df <- as.data.frame(scores_list, stringsAsFactors = FALSE)
    names(result_df) <- paste("eva_score_", rda_keys, sep = "")
    
    return(result_df)
  } else {
    return(tibble(score_earned = NA))
  }
}
flattened_zenodo_results <- do.call(c, zenodo_results)

result_df_f <- map_dfr(flattened_zenodo_results, extract_values_f)
result_df_f$article <- charite_zenodo$article
result_df_f$guid <- charite_zenodo$best_identifier
result_df_f <- result_df_f |> select(article, guid, everything())

extract_values_a <- function(entry) {
  if ("accessible" %in% names(entry)) {
    rda_keys <- names(entry[["accessible"]])[grepl("^rda_a\\d+_\\d+", names(entry[["accessible"]]))]
    
    scores_list <- vector("list", length(rda_keys))
    
    for (i in seq_along(rda_keys)) {
      if ("score" %in% names(entry[["accessible"]][[rda_keys[i]]])) {
        scores_list[[i]] <- entry[["accessible"]][[rda_keys[i]]][["score"]][["earned"]]
      } else {
        scores_list[[i]] <- NA
      }
    }
    
    result_df <- as.data.frame(scores_list)
    names(result_df) <- paste("eva_score_", rda_keys, sep = "")
    
    
    return(result_df)
  } else {
    return(tibble(score_earned = NA))
  }
}


result_df_a <- map_dfr(flattened_zenodo_results, extract_values_a) # 13 variables


extract_values_i <- function(entry) {
  if ("interoperable" %in% names(entry)) {
    rda_keys <- names(entry[["interoperable"]])[grepl("^rda_i\\d+_\\d+", names(entry[["interoperable"]]))]
    
    scores_list <- vector("list", length(rda_keys))
    
    for (i in seq_along(rda_keys)) {
      if ("score" %in% names(entry[["interoperable"]][[rda_keys[i]]])) {
        scores_list[[i]] <- entry[["interoperable"]][[rda_keys[i]]][["score"]][["earned"]]
      } else {
        scores_list[[i]] <- NA
      }
    }
    
    result_df <- as.data.frame(scores_list)
    names(result_df) <- paste("eva_score_", rda_keys, sep = "")
    
    return(result_df)
  } else {
    return(tibble(score_earned = NA))
  }
}

result_df_i <- map_dfr(flattened_zenodo_results, extract_values_i) # 13 variables


extract_values_r <- function(entry) {
  if ("reusable" %in% names(entry)) {
    rda_keys <- names(entry[["reusable"]])[grepl("^rda_r\\d+_\\d+", names(entry[["reusable"]]))]
    
    scores_list <- vector("list", length(rda_keys))
    
    for (i in seq_along(rda_keys)) {
      if ("score" %in% names(entry[["reusable"]][[rda_keys[i]]])) {
        scores_list[[i]] <- entry[["reusable"]][[rda_keys[i]]][["score"]][["earned"]]
      } else {
        scores_list[[i]] <- NA
      }
    }
    
    result_df <- as.data.frame(scores_list)
    names(result_df) <- paste("eva_score_", rda_keys, sep = "")
    
    return(result_df)
  } else {
    return(tibble(score_earned = NA))
  }
}

result_df_r <- map_dfr(flattened_zenodo_results, extract_values_r) # 11 variables


eva_zenodo <- bind_cols(result_df_f, result_df_a, result_df_i, result_df_r)

eva_zenodo <- eva_zenodo |> 
  mutate(eva_f1 = round((eva_score_rda_f1_01d+eva_score_rda_f1_01m+
                      eva_score_rda_f1_02d+eva_score_rda_f1_02m)/4, digits = 2),
         eva_f2 = eva_score_rda_f2_01m,
         eva_f3 = eva_score_rda_f3_01m,
         eva_f4 = eva_score_rda_f4_01m,
         eva_a1 = round((eva_score_rda_a1_01m+eva_score_rda_a1_02d+eva_score_rda_a1_02m+
           eva_score_rda_a1_03d+eva_score_rda_a1_03m+eva_score_rda_a1_04d+
           eva_score_rda_a1_04m+eva_score_rda_a1_05d)/8, digits = 2),
         eva_a1.1 = round((eva_score_rda_a1_1_01d+eva_score_rda_a1_1_01m)/2, digits = 2),
         eva_a1.2 = eva_score_rda_a1_2_01d,
         eva_a2 = eva_score_rda_a2_01m,
         eva_i1 = round((eva_score_rda_i1_01d+eva_score_rda_i1_01m+eva_score_rda_i1_02d+
                        eva_score_rda_i1_02m)/4, digits = 2),
         eva_i2 = round((eva_score_rda_i2_01d+eva_score_rda_i2_01m)/2, digits = 2),
         eva_i3 = round((eva_score_rda_i3_01d+eva_score_rda_i3_01m+eva_score_rda_i3_02d+
                  eva_score_rda_i3_02m+eva_score_rda_i3_03m+eva_score_rda_i3_04m)/6, digits = 2),
         eva_r1 = eva_score_rda_r1_01m,
         eva_r1.1 = round((eva_score_rda_r1_1_01m+eva_score_rda_r1_1_02m+
                             eva_score_rda_r1_1_03m)/3, digits = 2),
         eva_r1.2 = eva_score_rda_r1_2_02m,
         eva_r1.3 = round((eva_score_rda_r1_3_01d+eva_score_rda_r1_3_01m+eva_score_rda_r1_3_02d+
           eva_score_rda_r1_3_02m)/4, digits = 2)
  )

eva_zenodo <- eva_zenodo |>
  rowwise() |> 
  mutate(total_score = round(sum(c_across(3:43))/41, digits = 2))

save(eva_zenodo, file = "./output/Rdata/fair eva/fair_eva_df_zenodo.Rdata")
write.csv2(eva_zenodo, 
           file = "output/csv/fair_eva_df_zenodo.csv", row.names = F)

save.image("output/Rdata/fair eva/aggregated_fair_eva_scores_zenodo.Rdata")

#write.csv(eva_df, "fair_eva_2021_2024_05_10.csv", quote = T)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

