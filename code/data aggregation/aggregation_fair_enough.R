#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate results from Fair Enough Assessment ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(purrr)
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(hexView)

# Load data
# source("R/01_rdm_ids_2021.R")
# load("./output/Rdata/fair enough/fair_enough_list_overall_scores_2021_2024_02_25.Rdata")
load("./output/Rdata/fair enough/aggregation_fair_enough_2024_11_05.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate Fair Enough overall scores ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Check depth of nested fair_enough_list_2021
pluck_depth(fair_enough_list_2021) # 16


fair_enough_summary <- map_dfr(
  seq_along(fair_enough_list_2021),
  ~ {
 if ("score" %in% names(fair_enough_list_2021[[.x]])) {
      scores <- list(
        fair_enough_score = fair_enough_list_2021[[.x]][["score"]],
        fair_enough_score_total = fair_enough_list_2021[[.x]][["score_max"]],
        fair_enough_score_percent = fair_enough_list_2021[[.x]][["score_percent"]]
      )
    } else {
      scores <- list(
        fair_enough_xml_document = inherits(fair_enough_list_2021[[.x]][["doc"]], "externalptr")
        #xml_node = fair_enough_list_2021[[.x]][["node"]]
      )
    }
    
    # Filter out NULL or list elements
    fair_enough_scores <- scores[!sapply(scores, is.null) & 
                                               !sapply(scores, is.list)]
    
    if (length(scores) > 0) {
      fair_enough_scores_df <- data.frame(fair_enough_scores)
      fair_enough_scores_df <- fair_enough_scores_df %>% mutate(guid = fair_enough_list_2021[[.x]][["subject"]])
    } else {
      fair_enough_scores_df <- NULL
    }
    
    return(fair_enough_scores_df)
  }
)


save(fair_enough_summary, file = "output/Rdata/fair enough/fair_enough_df_overall_scores_2024_11_02.Rdata")
write.csv2(fair_enough_summary, "./output/csv/fair_enough_df_overall_scores_2021_2024_11_02.csv", row.names = F)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Aggregate and manipulate FAIR Enough individual metrics scores ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

load("./output/Rdata/fair enough/fair_enough_api_each_metric_list.Rdata")

extract_values <- function(entry) {
  if (
    !is.null(entry[["http://semanticscience.org/resource/SIO_000332"]]) &&
    length(entry[["http://semanticscience.org/resource/SIO_000332"]]) > 0 &&
    !is.null(entry[["http://semanticscience.org/resource/SIO_000332"]][[1]][["@id"]]) &&
    !is.null(entry[["http://semanticscience.org/resource/SIO_000300"]]) &&
    length(entry[["http://semanticscience.org/resource/SIO_000300"]]) > 0 &&
    !is.null(entry[["http://semanticscience.org/resource/SIO_000300"]][[1]][["@value"]]) &&
    !is.null(entry[["http://schema.org/comment"]]) &&
    length(entry[["http://schema.org/comment"]]) > 0 &&
    !is.null(entry[["http://schema.org/comment"]][[1]][["@value"]])
  ) {
    
    id_value <- entry[["http://semanticscience.org/resource/SIO_000332"]][[1]][["@id"]]
    value_value <- entry[["http://semanticscience.org/resource/SIO_000300"]][[1]][["@value"]]
    comment_value <- entry[["http://schema.org/comment"]][[1]][["@value"]]
    df <- data.frame(
      ID = id_value,
      Value = value_value,
      Comment = comment_value
    )
    
    return(df)
  } else {
    
    return(data.frame(ID = NA, Value = NA, Comment = NA))
  }
}

decode_json <- function(hex_data) {
  data_char <- rawToChar(as.raw(hex_data))
  tryCatch(
    {
      json_data <- jsonlite::fromJSON(data_char)

      return(json_data)
    },
    error = function(e) {
      cat("Error decoding JSON data:", e$message, "\n")
       return(NULL)
    }
  )
}


test1 <- decode_json(fair_enough_scores[[1]])
test2 <- decode_json(fair_enough_scores[[7]]) # server error evaluating this dataset

full_scores_list <- lapply(fair_enough_scores, decode_json)
full_scores_df <- map_dfr(
  seq_along(full_scores_list),
  ~ {
    if ("score" %in% names(full_scores_list[[.x]])) {
      scores <- list(
        enough_score = full_scores_list[[.x]][["score"]],
        enough_score_total = full_scores_list[[.x]][["score_max"]],
        enough_score_percent = full_scores_list[[.x]][["score_percent"]]
      )
    } else {
      scores <- list(
        enough_xml_document = inherits(full_scores_list[[.x]][["doc"]], "externalptr")
        #xml_node = fair_enough_list_2021[[.x]][["node"]]
      )
    }
    scores <- scores[!sapply(scores, is.null) & !sapply(scores, is.list)]
    
    if (length(scores) > 0) {
      scores_df <- data.frame(scores)
      scores_df <- scores_df %>% mutate(guid = full_scores_list[[.x]][["subject"]])
    } else {
      scores_df <- NULL
    }
    
    return(scores_df)
  }
)

process_metric_metadata <- function(data, list_name, new_column_name, new_column_name2) {
  decoded_json <- lapply(data, decode_json)
  extracted_values <- lapply(decoded_json, extract_values)
  combined_df <- do.call(rbind, extracted_values)
  renamed_df <- combined_df |> 
  #  rename(Gen2_FM_F1B_metadata = Value)
    rename({{ new_column_name }} := Value,
           {{ new_column_name2 }} := Comment)
  
  selected_df <- renamed_df |> 
  #  select(ID, Gen2_FM_F1B_metadata)
    select(ID, {{new_column_name}}, {{ new_column_name2 }})
  
  return(selected_df)
}

F1_metadata_identifier_persistence_df <- process_metric_metadata(F1_metadata_identifier_persistence, 
                                                                 "F1_metadata_identifier_persistence", 
                                                                 "fair_enough_Gen2_FM_F1B_metadata",
                                                                 "fair_enough_Gen2_FM_F1B_metadata_Comment")

F1_data_identifier_persistence_df <- process_metric_metadata(F1_data_identifier_persistence,
                                                             "F1_data_identifier_persistence",
                                                             "fair_enough_Gen2_FM_F1B_data",
                                                             "fair_enough_Gen2_FM_F1B_data_Comment"
                                                             )
F1_unique_identifier_df <- process_metric_metadata(F1_unique_identifier,
                                                  "F1_unique_identifier",
                                                  "fair_enough_Gen2_FM_F1A",
                                                  "fair_enough_Gen2_FM_F1A_Comment")

F2_structured_metadata_df <- process_metric_metadata(F2_structured_metadata,
                                                     "F2_structured_metadata",
                                                     "fair_enough_Gen2_FM_F2A",
                                                     "fair_enough_Gen2_FM_F2A_Comment")

F2_grounded_metadata_df <- process_metric_metadata(F2_grounded_metadata,
                                                   "F2_grounded_metadata",
                                                   "fair_enough_Gen2_FM_F2B",
                                                   "fair_enough_Gen2_FM_F2B_Comment")

F3_metadata_identifier_in_metadata_df <- process_metric_metadata(F3_metadata_identifier_in_metadata,
                                                                 "F3_metadata_identifier_in_metadata",
                                                                 "fair_enough_Gen2_FM_F3_metadata",
                                                                 "fair_enough_Gen2_FM_F3_metadata_Comment")

F3_data_identifier_in_metadata_df <- process_metric_metadata(F3_data_identifier_in_metadata,
                                                             "F3_data_identifier_in_metadata",
                                                             "fair_enough_Gen2_FM_F3_data",
                                                             "fair_enough_Gen2_FM_F3_data_Comment")
F4_searchable_df <- process_metric_metadata(F4_searchable,
                                            "F4_searchable",
                                            "fair_enough_Gen2_FM_F4",
                                            "fair_enough_Gen2_FM_F4_Comment")

A1.1_metadata_protocol_df <- process_metric_metadata(A1.1_metadata_protocol,
                                                     "A1.1_metadata_protocol",
                                                     "fair_enough_Gen2_FM_A1.1_metadata",
                                                     "fair_enough_Gen2_FM_A1.1_metadata_Comment")

A1.1_data_protocol_df <- process_metric_metadata(A1.1_data_protocol,
                                                 "A1.1_data_protocol",
                                                 "fair_enough_Gen2_FM_A1.1_data",
                                                 "fair_enough_Gen2_FM_A1.1_data_Comment")

A1.2_metadata_authorization_df <- process_metric_metadata(A1.2_metadata_authorization,
                                                          "A1.2_metadata_authorization",
                                                          "fair_enough_Gen2_FM_A1.2_metadata",
                                                          "fair_enough_Gen2_FM_A1.2_metadata_Comment")

A1.2_data_authorization_df <- process_metric_metadata(A1.2_data_authorization,
                                                      "A1.2_data_authorization",
                                                      "fair_enough_Gen2_FM_A1.2_data",
                                                      "fair_enough_Gen2_FM_A1.2_data_Comment")

A2_metadata_persistence_df <- process_metric_metadata(A2_metadata_persistence,
                                                      "A2_metadata_persistence",
                                                      "fair_enough_Gen2_FM_A2",
                                                      "fair_enough_Gen2_FM_A2_Comment")

I1_data_kr_language_strong_df <- process_metric_metadata(I1_data_kr_language_strong,
                                                         "I1_data_kr_language_strong",
                                                         "fair_enough_Gen2_FM_I1B_data",
                                                         "fair_enough_Gen2_FM_I1B_data_Comment")

I1_metadata_kr_language_strong_df <- process_metric_metadata(I1_metadata_kr_language_strong,
                                                             "I1_metadata_kr_language_strong",
                                                             "fair_enough_Gen2_FM_I1B_metadata",
                                                             "fair_enough_Gen2_FM_I1B_metadata_Comment")

I1_metadata_kr_language_weak_df <- process_metric_metadata(I1_metadata_kr_language_weak,
                                                           "I1_metadata_kr_language_weak",
                                                           "fair_enough_Gen2_FM_I1A_metadata",
                                                           "fair_enough_Gen2_FM_I1A_metadata_Comment")

I1_data_kr_language_weak_df <- process_metric_metadata(I1_data_kr_language_weak,
                                                       "I1_data_kr_language_weak",
                                                       "fair_enough_Gen2_FM_I1A_data",
                                                       "fair_enough_Gen2_FM_I1A_data_Comment")

I2_metadata_uses_fair_vocabularies_strong_df <- process_metric_metadata(I2_metadata_uses_fair_vocabularies_strong,
                                                                        "I2_metadata_uses_fair_vocabularies_strong",
                                                                        "fair_enough_Gen2_FM_I2B",
                                                                        "fair_enough_Gen2_FM_I2B_Comment")

I2_metadata_uses_fair_vocabularies_weak_df <- process_metric_metadata(I2_metadata_uses_fair_vocabularies_weak,
                                                                      "I2_metadata_uses_fair_vocabularies_weak",
                                                                      "fair_enough_Gen2_FM_I2A",
                                                                      "fair_enough_Gen2_FM_I2A_Comment")

I3_metadata_contains_outward_links_df <- process_metric_metadata(I3_metadata_contains_outward_links,
                                                                 "I3_metadata_contains_outward_links",
                                                                 "fair_enough_Gen2_FM_I3",
                                                                 "fair_enough_Gen2_FM_I3_Comment")

R1.1_metadata_includes_license_strong_df <- process_metric_metadata(R1.1_metadata_includes_license_strong,
                                                                    "R1.1_metadata_includes_license_strong",
                                                                    "fair_enough_Gen2_FM_R1.1_metadata",
                                                                    "fair_enough_Gen2_FM_R1.1_metadata_Comment")

R1.1_metadata_includes_license_weak_df <- process_metric_metadata(R1.1_metadata_includes_license_weak,
                                                                  "R1.1_metadata_includes_license_weak",
                                                                  "fair_enough_Gen2_FM_R1.1_data",
                                                                  "fair_enough_Gen2_FM_R1.1_data_Comment")

fair_enough_df <- bind_cols(#select(F1_data_identifier_persistence_df, ID, 2),
  #F1_data_identifier_persistence_df, 
                            select(F1_unique_identifier_df,2:3), 
                            select(F1_metadata_identifier_persistence_df, ID, 2:3),
                            select(F2_grounded_metadata_df,2:3),
                            select(F2_structured_metadata_df,2:3),
                            select(F3_data_identifier_in_metadata_df,2:3),
                            select(F3_metadata_identifier_in_metadata_df,2:3),
                            select(F4_searchable_df,2:3),
                            select(A1.1_data_protocol_df,2:3),
                            select(A1.1_metadata_protocol_df,2:3),
                            select(A1.2_data_authorization_df,2:3),
                            select(A1.2_metadata_authorization_df,2:3),
                            select(A2_metadata_persistence_df,2:3),
                            select(I1_data_kr_language_strong_df,2:3),
                            select(I1_data_kr_language_weak_df,2:3),
                            select(I1_metadata_kr_language_strong_df,2:3),
                            select(I1_metadata_kr_language_weak_df,2:3),
                            select(I2_metadata_uses_fair_vocabularies_strong_df,2:3),
                            select(I2_metadata_uses_fair_vocabularies_weak_df,2:3),
                            select(I3_metadata_contains_outward_links_df,2:3),
                            select(R1.1_metadata_includes_license_strong_df,2:3),
                            select(R1.1_metadata_includes_license_weak_df,2:3)
                            )

save.image("./output/Rdata/fair enough/aggregation_fair_enough_each_metric_2024_11_05.Rdata")
write.csv2(fair_enough_df, "./output/fair_enough_df_20241105.csv", row.names = F)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine full scores and individual metrics ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load("output/Rdata/fair enough/fair_enough_df_overall_scores_2024_05_10.Rdata")
fair_enough_df_full <- bind_cols(fair_enough_summary, fair_enough_df) |>
  select(-guid) |>
  relocate(ID, .before = enough_score) |>
  mutate(across(c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42, 44, 46), as.numeric)) |>
  rename(
    fair_enough_score = enough_score,
    fair_enough_score_total = enough_score_total,
    fair_enough_score_percent = enough_score_percent,
    fair_enough_xml_document = enough_xml_document
  )

fair_enough_df_full$fair_enough_score_metrics_count <- rowSums(fair_enough_df_full[, c(6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42, 44, 46)], na.rm = TRUE)
#fair_enough_df_full$fair_enough_score_metrics_count <- rowSums(fair_enough_df_full[6:26])
fair_enough_df_full <- fair_enough_df_full |>
  relocate(fair_enough_score_metrics_count, .after = fair_enough_score_percent) |>
  mutate(fair_enough_score_metrics_percent = round((fair_enough_score_metrics_count / 22)*100, digits = 2), 
         .after = fair_enough_score_metrics_count)

fair_enough_df_full <- fair_enough_df_full |>
  mutate(fair_enough_f1 = round((fair_enough_Gen2_FM_F1A+fair_enough_Gen2_FM_F1B_metadata)/2*100, 2),
         fair_enough_f2 = round((fair_enough_Gen2_FM_F2A + fair_enough_Gen2_FM_F2B)/2*100, 2),
         fair_enough_f3 = round((fair_enough_Gen2_FM_F3_data + fair_enough_Gen2_FM_F3_metadata)/2*100, 2),
         fair_enough_f4 = round((fair_enough_Gen2_FM_F4)*100, 2),
         fair_enough_f = round((fair_enough_f1 + fair_enough_f2 + 
                                  fair_enough_f3 + fair_enough_f4)/4, 2),
         fair_enough_a1.1 = round((fair_enough_Gen2_FM_A1.1_data+fair_enough_Gen2_FM_A1.1_metadata)/2*100, 2),
         fair_enough_a1.2 = round((fair_enough_Gen2_FM_A1.2_data+fair_enough_Gen2_FM_A1.2_metadata)/2*100, 2),
         fair_enough_a2 = round((fair_enough_Gen2_FM_A2)*100, 2),
         fair_enough_a = round((fair_enough_a1.1 + fair_enough_a1.2 + fair_enough_a2)/3, 2),
         fair_enough_i1 = round((fair_enough_Gen2_FM_I1A_data+fair_enough_Gen2_FM_I1B_data+
                                   fair_enough_Gen2_FM_I1A_metadata+fair_enough_Gen2_FM_I1B_metadata)/4*100, 2),
         fair_enough_i2 = round((fair_enough_Gen2_FM_I2A+fair_enough_Gen2_FM_I2B)/2*100, 2),
         fair_enough_i3 = round((fair_enough_Gen2_FM_I3)/2*100, 2),
         fair_enough_i = round((fair_enough_i1 + fair_enough_i2 + fair_enough_i3)/3, 2),
         fair_enough_r1.1 = round((fair_enough_Gen2_FM_R1.1_data+fair_enough_Gen2_FM_R1.1_metadata)/2*100, 2),
         fair_enough_r = round((fair_enough_r1.1))
  ) 

save(fair_enough_df_full, file= "output/Rdata/fair enough/fair_enough_df_combined_2025_01_29.Rdata")
write.csv2(fair_enough_df_full, 
           file = "output/Rdata/fair enough/fair_enough_df_combined_2025_01_29.csv", row.names = F)
save.image("./output/Rdata/fair enough/aggregation_fair_enough_2025_01_29.Rdata")
