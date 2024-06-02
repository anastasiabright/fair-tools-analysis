library(dplyr)
library(purrr)

load("./output/Rdata/fuji data/fuji_local_list_2021_2024_02_18.Rdata")
load("./output/Rdata/fair-enough data/fair_enough_df_combined_2024_05_11.Rdata")
load("./output/Rdata/fair checker data/fair_checker_api_list.Rdata")

fair_checker_summary <- map_dfr(
  seq_along(fair_checker_list),
  ~ bind_rows(fair_checker_list[[.x]]))

fuji_summary <-
  map_dfr(
    seq_along(fuji_local_list_2021), 
    ~ bind_rows(fuji_local_list_2021[[.x]][["summary"]], .id = "id") |>
      mutate(rd_id = fuji_local_list_2021[[.x]][["request"]][["object_identifier"]], 
             .before = id)# |>
      #mutate(comment = fuji_local_list_2021[[.x]][["results"]][["test_debug"]][[2]])
    )

