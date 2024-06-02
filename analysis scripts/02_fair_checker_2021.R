library(httr)
library(tidyverse)

source("R/01_rdm_ids_2021.R")

fair_checker_api <- function(rd_id){
  
  headers = c(
    `accept` = 'application/json')
 
  data <- list(url = rd_id,
               test_debug = TRUE)
  
  res <- httr::GET(url = 'https://fair-checker.france-bioinformatique.fr/api/check/metrics_all', 
                    httr::add_headers(.headers=headers), 
                    query = data,
                    encode = "json")  
  print(res)
  fair_checker_parsed <- content(res)
  return(fair_checker_parsed)
}

test <- fair_checker_api(charite_rd_2021$best_identifier[1])

fair_checker_list <- map(charite_rd_2021$best_identifier, fair_checker_api)
# 18:42 - 19:23

#rm(rate, test, fair_enough_server, headers, save_data_xlsx, slow_fair_enough)

save.image(file = "./output/Rdata/fair-enough data/fair_enough_each_metric.Rdata")
