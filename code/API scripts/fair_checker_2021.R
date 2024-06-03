#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the local FAIR-Checker server ----
# Current code owner Anastasiia Iarkaeva (jaatnew@gmail.com)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(httr)
library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data 
source("R/01_rdm_ids_2021.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FAIR-Checker querying ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Function to query local FAIR-Checker API with guids
fair_checker_api <- function(rd_id){
  
  headers = c(`accept` = 'application/json')
 
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

# Testing the functionality of the API
test <- fair_checker_api(charite_rd_2021$best_identifier[1])

# Query larger set of ids with map()
fair_checker_list <- map(charite_rd_2021$best_identifier, fair_checker_api)
# 18:42 - 19:23

# Save data locally
save.image(file = "./output/Rdata/fair checker/fair_checker_api_list.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove unused objects ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(rate, test, fair_checker_api, headers, save_data_xlsx)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Help
# https://curlconverter.com/#r curl translater
# https://developer.ibm.com/articles/what-is-curl-command/ how does curl work
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/