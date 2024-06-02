#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the FAIR Enough server ----
# Contact: jan.taubitz@charite.de
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare R environment----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)
library(jsonlite)
#set_config(use_proxy(url="147.75.34.83",port=80))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load GUIS from FUJI Assessment
#load("./output/Rdata/fuji data/fair_fuji_2021.Rdata")
#load("./output/Rdata/fair_enough_list.Rdata")
load("./output/Rdata/fair-enough data/fair_enough_each_metric.Rdata")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FAIR enough data ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# API function to query FAIR Enough server with ids
fair_enough_server <- function(rd_id) {
  headers = c(`accept` = 'application/json',
              `Content-Type` = 'application/json')
  
  data <- list(subject = rd_id,
                #resource_uri = rd_id,
               collection = "fair-enough-data")
               #collection = "fair-evaluator-maturity-indicators")
  
  res <-
    httr::POST(
      #url = 'https://api.fair-enough.semanticscience.org/evaluations', #first attempt - 
      #not sufficient - url = 'https://api.fair-enough.semanticscience.org/evaluations',
      
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_identifier_persistence', # F1
     url = 'https://w3id.org/FAIR_Tests/tests/gen2_unique_identifier', # F1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_identifier_persistence', # F1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_structured_metadata', # F2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_grounded_metadata', # F2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_identifier_in_metadata', # F3
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_identifier_in_metadata', # F3
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_searchable', # F4
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_protocol', # A1.1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_protocol', # A1.1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_authorization', # A1.2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_authorization', # A1.2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_persistence', # A2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_kr_language_strong', # I1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_kr_language_strong', # I1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_kr_language_weak', # I1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_data_kr_language_weak', # I1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_uses_fair_vocabularies_strong', # I2

     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_uses_fair_vocabularies_weak', # I2
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_contains_outward_links', # I3
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_includes_license_strong', # R1.1
     # url = 'https://w3id.org/FAIR_Tests/tests/gen2_metadata_includes_license_weak', # R1.1
     

      httr::add_headers(.headers = headers),
      body = data,
      encode = "json"
      
    )
  
  print(res)
  
  fair_enough_raw <- content(res, as = "raw") #"parsed"
}

#test <- fair_enough_server('https://www.addgene.org/137191/')
#json_test <- fromJSON(rawToChar(as.raw(test)))

# Query larger set of ids with map()
# Use slowly() to set rate limit
# rate <- rate_delay(20)
# slow_fair_enough <-
#   slowly(fair_enough_server, rate = rate, quiet = TRUE)

fair_enough_scores <-
  map(charite_rd_2021$best_identifier, fair_enough_server)
# Time 20:00 - 

# F1_metadata_identifier_persistence <-
#    map(charite_rd_2021$best_identifier, slow_fair_enough)
# # Time 19:50 - 00:50 = 5h

# F1_unique_identifier <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 13:23 - 13:28

# F1_data_identifier_persistence <- 
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 21:07 - 22:17 - 1h 10m

# F2_structured_metadata <-
#   map(charite_rd_2021$best_identifier, slow_fair_enough)
# # Time 19:09 - 22:59 - 3 h 50 m

# F2_grounded_metadata <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 14:39 - 15:47

# F3_metadata_identifier_in_metadata <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 08:22 - 09:31 - 1h 9m

# F3_data_identifier_in_metadata <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 09:32 - 10:44 - 1h 12m

# F4_searchable <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 16:56 - 18:18 - 1h 22m

# A1.1_metadata_protocol <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 12:17 - 14:31 - 2h 14m

# A1.1_data_protocol <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 14:34 - 16:45 - 3h 11m

# A1.2_metadata_authorization <-
#   map(charite_rd_2021$best_identifier, slow_fair_enough)
# # Time 16:07 - 18:50 = 2h 43

# A1.2_data_authorization <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 14:48 - 16:05 - 1h 17m

# A2_metadata_persistence <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 16:08 - 17:19 - 1h 11m

# I1_data_kr_language_strong <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 17:23 - 18:40 - 1h 17m

# I1_metadata_kr_language_strong <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 18:41 - 19:50 - 1h 9m

# I1_metadata_kr_language_weak <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 19:55 - 21:03 - 2h 8m

# I1_data_kr_language_weak <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 21:10 - 22:17 - 1h 7m

# I2_metadata_uses_fair_vocabularies_strong <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 22:19 - 23:28 - 1h 9m
# 
# I2_metadata_uses_fair_vocabularies_weak <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time ? - 11:00

# I3_metadata_contains_outward_links <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 11:05 - 12:14 - 1h 9m

# R1.1_metadata_includes_license_strong <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 12:15 - 13:25 - 1h 10m

# R1.1_metadata_includes_license_weak <-
#   map(charite_rd_2021$best_identifier, fair_enough_server)
# # Time 13:37 - 14:37 - 1h

#rm(rate, test, fair_enough_server, headers, save_data_xlsx, slow_fair_enough)

save.image("./output/Rdata/fair-enough data/fair_enough_each_metric.Rdata")
