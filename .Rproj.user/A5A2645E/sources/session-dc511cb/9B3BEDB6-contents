#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# This script queries the local FUJI server ----
# Current code owner Anastasiia Iarkaeva (jaatnew@gmail.com)
# In acknowledgement to the original code owner Jan Taubitz
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(httr)
library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load data 
source("R/01_rdm_ids_2021.R")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FUJI local server ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Commands to start local F-UJI server in docker
system2(command = "pwd")

system2(command = "docker",
        args    = c("ps -a"))

# Function to query local FUJI server with guids
fuji_local_server <- function(rd_id){
  
  headers = c(`accept` = 'application/json',
              `Authorization` = 'Basic dGVzdDp0ZXN0',
              `Content-Type` = 'application/json')
  
  data <- list(metadata_service_endpoint = "http://ws.pangaea.de/oai/provider", 
               # metadata_service_endpoint http://ws.pangaea.de/oai/provider gives less scores?!
               metadata_service_type = "oai_pmh",
               object_identifier = rd_id,
               test_debug = TRUE,
               use_datacite = TRUE)
  
  res <- httr::POST(url = 'http://localhost:1071/fuji/api/v1/evaluate', 
                    httr::add_headers(.headers=headers), 
                    body = data, 
                    encode = "json")  
  print(res)
  fuji_local_parsed <- content(res)
  #return(fuji_local_parsed)
}

test <- fuji_local_server('https://www.addgene.org/137191/')

# Create vector with research data ids
charite_rd_2021_guids <- charite_rd_2021 |> pull(best_identifier)
 fuji_guid_ids <- fuji_guid |> pull(guid)

# Sample for testing purposes
sample <- sample(fuji_guid_ids, 2)
fuji_local_list <- map(sample, fuji_local_server)

# Query larger set of ids with map()
#fuji_local_list <- map(charite_rd_2021_guids, fuji_local_server)

# Create rate delay (not necessary for local server)
rate <- rate_delay(20)
slow_fuji <-
  slowly(fuji_local_server, rate = rate, quiet = FALSE)

fuji_local_list <-
  map(charite_rd_2021$best_identifier, slow_fuji)


# Save data locally
save(fuji_local_list, file = "output/Rdata/fuji_local_list_2021_2024_02_18.Rdata")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Remove unused objects ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(rate, test, fuji_local_server, headers, save_data_xlsx, slow_fuji)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# End ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Help
# https://curlconverter.com/#r curl translater
# https://developer.ibm.com/articles/what-is-curl-command/ how does curl work
# https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/