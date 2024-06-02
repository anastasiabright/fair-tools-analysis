cat("\014") # Clear your console
rm(list = ls()) # Clear your environment

library(httr)
library(tidyverse)

source("R/01_rdm_ids_2021.R")
#load("./output/Rdata/eva data/zenodo_results.Rdata")
load("./output/Rdata/selected_articles.Rdata")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load guids from from previous data processing ----
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Function to query local FUJI server with guids
faireva_local_server <- function(rd_id){
  
  headers = c(
    `accept` = 'application/json',
    #`accept` = '*/*',
    `Content-Type` = 'application/json'
    #`Content-Type` = 'text/xml'#,
    #`access_token` = 'ACCESS_TOKEN' # zenodo - worked
    #`Authorization` = dryad - not author: https://github.com/datadryad/dryad-app/blob/main/documentation/apis/api_accounts.md
    #`Authorization` = figshare - not author: https://docs.figshare.com/
    #`Authorization` = mendeley - not author: https://data.mendeley.com/api/docs/#section/Authentication/Obtain-a-Token-Using-Authorization-Code-flow

    )
  
  data <- list(id = rd_id,
               lang = "es",
               #oai_base = "https://zenodo.org/oai2d", # Zenodo - OAI
               #oai_base = "https://data.mendeley.com/oai", # Mendeley Data - OAI? not listed on re3data
               #oai_base = "https://heidata.uni-heidelberg.de/oai", # error on server!
               #oai_base = "https://openneuro.org/crn/graphql", # REST (GraphL)
               #oai_base = "https://datadryad.org/api/v2/datasets/", # REST! (OAI-PMH not available https://www.re3data.org/repository/r3d100000044)
               oai_base = "https://api.figshare.com/v2/oai", # Figshare - OAI-PMH, but not working
               repo = "oai-pmh"
               )
  
  # 2.4 https://www.openarchives.org/OAI/openarchivesprotocol.html#UniqueIdentifier
  
  res <- httr::POST(url = 'http://localhost:9090/v1.0/rda/rda_all',
                    httr::add_headers(.headers=headers), 
                    body = data,
                    encode = "json")  
  print(res)
  faireva_local_raw <- content(res)
}

# test_oai <- faireva_local_server('https://www.addgene.org/137191/')
# test_oai2 <- faireva_local_server('https://doi.org/10.6084/m9.figshare.14524716.v1')
# test_oai3 <- faireva_local_server('http://hdl.handle.net/10261/157765')
# test_oai4 <- faireva_local_server('https://doi.org/10.5281/zenodo.4454489')

zenodo_ds <- charite_rd_2021 |>
  filter(repository == "Zenodo") # 39
test_zenodo <- faireva_local_server(zenodo_ds$best_identifier[1])
zenodo_results <- map(zenodo_ds$best_identifier, faireva_local_server)

# pdb_ds <- charite_rd_2021 |>
#   filter(repository == "wwPDB")
# pdb_results <- map(pdb_ds$best_identifier, faireva_local_server)

# mendeley_ds <- charite_rd_2021 |>
#   filter(repository == "Mendeley Data")
# test <- faireva_local_server(mendeley_ds$best_identifier[1])
# mendeley_results <- map(mendeley_ds$best_identifier, faireva_local_server)
# 
# heidata_ds <- charite_rd_2021 |>
#   filter(repository == "heiDATA")
# test <- faireva_local_server(heidata_ds$best_identifier[1])

# openneuro_ds <- charite_rd_2021 |>
#   filter(repository == "OpenNeuro")
# test <- faireva_local_server(openneuro_ds$best_identifier[1])

# dryad_ds <- charite_rd_2021 |>
#   filter(repository == "DRYAD")
# test_dryad <- faireva_local_server(dryad_ds$best_identifier[1])

# figshare_ds <- charite_rd_2021 |>
#   filter(repository == "figshare")
# test <- faireva_local_server(figshare_ds$best_identifier[1])

