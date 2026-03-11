library(DBI)
devtools::load_all(here::here("../../omopgenerics"))
devtools::load_all(here::here("../../CDMConnector"))
library(OmopSketch)
library(odbc)
library(RPostgres)
library(duckdb)

open_omop_dataset <- function(dir, format) {
  list_directories <- function(path) {
    purrr::set_names(list.dirs(path, recursive = FALSE), ~ basename(.))
  }
  open_arrow_datasets <- function(path) {
    purrr::map(list_directories(path), arrow::open_dataset, format = format)
  }
  if (!dir.exists(dir)) {
    stop("Specified directory doesn't exist!")
  }
  if (!format %in% c("parquet", "csv")) {
    stop("Invalid specified format! Should be `parquet` or `csv`")
  }
  if (!dir.exists(file.path(dir, "public", "omop"))) {
    stop("`public/omop` folder should exist in specified directory!")
  }
  omop_dataset <- list()
  omop_dataset$public <- purrr::map(
    list_directories(file.path(dir, "public")),
    open_arrow_datasets
  )
  if ("private" %in% basename(list.dirs(dir, recursive = FALSE))) {
    omop_dataset$private <- open_arrow_datasets(file.path(dir, "private"))
  }
  return(omop_dataset)
}

preprocess_dataset <- function(ds) {
  ids <- ds$public$omop$observation_period |> 
    dplyr::filter(observation_period_end_date < observation_period_start_date) |>
    dplyr::collect() |> 
    dplyr::pull(person_id)
  ds$public$omop$person <- ds$public$omop$person |> 
    dplyr::filter(!person_id %in% ids)
  ds$public$omop$observation_period <- ds$public$omop$observation_period |> 
    dplyr::filter(!person_id %in% ids)
  ds$public$omop$visit_occurrence <- ds$public$omop$visit_occurrence |> 
    dplyr::filter(!person_id %in% ids)
  ds$public$omop$drug_exposure <- ds$public$omop$drug_exposure |> 
    dplyr::filter(!person_id %in% ids)
  ds$public$omop$person <- ds$public$omop$person |> 
    dplyr::mutate(location_id = ifelse(is.na(location_id), 1, location_id))
  return(ds)
}

get_cdm_connector <- function(dir, cdm_name, write_prefix) {
  data <- preprocess_dataset(open_omop_dataset(dir, format = "parquet"))
  cdmFromDatasets(data$public$omop, cdm_name, write_prefix)
}

# TO BE UPDATED
ds_dir <- ""
cdmName <- ""

# DO NOT CHANGE
cdmSchema <- "main"
writeSchema <- "main"
prefix <- "_out_"
minCellCount <- 5

cdm <- get_cdm_connector(ds_dir, cdmName, prefix)

source("RunConceptCounts.R")
