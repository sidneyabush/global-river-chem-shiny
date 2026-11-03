# ---- setup ----
# look for the prepared release before starting any package downloads
file_names <- c("Master_Chem_20261029.rds", "CQ_20261029.rds", "Spatial_20261102.rds")
# read shared files from box or another folder outside the project
data_folder <- Sys.getenv("SHINY_DATA_DIR", unset = "data")
data_files <- setNames(file.path(data_folder, file_names), file_names)
# accept files beside the app in existing project copies
use_existing <- !file.exists(data_files) & file.exists(file_names)
data_files[use_existing] <- file_names[use_existing]
summary_file <- file.path(data_folder, "App_Chemistry_Summaries.rds")
if (!file.exists(summary_file)) summary_file <- "App_Chemistry_Summaries.rds"
missing_files <- data_files[!file.exists(data_files)]
if (length(missing_files)) {
  stop("Missing app data: ", paste(missing_files, collapse = ", "),
    ". Set SHINY_DATA_DIR to the folder containing the prepared files and run the app again.", call. = FALSE)
}

source("install_packages.R", local = TRUE)
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(leaflet)
  library(ggplot2)
  library(lubridate)
})

# ---- app sections ----
# load the shared settings before creating the pages and outputs
source(file.path("sections", "chemistry_summaries.R"), local = TRUE)
source(file.path("sections", "data_and_settings.R"), local = TRUE)
source(file.path("sections", "page_layout.R"), local = TRUE)
source(file.path("sections", "interactive_outputs.R"), local = TRUE)

shinyApp(ui = overview_ui, server = overview_server)
