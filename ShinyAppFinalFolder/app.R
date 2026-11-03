# ---- setup ----
# look for the prepared release before starting any package downloads
file_names <- c("Master_Chem_20261029.rds", "CQ_20261029.rds", "Spatial_20261102.rds")
data_files <- setNames(file.path("data", file_names), file_names)
# accept files beside the app in existing project copies
use_existing <- !file.exists(data_files) & file.exists(file_names)
data_files[use_existing] <- file_names[use_existing]
missing_files <- data_files[!file.exists(data_files)]
if (length(missing_files)) {
  stop("Missing app data: ", paste(missing_files, collapse = ", "),
    ". Put the prepared files in the data folder and run the app again.", call. = FALSE)
}

source("install_packages.R", local = TRUE)
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(leaflet)
  library(ggplot2)
  library(ggpmisc)
  library(lubridate)
})

# ---- app sections ----
# load the shared settings before creating the pages and outputs
source(file.path("sections", "data_and_settings.R"), local = TRUE)
source(file.path("sections", "page_layout.R"), local = TRUE)
source(file.path("sections", "interactive_outputs.R"), local = TRUE)

shinyApp(ui = overview_ui, server = overview_server)
