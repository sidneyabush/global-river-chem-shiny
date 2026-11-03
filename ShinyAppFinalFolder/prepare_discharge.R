# ---- rebuild the five corrected daily gauge series ----
# run from the app folder with the workflow folder and a source folder
# the source folder needs the five raw gauge csvs and site-reference.rds
# the gauge csvs come from the shared discharge_files drive folder
# save the site_reference_table_lter_v3 sheet as an rds with its original column names
# this historical rebuild needs the previously prepared master_q_20260928.rds
# the workflow retrieves the archived daily source records where needed
library(data.table)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("provide the discharge workflow folder and the gauge source folder")
workflow <- args[1]
sources <- args[2]
source(file.path(workflow, "discharge_functions.R"))

files <- c("SHINGLE_CREEK_Q", "LUQ_Q1_Q_fill", "LUQ_Q2_Q_fill", "LUQ_Q3_Q_fill", "Site7_Q")
reference <- as.data.frame(readRDS(file.path(sources, "site-reference.rds")))
reference <- reference[reference$Discharge_File_Name %in% files, ]
cache <- new.env(parent = emptyenv())
shingle <- file.path(workflow, "discharge-inputs", "20261027_USGS_05288705_daily_discharge.json")
results <- list()
for (gauge in files) {
  original <- fread(file.path(sources, paste0(gauge, ".csv")))
  restored <- restore_original_discharge(original, gauge, reference, cache, shingle)
  results[[gauge]] <- prepare_daily_discharge(restored, gauge, reference,
    c("Discharge", "Q", "Qcms"), "Date")
}
corrected <- as.data.table(attach_discharge_sites(rbindlist(results), reference))

# replace only the corrected gauges in the published-site export
daily <- as.data.table(readRDS("Master_Q_20260928.rds"))
corrected <- corrected[Stream_Name %in% daily$Stream_Name]
daily <- rbindlist(list(daily[!Discharge_File_Name %in% files], corrected), fill = TRUE)
daily <- unique(daily, by = c("Stream_Name", "Date", "Qcms"))
if (anyDuplicated(daily, by = c("Stream_Name", "Date"))) {
  stop("the daily export still has conflicting site dates")
}
saveRDS(as.data.frame(daily), "Master_Q_corrected.rds")
