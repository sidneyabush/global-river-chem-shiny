# ---- prepare chemistry and paired flow for the full release ----
# run from the app folder with the chemistry and validated combined exports
library(data.table)
inputs <- commandArgs(trailingOnly = TRUE)
if (length(inputs) != 2) stop("provide the chemistry and validated combined exports")

# convert the openriver solutes from micromoles to mg per liter
# nitrogen and phosphorus are reported as elemental mass
weights <- c(Ca = 40.078, Mg = 24.305, K = 39.0983, Na = 22.98976928,
  DSi = 28.0855, NO3 = 14.0067, NOx = 14.0067, SRP = 30.973762, PO4 = 30.973762,
  NH4 = 14.0067, TN = 14.0067, TP = 30.973762, Cl = 35.45, SO4 = 96.06)
prepare_solutes <- function(rows) {
  rows <- rows[variable %in% names(weights)]
  if (any(is.na(rows$units) | rows$units != "uM")) stop("unexpected chemistry units")
  rows[, value := value * unname(weights[variable]) / 1000]
  rows[, units := "mg/L"]
  # retain nitrate and nitrate plus nitrite as separate solutes
  # glass reports soluble reactive phosphorus and phosphate together as dip
  rows[variable %in% c("SRP", "PO4"), variable := "DIP"]
  rows[, date := as.Date(date)]
  rows[!is.na(date)]
}

chemistry <- prepare_solutes(fread(inputs[1],
  select = c("LTER", "Stream_Name", "variable", "units", "value", "date")))
chemistry[, year := as.integer(format(date, "%Y"))]
chemistry[, `:=`(min = min(date), max = max(date)), by = .(LTER, Stream_Name)]

paired <- fread(inputs[2],
  select = c("LTER", "Stream_Name", "date", "variable", "value", "units", "Qcms", "Q_Flag"))
paired <- prepare_solutes(paired[!is.na(Qcms)])
saveRDS(as.data.frame(chemistry), "Master_Chem_20261029.rds")
saveRDS(as.data.frame(paired), "CQ_20261029.rds")

# refresh the small startup summaries whenever the observation files change
rm(chemistry, paired)
invisible(gc())
source(file.path("sections", "chemistry_summaries.R"))
files <- c("Master_Chem_20261029.rds", "CQ_20261029.rds")
saveRDS(chemistry_summaries(setNames(files, files)), "App_Chemistry_Summaries.rds")
