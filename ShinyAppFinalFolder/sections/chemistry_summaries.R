# ---- prepare the small tables used before opening observation plots ----
chemistry_summaries <- function(files) {
  chemistry <- readRDS(files[["Master_Chem_20261029.rds"]])
  data.table::setDT(chemistry)
  data.table::set(chemistry, j = c("LTER", "units"), value = NULL)
  by_year <- chemistry[is.finite(value),
    .(num_streams = data.table::uniqueN(Stream_Name), num_observations = .N),
    by = .(year, variable)]
  by_site <- chemistry[is.finite(value),
    .(first_year = min(year), last_year = max(year), observations = .N),
    by = .(Stream_Name, variable)]
  means <- chemistry[, .(value = mean(value, na.rm = TRUE)), by = .(Stream_Name, variable)]
  summaries <- list(
    by_year = as.data.frame(by_year),
    by_site = split(as.data.frame(by_site), by_site$Stream_Name),
    means = tidyr::pivot_wider(as.data.frame(means), names_from = variable, values_from = value),
    chemistry_sites = unique(chemistry$Stream_Name),
    dates = range(chemistry$date, na.rm = TRUE),
    solutes = unique(chemistry$variable)
  )
  rm(chemistry)
  invisible(gc())

  pairs <- readRDS(files[["CQ_20261029.rds"]])
  valid_pairs <- is.finite(pairs$Qcms) & pairs$Qcms > 0 & is.finite(pairs$value) & pairs$value > 0
  summaries$paired_counts <- table(pairs$Stream_Name[valid_pairs])
  summaries$paired_sites <- unique(pairs$Stream_Name)
  summaries$paired_solutes <- unique(pairs$variable)
  summaries$file_hashes <- unname(tools::md5sum(files))
  summaries
}
