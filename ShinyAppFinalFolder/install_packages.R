# ---- package setup ----
# install only missing packages so later launches can reuse them
if (getRversion() < "4.4") {
  stop("Please install R 4.4 or newer before running this app.", call. = FALSE)
}

packages <- c("shiny", "bslib", "dplyr", "tidyr", "leaflet", "ggplot2", "ggpmisc",
  "lubridate", "DT", "htmltools", "htmlwidgets")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
  missing_packages <- missing_packages[
    !vapply(missing_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages)) {
    stop("Could not load these packages: ", paste(missing_packages, collapse = ", "),
      ". Check the installation output in the R console and run the app again.", call. = FALSE)
  }
}
