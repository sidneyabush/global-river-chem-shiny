# Global River Chemistry Shiny app

[Open the live app](https://sidneyabush.shinyapps.io/global-river-chem-shiny/)

## Run locally

- Install [R 4.4 or newer](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
- [Download and unzip the project](https://github.com/sidneyabush/global-river-chem-shiny/archive/refs/heads/main.zip). Open `ShinyAppFinalFolder/global-river-chem-shiny.Rproj` in RStudio.
- Download the app inputs from `SiSyn/spatial-data-extractions/spatial-data/shiny-app-inputs/` in Box. Keep `Master_Chem_20261029.rds`, `CQ_20261029.rds`, and `Spatial_20261102.rds` together in one local folder. Include `App_Chemistry_Summaries.rds` if available for faster startup.
- In the RStudio console, run the line below with the path to the folder containing the downloaded files.

```r
Sys.setenv(SHINY_DATA_DIR = "/path/to/shiny-app-inputs")
```

- Open `app.R` and click **Run App**. If the button is missing, run `source("install_packages.R")` in the RStudio console first. Stay online for package installation and the map.
