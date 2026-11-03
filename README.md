# Global River Chemistry Shiny app

Explore river chemistry, watershed characteristics, and concentration–discharge relationships.

## Run locally

- Install [R 4.4 or newer](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
- [Download the project ZIP](https://github.com/sidneyabush/global-river-chem-shiny/archive/refs/heads/main.zip) and unzip it. Inside `ShinyAppFinalFolder`, open `global-river-chem-shiny.Rproj` in RStudio.
- Open `app.R` and click **Run App**. If RStudio needs Shiny before it can show the button, run `source("install_packages.R")` in the R console first.
- Stay online for the first launch so R can install missing packages. The interactive basemap also uses an internet connection.

First-time package installation passed on macOS with R 4.5 using an empty package library. The clean setup also rendered both CAMREX greenup cycles with their dates, coverage percentages, and flags.

From the repository root, you can also run `shiny::runApp("ShinyAppFinalFolder")` once Shiny is installed.

## App data

Use the three prepared RDS files below. In a current copy of the project, they sit beside `app.R`. If you receive the files separately, put them in `ShinyAppFinalFolder/data/` and keep the filenames unchanged. The app checks that folder first.

| File | What it contains |
| --- | --- |
| [`Master_Chem_20261029.rds`](ShinyAppFinalFolder/Master_Chem_20261029.rds) | Stream chemistry observations |
| [`CQ_20261029.rds`](ShinyAppFinalFolder/CQ_20261029.rds) | Paired chemistry and discharge observations |
| [`Spatial_20261102.rds`](ShinyAppFinalFolder/Spatial_20261102.rds) | App-ready watershed summaries and both CAMREX greenup cycles, including dates, coverage percentages, and flags |

### Refresh the spatial data

Ask the project maintainer for the latest verified Updated release. The current source file is `deliverables/updated-annual-camrex-npp.rds` in the maintainer’s `data-workflow_spatial` workspace. For setup, the maintainer can share the app-ready `Spatial_20261102.rds` directly. The full release RDS is used to rebuild that app-ready file.

To rebuild it yourself, get `updated-annual-camrex-npp.rds` from the maintainer and place it in `ShinyAppFinalFolder/data/`. Open Terminal in `ShinyAppFinalFolder` and run

```bash
Rscript prepare_spatial.R data/updated-annual-camrex-npp.rds
```

The command writes `Spatial_20261102.rds` beside `app.R`. If there is an older copy in `data/`, replace it with the newly generated file so the app loads the refresh. Restart the app and open **CAMREX Greenup** to see both cycles.

## Edit the app

- `app.R` starts the app. `install_packages.R` lists the packages it needs.
- `sections/data_and_settings.R` prepares the data and defines shared labels and styles.
- `sections/page_layout.R` defines the tabs, menus, and plot locations.
- `sections/interactive_outputs.R` connects the filters and menus to the maps, tables, and plots.

After editing, save the file, click **Stop**, and click **Run App** again. Use `app.R` for this version. The [2024 workshop archive](workshop-2024-archive/) holds the older app scripts, data, and setup files for reference.
