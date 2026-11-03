# Global River Chemistry Shiny app

Explore river chemistry, watershed characteristics, and concentration–discharge relationships.

## Run locally

- Install [R 4.4 or newer](https://cran.r-project.org/) and [RStudio Desktop](https://posit.co/download/rstudio-desktop/).
- [Download the project ZIP](https://github.com/sidneyabush/global-river-chem-shiny/archive/refs/heads/main.zip) and unzip it. Inside `ShinyAppFinalFolder`, open `global-river-chem-shiny.Rproj` in RStudio.
- Open `app.R` and click **Run App**. If RStudio needs Shiny before it can show the button, run `source("install_packages.R")` in the R console first.
- Stay online for the first launch so R can install missing packages. The interactive basemap also uses an internet connection.

From the repository root, you can also run `shiny::runApp("ShinyAppFinalFolder")` once Shiny is installed.

## App data

The ZIP includes all three prepared data files beside `app.R` in `ShinyAppFinalFolder`. Leave them there and run the app. The links below also point to each file on GitHub. Open the file and click **Download raw file** if you need a separate copy.

If you get the files separately, create `ShinyAppFinalFolder/data` and put all three files there with their names unchanged.

| File | What it contains |
| --- | --- |
| [`Master_Chem_20261029.rds`](ShinyAppFinalFolder/Master_Chem_20261029.rds) | Stream chemistry observations |
| [`CQ_20261029.rds`](ShinyAppFinalFolder/CQ_20261029.rds) | Paired chemistry and discharge observations |
| [`Spatial_20261102.rds`](ShinyAppFinalFolder/Spatial_20261102.rds) | Site information and watershed characteristics |

## Edit the app

- `app.R` starts the app. `install_packages.R` lists the packages it needs.
- `sections/data_and_settings.R` prepares the data and defines shared labels and styles.
- `sections/page_layout.R` defines the tabs, menus, and plot locations.
- `sections/interactive_outputs.R` connects the filters and menus to the maps, tables, and plots.

After editing, save the file, click **Stop**, and click **Run App** again. Use `app.R` for this version. The older Shiny scripts are retained for reference.
