# ---- read in data for tables, plotting, mapping ----

# reuse the small summaries if they match the two observation files
observation_files <- data_files[c("Master_Chem_20261029.rds", "CQ_20261029.rds")]
summaries <- if (file.exists("App_Chemistry_Summaries.rds")) readRDS("App_Chemistry_Summaries.rds")
if (!identical(summaries$file_hashes, unname(tools::md5sum(observation_files)))) {
  summaries <- chemistry_summaries(observation_files)
}
master_chem_summary <- summaries$by_year
site_chemistry <- summaries$by_site
solute_means <- summaries$means
paired_counts <- summaries$paired_counts
chemistry_dates <- summaries$dates

# put the solutes with the most observations at the top of the plot
solute_observation_totals <- master_chem_summary %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(total = sum(num_observations), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(total), variable)

solute_choices <- c(
  "Dissolved silicon (DSi)" = "DSi", "Nitrate (NO₃)" = "NO3",
  "Nitrate plus nitrite (NOₓ)" = "NOx",
  "Ammonium (NH₄)" = "NH4", "Dissolved inorganic phosphorus (DIP)" = "DIP",
  "Total nitrogen (TN)" = "TN", "Total phosphorus (TP)" = "TP",
  "Calcium (Ca)" = "Ca", "Magnesium (Mg)" = "Mg", "Potassium (K)" = "K",
  "Sodium (Na)" = "Na", "Chloride (Cl)" = "Cl", "Sulfate (SO₄)" = "SO4"
)
master_chem_summary$variable <- factor(master_chem_summary$variable,
  levels = rev(solute_observation_totals$variable))


# ---- spatial data products from the updated release ----
# format units consistently across displayed fields and source tables
display_units <- function(labels) {
  replacements <- c("g C/m2/year" = "g C m⁻² yr⁻¹", "g C/m²/year" = "g C m⁻² yr⁻¹",
    "mm/year" = "mm yr⁻¹", "mg/L" = "mg L⁻¹", "m3/s" = "m³ s⁻¹",
    "m3/day" = "m³ d⁻¹", "m³/day" = "m³ d⁻¹", "people/km2" = "people km⁻²",
    "people/km²" = "people km⁻²", "count/1,000 km2" = "count (1,000 km²)⁻¹",
    "count/1,000 km²" = "count (1,000 km²)⁻¹", "kg/ha" = "kg ha⁻¹",
    "P2O5/ha" = "P₂O₅ ha⁻¹", "K2O/ha" = "K₂O ha⁻¹",
    "km2" = "km²", "million m3" = "million m³", "deg C" = "°C", "(degrees)" = "(°)")
  for (unit in names(replacements)) {
    labels <- gsub(unit, replacements[[unit]], labels, fixed = TRUE)
  }
  sub(" \\(([^()]*\\([^()]*\\)[^()]*)\\)$", " [\\1]", labels)
}

# keep the solute codes in the data and use subscripts on labels
solute_notation <- function(labels) {
  labels[labels == "NO3"] <- "NO₃"
  labels[labels == "NOx"] <- "NOₓ"
  labels[labels == "NH4"] <- "NH₄"
  labels[labels == "SO4"] <- "SO₄"
  labels
}

# show the element used to report concentration
solute_units <- function(solutes) {
  basis <- c(DSi = "Si", NO3 = "N", NOx = "N", NH4 = "N", TN = "N", DIP = "P", TP = "P")
  elements <- unname(basis[solutes])
  ifelse(is.na(elements), "mg L⁻¹", paste0("mg ", elements, " L⁻¹"))
}

data_source_table <- read.delim(text = "
  Data product | Variable | Spatial resolution | Period or timing | Units
  BESSv2 | Gross primary production (GPP) | 180 arcseconds | 1982-2022; annual | g C/m²/year
  ERA5-Land | Mean annual precipitation (MAP) | About 11,100 m | 2000-2025; annual | mm
  ERA5-Land | Mean annual temperature (MAT) | About 11,100 m | 2000-2025; annual | °C
  ERA5-Land | Evapotranspiration (ET) | About 11,100 m | 2000-2025; annual | mm
  ERA5-Land | Potential evaporation (PET) | About 11,100 m | 2000-2025; annual | mm
  ERA5-Land | Snow cover | About 11,100 m | 2000-2025; annual | % of watershed area
  ERA5-Land | Peak snow water equivalent (SWE) | About 11,100 m | 2000-2025; annual | mm
  LandScan Global | Population and data coverage | About 1 km | 2000-2024; annual | People, people/km², % of watershed area
  Global Dam Watch v1.0 | Dams | Point locations | 2024 release; static | Count, count/1,000 km², million m³, MW
  NPKGRIDS | Fertilizer application index | About 5,600 m | Circa 2020; static | kg/ha (N, P₂O₅, K₂O)
  HydroWASTE v1.0 | Wastewater treatment plants | Point locations | 2022 release; static | Count, count/1,000 km², people, m³/day, treatment class
  HydroRIVERS routing | Distance to coast and outlet | River network | Static | km
  MODIS Terra MOD17A3HGF v6.1 | Net primary production (NPP) | 500 m | 2001-2025; annual | g C/m²/year
  MODIS Terra and Aqua MCD12Q2 v6.1 | First greenup (Cycle 0) | 500 m | 2001-2024 | Calendar date
  Global Land Cover with Fine Classification System (GLC-FCS30D) | Land cover | 30 m | 1985–2022; annual, with interpolation between source years where available | % of watershed area
  WorldClim v2.1 | Elevation | 30 arcseconds | Static | m above sea level
  WorldClim v2.1 | Basin slope | 30 arcseconds | Static | °
  Global Lithological Map (GLiM) | Lithology | 30 arcminutes | Static | Rock class, % of watershed area
  GlobPermafrost PERPROB (Obu et al., 2018) | Permafrost occurrence probability | 1 km | Static; modeled for 2000-2016; Northern Hemisphere land north of 25°N | % probability
", sep = "|", strip.white = TRUE, check.names = FALSE)

data_source_table$Units <- display_units(data_source_table$Units)

product_urls <- c(
  "ERA5-Land" = "https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land",
  "MODIS Terra MOD17A3HGF v6.1" = "https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD17A3HGF",
  "BESSv2" = "https://www.environment.snu.ac.kr/bessv2",
  "Global Land Cover with Fine Classification System (GLC-FCS30D)" = "https://zenodo.org/records/8239305",
  "LandScan Global" = "https://landscan.ornl.gov/",
  "Global Dam Watch v1.0" = "https://doi.org/10.6084/m9.figshare.25988293.v1",
  "NPKGRIDS" = "https://doi.org/10.6084/m9.figshare.24616050",
  "HydroWASTE v1.0" = "https://www.hydrosheds.org/products/hydrowaste",
  "HydroRIVERS routing" = "https://www.hydrosheds.org/products/hydrorivers",
  "MODIS Terra and Aqua MCD12Q2 v6.1" = "https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/products/MCD12Q2",
  "WorldClim v2.1" = "https://www.worldclim.org/data/worldclim21.html",
  "Global Lithological Map (GLiM)" = "https://doi.org/10.1594/PANGAEA.788537",
  "GlobPermafrost PERPROB (Obu et al., 2018)" = "https://doi.org/10.1594/PANGAEA.888600"
)

# read the watershed summaries
all_drivers_table <- readRDS(data_files[["Spatial_20261102.rds"]])
names(all_drivers_table) <- sub("^rock: ", "Rock: ", names(all_drivers_table))
# show the combined rock classes used in the grl paper
all_drivers_table[["Dominant Lithology"]] <- all_drivers_table[["GRL Lithology Group"]]
all_drivers_table[["GRL Lithology Group"]] <- NULL
# shorten the network name for all app views
network <- all_drivers_table[["Observation Network"]]
levels(network)[levels(network) == "Tanguro(Jankowski)"] <- "Tanguro"
all_drivers_table[["Observation Network"]] <- network
spatial_years <- attr(all_drivers_table, "year_coverage")
# display valid fractions as percentages and keep the saved data intact
fraction_fields <- names(all_drivers_table)[endsWith(names(all_drivers_table), "(fraction)")]
for (field in fraction_fields) {
  values <- all_drivers_table[[field]]
  values[!is.finite(values) | values < 0 | values > 1] <- NA_real_
  all_drivers_table[[field]] <- 100 * values
}
names(all_drivers_table) <- sub("(fraction)", "(%)", names(all_drivers_table), fixed = TRUE)
names(spatial_years) <- sub("(fraction)", "(%)", names(spatial_years), fixed = TRUE)
names(all_drivers_table) <- display_units(names(all_drivers_table))
# label each treatment class as a count of mapped plants
treatment_fields <- paste(c("Primary", "Secondary", "Advanced"), "Wastewater Treatment Plants")
is_treatment <- names(all_drivers_table) %in% treatment_fields
names(all_drivers_table)[is_treatment] <- paste(names(all_drivers_table)[is_treatment], "(count)")
names(spatial_years) <- display_units(names(spatial_years))
source_labels <- c("Chemistry Data" = "Chemistry", "Discharge Data" = "Discharge",
  "Watershed Boundary" = "Watershed boundary", "Drainage Area Source" = "Drainage area")
source_columns <- names(source_labels)
all_drivers_map <- all_drivers_table %>% select(-all_of(source_columns), -all_of(c("Has Discharge", "WRTDS")))

# use the site reference country in the chemistry controls
site_countries <- setNames(all_drivers_table$Country, all_drivers_table[["Stream Name"]])
chemistry_countries <- sort(unique(as.character(site_countries[summaries$chemistry_sites])))
paired_countries <- sort(unique(as.character(site_countries[summaries$paired_sites])))

# load each full observation table only when a plot first needs it
delayedAssign("master_chem", {
  rows <- readRDS(data_files[["Master_Chem_20261029.rds"]])
  rows <- rows[c("Stream_Name", "variable", "value", "date", "min", "max")]
  rows$Country <- unname(site_countries[rows$Stream_Name])
  rows
}, eval.env = environment(), assign.env = environment())

delayedAssign("cq_all_nodups", {
  rows <- readRDS(data_files[["CQ_20261029.rds"]])
  rows <- rows[c("Stream_Name", "variable", "value", "Qcms")]
  rows$Country <- unname(site_countries[rows$Stream_Name])
  rows
}, eval.env = environment(), assign.env = environment())

# point yzeron users to the provider catalog if the station pages are unavailable
yzeron <- all_drivers_table[["Observation Network"]] == "Yzeron"
all_drivers_table[["Chemistry Data"]][yzeron] <- "https://bdoh.inrae.fr/YZERON/"
yzeron_flow <- yzeron & nzchar(all_drivers_table[["Discharge Data"]])
all_drivers_table[["Discharge Data"]][yzeron_flow] <- "https://doi.org/10.57745/BNG6AE"

# give dataset links a consistent appearance in both tables
source_link <- function(url, label) {
  as.character(tags$a(href = url, class = "dataset-link", target = "_blank",
    rel = "noopener noreferrer", label))
}

# render only web links from the reference and escape the link text and addresses
dataset_links <- function(values, label) {
  values <- values[!is.na(values)]
  urls <- unique(unlist(regmatches(values,
    gregexpr("https?://[^[:space:];<>\"]+", values, ignore.case = TRUE))))
  # replace retired pages and api endpoints with public provider destinations
  replacements <- c(
    "https://streamstats.usgs.gov/streamstats/" = "https://streamstats.usgs.gov/ss/",
    "https://ddapi20-waterwebservices.rijkswaterstaat.nl/ONLINEWAARNEMINGENSERVICES/OphalenWaarnemingen" =
      "https://rijkswaterstaatdata.nl/waterdata/"
  )
  replace <- urls %in% names(replacements)
  urls[replace] <- unname(replacements[urls[replace]])
  urls <- unique(urls)
  vapply(seq_along(urls), function(index) {
    source_link(urls[index], if (length(urls) == 1) label else paste(label, index))
  }, character(1))
}

# keep related sources together without hiding any reference links
source_entry <- function(values, label) {
  links <- dataset_links(values, label)
  if (length(links) <= 1) return(paste(links, collapse = ""))
  as.character(tags$details(
    tags$summary(paste0(label, " (", length(links), " sources)")),
    lapply(links, function(link) tags$div(HTML(link)))
  ))
}
color_drivers <- names(all_drivers_map)[vapply(all_drivers_map, is.factor, logical(1))]

# keep dropdown labels short while retaining the full data field names
driver_choices <- function(fields) {
  choices <- setNames(fields, sub("(1985–2022 mean, %)", "(%)", fields, fixed = TRUE))
  labels <- sub("^NPP ", "Net Primary Production ", names(choices))
  names(choices) <- sub("^GPP ", "Gross Primary Production ", labels)
  solutes <- match(fields, unname(solute_choices))
  is_solute <- !is.na(solutes)
  names(choices)[is_solute] <- paste0(names(solute_choices)[solutes[is_solute]], " (", solute_units(fields[is_solute]), ")")
  groups <- dplyr::case_when(
    fields %in% unname(solute_choices) ~ "Solutes",
    grepl("^(Nitrogen|Phosphorus) Fertilizer Index", fields) ~ "Agriculture",
    grepl("^(Dam |Population|Wastewater|Primary Wastewater|Secondary Wastewater|Advanced Wastewater|Potassium Fertilizer Index)", fields) ~ "Human impacts",
    grepl("^(NPP |GPP |First Greenup|Second Greenup)", fields) ~ "Productivity and phenology",
    grepl("^(Precipitation|Mean Annual Temperature|Evapotranspiration|Potential Evaporation|Annual Maximum Snow Cover|Peak |Mean Permafrost Probability)", fields) ~ "Climate and snow",
    grepl("^(Land Cover:|Dominant Land Cover)", fields) ~ "Land cover",
    grepl("^(Rock:|Dominant Lithology)", fields) ~ "Lithology",
    grepl("^(Drainage Area|Distance to|Mean Basin Slope|Mean Elevation)", fields) ~ "Watershed characteristics",
    fields %in% c("Country", "Observation Network", "Latitude", "Longitude") ~ "Location and network",
    TRUE ~ "Other variables"
  )
  group_order <- c("Location and network", "Solutes", "Climate and snow",
    "Watershed characteristics", "Land cover", "Lithology", "Productivity and phenology",
    "Agriculture", "Human impacts", "Other variables")
  split(choices, factor(groups, levels = group_order), drop = TRUE)
}

# add site means to the map and exploration data
exploration <- left_join(all_drivers_map, solute_means, by = c("Stream Name" = "Stream_Name"))
map_color_options <- setdiff(names(exploration), c("Stream Name", "Latitude", "Longitude"))
quant_drivers <- setdiff(names(exploration)[vapply(exploration, is.numeric, logical(1))],
  c("Latitude", "Longitude"))

# use the same field names for the filter dropdown and its controls
exploration_filter_choices <- driver_choices(c("Latitude", "Longitude", quant_drivers))
exploration_filter_labels <- unlist(unname(exploration_filter_choices))

# describe how each spatial predictor was summarized
spatial_label <- function(variable) {
  if (variable %in% unname(solute_choices)) return(paste0(solute_notation(variable), " (", solute_units(variable), ")"))
  coverage <- spatial_years[[variable]]
  if (!is.null(coverage)) {
    summary <- if (startsWith(variable, "Dominant Land Cover")) {
      "largest average share by site"
    } else {
      "site average"
    }
    return(paste0(variable, "\n", summary, " · source period ", coverage$period))
  }
  if (variable %in% setdiff(map_color_options, c("Country", "Observation Network"))) {
    return(paste0(variable, "\nstatic"))
  }
  solute_notation(variable)
}

# list consecutive years as ranges while showing any gaps
format_site_years <- function(years) {
  if (!length(years)) return("No data")
  runs <- split(years, cumsum(c(TRUE, diff(years) != 1)))
  paste(vapply(runs, function(run) {
    paste(unique(range(run)), collapse = "–")
  }, character(1)), collapse = ", ")
}

site_year_details <- function(variables) {
  variables <- intersect(unique(variables), names(spatial_years))
  if (!length(variables)) return(NULL)
  details <- all_drivers_table[c("Stream Name", "Observation Network")]
  for (variable in variables) {
    coverage <- spatial_years[[variable]]
    heading <- paste0(variable, " · source ", coverage$period)
    details[[heading]] <- vapply(coverage$years[details[["Stream Name"]]],
      format_site_years, character(1))
  }
  details
}

# ---- make base map and add options for coloration ----
# allow a close fit to the sites at different map widths
site_map <- leaflet(all_drivers_map,
  options = leafletOptions(minZoom = 1, zoomSnap = 0, scrollWheelZoom = "center", maxBoundsViscosity = 1)) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  setMaxBounds(lng1 = -540, lat1 = -85.051129, lng2 = 540, lat2 = 85.051129)

# ---- shared colors and plot style ----
app_ink <- "#24323d"
app_blue <- "#2f6c8f"
app_teal <- "#1b6261"
app_light <- "#e2eeef"
app_border <- "#d7e3ea"
app_missing <- "#a8b8be"
app_numeric_colors <- c(app_light, "#76adb1", app_teal)

category_colors <- function(count) {
  colors <- c(app_blue, app_teal, "#a96c21", "#796189", "#58763e", "#a65753",
    "#397e9b", "#937245", "#64679b", "#3c8372", "#93657e", "#576b80")
  if (count <= length(colors)) return(head(colors, count))

  # extend the app's accent colors with darker and lighter shades
  shades <- vapply(colors, function(color) {
    dark <- grDevices::colorRampPalette(c(color, app_ink))(4)[2]
    light <- grDevices::colorRampPalette(c(color, app_light))(7)[3:4]
    c(dark, light)
  }, character(3))
  rep_len(c(colors, as.vector(t(shades))), count)
}

app_category_scale <- function() {
  discrete_scale(c("colour", "fill"), palette = category_colors, na.value = app_missing)
}

# round displayed labels while keeping the original values for calculations
whole_number <- function(values) {
  formatC(round(values) + 0, format = "f", digits = 0, big.mark = ",")
}

# show fit as a whole percentage while keeping the fitted line at full precision
whole_fit_label <- function(intercept, slope, fit) {
  paste0("y = ", whole_number(intercept), ifelse(slope < 0, " − ", " + "),
    whole_number(abs(slope)), "x; R² = ", whole_number(100 * fit), "%")
}

app_plot_theme <- function() {
  theme_minimal(base_size = 14, base_family = "Arial") +
    theme(text = element_text(colour = app_ink),
      axis.text = element_text(colour = app_ink),
      axis.line.x.bottom = element_line(colour = app_ink),
      axis.line.y.left = element_line(colour = app_ink),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      legend.key = element_rect(fill = "white", colour = NA))
}

# ---- project links and team ----
project_url <- "https://global-river-chem.github.io"
papers <- data.frame(
  title = c(
    "Decadal Shifts Towards Higher Riverine Silicon Relative to Nitrogen and Phosphorus Across High Latitudes",
    "Thinking Outside the Rocks: Subsurface Water Storage, Topography, and Land Cover Are Key Modulators of Large-Scale Riverine Dissolved Silicon Dynamics",
    "GlASS – Global Aggregation of Stream Silica",
    "Climate, Hydrology, and Nutrients Control the Seasonality of Si Concentrations in Rivers",
    "Establishing Fluvial Silicon Regimes and Their Stability Across the Northern Hemisphere",
    "Long-Term Changes in Concentration and Yield of Riverine Dissolved Silicon From the Poles to the Tropics"
  ),
  journal = c("Global Biogeochemical Cycles · 2026", "Geophysical Research Letters · 2026",
    "Scientific Data · 2025", "Journal of Geophysical Research: Biogeosciences · 2024",
    "Limnology and Oceanography Letters · 2024", "Global Biogeochemical Cycles · 2023"),
  doi = c("10.1029/2025GB008926", "10.1029/2025GL118853", "10.1038/s41597-025-05937-2",
    "10.1029/2024JG008141", "10.1002/lol2.10372", "10.1029/2022GB007678"),
  authors = c(
    paste("Joanna C. Carey, Nicholas J. Lyon, Keira Johnson, Pamela L. Sullivan, Sidney A. Bush,",
      "Lienne R. Sethna, Ruth Heindel, Pirkko Kortelainen, Hjalmar Laudon, Diane M. McKnight,",
      "Amanda Poste, Antti Raike, Kathi Jo Jankowski"),
    paste("Sidney A. Bush, Keira Johnson, Kathi Jo Jankowski, Joanna C. Carey,",
      "Lienne R. Sethna, Nicholas J. Lyon, Pamela L. Sullivan"),
    paste("Kathi Jo Jankowski, Keira Johnson, Nicholas J. Lyon, Sidney A. Bush, Paul Julian,",
      "Lienne R. Sethna, Diane M. McKnight, William H. McDowell, Adam S. Wymore, Pirkko Kortelainen,",
      "Hjalmar Laudon, Ruth C. Heindel, Amanda E. Poste, Arial Shogren, Fred Worral, Luke Mosley,",
      "Pamela L. Sullivan, Joanna C. Carey"),
    paste("Keira Johnson, Kathi Jo Jankowski, Joanna C. Carey, Lienne R. Sethna, Sidney A. Bush,",
      "Diane McKnight, William H. McDowell, Adam S. Wymore, Pirkko Kortelainen, Jeremy B. Jones,",
      "Nicholas J. Lyon, Hjalmar Laudon, Amanda E. Poste, Pamela L. Sullivan"),
    paste("Keira Johnson, Kathi Jo Jankowski, Joanna Carey, Nicholas J. Lyon, William H. McDowell,",
      "Arial Shogren, Adam Wymore, Lienne Sethna, Wilfred M. Wollheim, Amanda E. Poste, Pirkko Kortelainen,",
      "Ruth Heindel, Hjalmar Laudon, Antti Räike, Jeremy B. Jones, Diane McKnight, Paul Julian,",
      "Sidney Bush, Pamela L. Sullivan"),
    paste("Kathi Jo Jankowski, Keira Johnson, Lienne Sethna, Paul Julian, Adam S. Wymore,",
      "Arial J. Shogren, Patrick K. Thomas, Pamela L. Sullivan, Diane M. McKnight, William H. McDowell,",
      "Ruth Heindel, Jeremy B. Jones, Wilfred Wollheim, Benjamin Abbott, Linda Deegan, Joanna C. Carey")
  )
)
team <- data.frame(
  name = c("Joanna C. Carey", "Kathi Jo Jankowski", "Pamela Sullivan", "Sidney A. Bush",
    "Lienne Sethna", "Keira Johnson", "Nick Lyon", "Abra Atwood", "Ally Jacoby"),
  github = c("", "kathijo", "ecohydrolab", "sidneyabush", "lsethna", "hydrokeira",
    "njlyon0", "acatwood", "jaco2212"),
  scholar = c("n_HQZ7QAAAAJ", "3rzL9PEAAAAJ", "1RJWI5HNpNsC", "y-LJGhcAAAAJ",
    "Va7xBOkAAAAJ", "pC1oFD0AAAAJ", "6-y-77oAAAAJ", "y_fcPrAAAAAJ", ""),
  github_photo = c("", "", "", "21317032", "30530947", "73953949", "21199610", "", ""),
  scholar_photo = c("8", "3", "4", "", "", "", "", "", "")
)
team$website <- c(
  "https://www.babson.edu/about/our-leaders-and-scholars/faculty-and-academic-divisions/faculty-profiles/joanna-carey.php",
  "https://www.usgs.gov/staff-profiles/kathi-jo-jankowski",
  "https://sites.google.com/oregonstate.edu/sullivanlab/home", "",
  "https://www.liennesethna.com/", "", "https://njlyon0.github.io/", "", ""
)
team$portrait <- c(
  "https://facultyprofilefiles.babson.edu/MirroredFiles/jcarey/pci/Joanna%20Carey-1.jpg",
  "https://d9-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/s3fs-public/styles/staff_profile/public/thumbnails/image/self_Brazil_nutrients_small.jpg?itok=yCnPnPZJ",
  "pamela-sullivan-lab.jpg",
  "", "", "", "https://njlyon0.github.io/images/profiles/nick_lyon_2023.jpg", "", ""
)
team <- rbind(team, data.frame(
  name = c("Bryn Stewart", "Jessica Corman", "Nicole M. Fernandez", "Tao Wen",
    "Zanchenling Wang", "Arial Shogren", "Marcia Macedo", "Lauren E. Koenig"),
  github = "",
  scholar = c("OlE7W9wAAAAJ", "HJFCnrgAAAAJ", "qArjQWEAAAAJ", "5jsj4tUAAAAJ",
    "Umr0p-sAAAAJ", "tXSqKVEAAAAJ", "ycFBawcAAAAJ", "9QbFeXYAAAAJ"),
  github_photo = "",
  scholar_photo = c("1", "2", "2", "1", "1", "3", "2", "18"),
  website = c("https://sites.google.com/view/brynstewartlab/home", "https://ecostoich.weebly.com",
    "https://eaps.ethz.ch/en/people/profile.nicole-fernandez-franzan.html",
    "https://jaywen.com",
    "https://artsandsciences.syracuse.edu/people/graduate-students/zanchenling-wang/",
    "https://sites.google.com/view/shogrenscience/home",
    "https://people.climate.columbia.edu/users/profile/marcia-macedo",
    "https://www.usgs.gov/staff-profiles/lauren-koenig"),
  portrait = ""
))
team <- rbind(team, data.frame(
  name = "Elizabeth Elmstrom", github = "eelmstrom", scholar = "",
  github_photo = "", scholar_photo = "", website = "", portrait = ""
))
team <- rbind(team, data.frame(
  name = "Daniel Conley", github = "", scholar = "",
  github_photo = "", scholar_photo = "",
  website = "https://portal.research.lu.se/en/persons/daniel-conley/", portrait = ""
))

# match affiliations by name so they follow each person when the team is sorted
affiliations <- c(
  "Joanna C. Carey" = "Babson College",
  "Kathi Jo Jankowski" = "USGS",
  "Pamela Sullivan" = "Oregon State University",
  "Sidney A. Bush" = "Oregon State University",
  "Lienne Sethna" = "St. Croix Watershed Research Station",
  "Keira Johnson" = "University of Colorado Boulder",
  "Nick Lyon" = "University of Wisconsin–Madison",
  "Abra Atwood" = "Woodwell Climate Research Center",
  "Ally Jacoby" = "St. Croix Watershed Research Station",
  "Bryn Stewart" = "University of Arizona",
  "Jessica Corman" = "University of Nebraska–Lincoln",
  "Nicole M. Fernandez" = "ETH Zurich",
  "Tao Wen" = "Syracuse University",
  "Zanchenling Wang" = "Syracuse University",
  "Arial Shogren" = "University of Alabama",
  "Marcia Macedo" = "Woodwell Climate Research Center",
  "Lauren E. Koenig" = "USGS",
  "Elizabeth Elmstrom" = "Marine Biological Laboratory",
  "Daniel Conley" = "Lund University"
)
team$affiliation <- unname(affiliations[team$name])

# keep the requested lead order and sort the remaining team by last name
nick_row <- match("Nick Lyon", team$name)
remaining <- seq.int(nick_row + 1, nrow(team))
last_names <- sub(".* ", "", team$name[remaining])
lead_names <- c("Joanna C. Carey", "Kathi Jo Jankowski", "Pamela Sullivan",
  "Lienne Sethna", "Keira Johnson", "Sidney A. Bush", "Nick Lyon")
team <- team[c(match(lead_names, team$name), remaining[order(tolower(last_names))]), ]

paper_links <- lapply(seq_len(nrow(papers)), function(index) {
  tags$li(
    tags$span(class = "paper-journal", papers$journal[index]),
    tags$a(href = paste0("https://doi.org/", papers$doi[index]), papers$title[index]),
    tags$details(class = "paper-authors",
      tags$summary(paste0(sub(",.*", "", papers$authors[index]), " et al.")),
      tags$p(papers$authors[index]))
  )
})
team_cards <- lapply(seq_len(nrow(team)), function(index) {
  person <- team[index, ]
  scholar_portrait <- if (nzchar(person$scholar_photo)) {
    paste0("https://scholar.googleusercontent.com/citations?view_op=view_photo&user=",
      person$scholar, "&citpid=", person$scholar_photo)
  } else ""
  # use the requested portrait first, then a github or scholar photo
  photo <- person$portrait
  if (!nzchar(photo) && nzchar(person$github_photo)) {
    photo <- paste0("https://avatars.githubusercontent.com/u/", person$github_photo, "?s=160")
  } else if (!nzchar(photo)) {
    photo <- scholar_portrait
  }
  initials <- paste0(substr(strsplit(person$name, " ")[[1]], 1, 1), collapse = "")
  tags$article(class = "team-card",
    tags$div(class = "profile-photo",
      tags$span(toupper(initials), `aria-hidden` = "true"),
      if (nzchar(photo)) tags$img(src = photo, alt = "", loading = "lazy",
        `data-fallback` = if (photo != scholar_portrait) scholar_portrait else "",
        onerror = "if (this.dataset.fallback) { this.src = this.dataset.fallback; this.dataset.fallback = ''; } else { this.hidden = true; }")),
    tags$h3(person$name),
    tags$p(class = "team-affiliation", person$affiliation),
    tags$div(class = "team-links",
      if (nzchar(person$website)) tags$a(href = person$website, "Website"),
      if (nzchar(person$scholar)) tags$a(
        href = paste0("https://scholar.google.com/citations?hl=en&user=", person$scholar),
        "Google Scholar"),
      if (nzchar(person$github)) tags$a(href = paste0("https://github.com/", person$github), "GitHub")
    )
  )
})

# ---- reusable layout for plot controls and output ----
analysis_layout <- function(controls, content) {
  bslib::layout_sidebar(
    sidebar = controls,
    bslib::card(content, full_screen = TRUE, fill = FALSE, class = "output-card"),
    fill = FALSE, fillable = FALSE, class = "analysis-layout"
  )
}
analysis_controls <- function(...) {
  bslib::sidebar(..., title = "Plot controls", width = 280, open = list(desktop = "open", mobile = "closed"))
}

# clip each ivory drawing to the outline traced from the sticker artwork
diatom_drawing <- function(crop, position, outline) {
  clip_id <- paste0(position, "-outline")
  tags$svg(class = paste("hero-diatom", position), viewBox = crop,
    `aria-hidden` = "true", focusable = "false",
    tags$defs(tags$clipPath(id = clip_id, tags$path(d = outline))),
    tags$image(href = "diatom-drawings.png", width = 1600, height = 1549,
      `clip-path` = paste0("url(#", clip_id, ")")))
}

# use separate logo assets where available and frame the rest from the workshop slide
partner_logo <- function(name, file = NULL, crop = NULL) {
  mark <- if (!is.null(file)) {
    tags$img(src = paste0("partner-logos/", file), alt = "", loading = "lazy")
  } else {
    bounds <- as.numeric(strsplit(crop, " ", fixed = TRUE)[[1]])
    clip_id <- paste0("partner-", gsub("[^a-z]+", "-", tolower(name)))
    outline <- tags$rect(x = bounds[1], y = bounds[2], width = bounds[3], height = bounds[4])
    # the lter mark overlaps the upper right of the critical zone crop
    if (name == "Critical Zone Network") {
      outline <- tags$path(d = "M43 850 H390 V870 H678 V1080 H43 Z")
    }
    tags$svg(viewBox = crop, `aria-hidden` = "true", focusable = "false",
      tags$defs(tags$clipPath(id = clip_id, outline)),
      tags$image(href = "Slide1.jpeg", width = 720, height = 1087,
        `clip-path` = paste0("url(#", clip_id, ")")))
  }
  tags$div(class = "partner-tile", tags$div(class = "partner-mark", mark), tags$span(name))
}

# ---- prepare directory cells once for all sessions ----
delayedAssign("directory_rows", {
  directory <- all_drivers_table
  # place the labeled sources next to the site name
  links <- lapply(source_columns, function(column) {
    values <- directory[[column]]
    sources <- unique(values)
    formatted <- vapply(sources, function(value) {
      source_entry(value, source_labels[[column]])
    }, character(1))
    formatted[match(values, sources)]
  })
  sites <- directory[c("Observation Network", "Country", "Stream Name")]
  sites[["Solute Coverage"]] <- vapply(sites[["Stream Name"]], function(site) {
    coverage <- site_chemistry[[site]]
    if (!length(coverage$variable)) return("No chemistry records")
    rows <- paste0("<tr><td>", htmltools::htmlEscape(solute_notation(coverage$variable)),
      "</td><td>", coverage$first_year, "</td><td>", coverage$last_year,
      "</td><td>", format(coverage$observations, big.mark = ",", trim = TRUE), "</td></tr>")
    paste0("<details><summary>", length(coverage$variable), " of ", length(solute_choices),
      " solutes</summary><table class='solute-coverage-detail small mt-2'>",
      "<thead><tr><th scope='col'>Solute</th><th scope='col'>First year</th>",
      "<th scope='col'>Last year</th><th scope='col'>Observations</th></tr></thead>",
      "<tbody>", paste(rows, collapse = ""), "</tbody></table></details>")
  }, character(1))
  pairs <- as.integer(paired_counts[match(sites[["Stream Name"]], names(paired_counts))])
  pairs[is.na(pairs)] <- 0L
  sites[["Has Discharge"]] <- directory[["Has Discharge"]]
  sites[["Flagged for WRTDS"]] <- directory[["WRTDS"]]
  sites[["C–Q Pairs"]] <- pairs
  sites[["Source Links"]] <- vapply(seq_len(nrow(sites)), function(index) {
    entries <- vapply(links, function(column) column[index], character(1))
    entries <- entries[nzchar(entries)]
    if (!length(entries)) return("No source links listed")
    as.character(tags$details(tags$summary("View sources"),
      tags$div(class = "small mt-2", HTML(paste(entries, collapse = "<br>")))))
  }, character(1))
  sites[["Latitude (°)"]] <- directory$Latitude
  sites[["Longitude (°)"]] <- directory$Longitude
  # sort the expandable coverage cells by the underlying count
  sites[["Solute Count"]] <- vapply(sites[["Stream Name"]], function(site) {
    length(site_chemistry[[site]]$variable)
  }, integer(1))
  sites
}, eval.env = environment(), assign.env = environment())
