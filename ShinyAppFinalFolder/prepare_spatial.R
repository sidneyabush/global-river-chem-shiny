# ---- summarize watershed characteristics for the app ----
summarize_spatial <- function(spatial, metadata, npp_to_grams) {
  stopifnot(!anyDuplicated(spatial$Stream_Name))
  site_key <- function(rows) paste(rows$LTER, rows$Stream_Name, sep = "\r")
  stopifnot(!anyDuplicated(site_key(metadata)))
  source_row <- match(site_key(spatial), site_key(metadata))
  stopifnot(!anyNA(source_row))

  # fill missing coordinates only for exact matches in the original station records
  verified_coordinates <- data.frame(
    LTER = c("ARC", "ARC", "BNZ"),
    Stream_Name = c("Atigun River", "Roche Moutonnee", "C1"),
    Latitude = c(68.2666666666667, 68.3833333333333, 65.154086),
    Longitude = c(-149.45, -149.316666666667, -147.647846),
    Source = c(rep("https://doi.org/10.6073/pasta/574fd24522eee7a0c07fc260ccc0e2fa", 2),
      "https://doi.org/10.6073/pasta/da46bc1c76d59e836b8410feb28296e2")
  )
  coordinate_row <- match(site_key(spatial), site_key(verified_coordinates))
  needs_coordinates <- !is.finite(spatial$Latitude) | !is.finite(spatial$Longitude)
  recover <- which(needs_coordinates & !is.na(coordinate_row))
  spatial$Latitude[recover] <- verified_coordinates$Latitude[coordinate_row[recover]]
  spatial$Longitude[recover] <- verified_coordinates$Longitude[coordinate_row[recover]]

  country <- trimws(metadata$Country[source_row])
  country[country == "USA"] <- "United States"

  # keep the source window and the exact years behind each site average
  year_coverage <- list()
  average_years <- function(label, pattern, period = NULL, convert = identity) {
    columns <- grep(pattern, names(spatial), value = TRUE)
    stopifnot(length(columns) > 0)
    years <- as.integer(sub(".*_([0-9]{4})$", "\\1", columns))
    stopifnot(!anyDuplicated(years), !anyNA(years))
    values <- as.matrix(as.data.frame(lapply(spatial[columns], convert)))
    available <- is.finite(values)
    if (is.null(period)) {
      # use only years with values in the supplied release
      reported_years <- years[colSums(available) > 0L]
      period <- if (length(reported_years)) paste(unique(range(reported_years)), collapse = "–") else "No data"
    }
    values[!available] <- NA_real_
    means <- rowMeans(values, na.rm = TRUE)
    means[is.nan(means)] <- NA_real_
    year_coverage[[label]] <<- list(period = period, years = lapply(seq_len(nrow(values)), function(row) {
      sort(years[available[row, ]])
    }))
    means
  }

  # convert each annual greenup date to day of year before averaging
  greenup_day <- function(values) as.integer(format(as.Date(values, format = "%Y-%m-%d"), "%j"))

  annual_drivers <- list(
    "Annual Maximum Snow Cover (fraction)" = average_years("Annual Maximum Snow Cover (fraction)", "^snow_cover_[0-9]{4}$", "2000–2025"),
    "Precipitation (mm/year)" = average_years("Precipitation (mm/year)", "^precip_[0-9]{4}$"),
    "Evapotranspiration (mm/year)" = average_years("Evapotranspiration (mm/year)", "^et_[0-9]{4}$"),
    "Potential Evaporation (mm/year)" = average_years("Potential Evaporation (mm/year)", "^pet_[0-9]{4}$"),
    "Peak Snow Water Equivalent (mm)" = average_years("Peak Snow Water Equivalent (mm)", "^snow_swe_[0-9]{4}$", "2000–2025"),
    "Mean Annual Temperature (deg C)" = average_years("Mean Annual Temperature (deg C)", "^temp_[0-9]{4}$"),
    "NPP (g C/m2/year)" = average_years("NPP (g C/m2/year)", "^npp_[0-9]{4}$", "2001–2025") * npp_to_grams,
    "GPP (g C/m2/year)" = average_years("GPP (g C/m2/year)", "^gpp_[0-9]{4}$", "1982–2022"),
    "First Greenup (day of year)" = average_years("First Greenup (day of year)", "^greenup_c0_[0-9]{4}$", "2001–2024", greenup_day),
    "Population (people)" = average_years("Population (people)", "^pop_[0-9]{4}$", "2000–2024"),
    "Population Density (people/km2)" = average_years("Population Density (people/km2)", "^pop_density_[0-9]{4}$", "2000–2024"),
    "Population Coverage (fraction)" = average_years("Population Coverage (fraction)", "^pop_area_[0-9]{4}$", "2000–2024")
  )

  # average land cover shares over the full release window and leave out fill pixels
  land_years <- 1985:2022
  land_period <- paste(range(land_years), collapse = "–")
  cover <- grep(paste0("^lc_.*_(", paste(land_years, collapse = "|"), ")$"),
    names(spatial), value = TRUE)
  cover_classes <- setdiff(unique(sub("_[0-9]{4}$", "", cover)), "lc_fill")
  stopifnot(length(cover_classes) > 0)
  for (cover_class in cover_classes) {
    label <- gsub("_", " ", sub("^lc_", "", cover_class), fixed = TRUE)
    label <- paste0("Land Cover: ", label, " (", land_period, " mean, %)")
    pattern <- paste0("^", cover_class, "_(", paste(land_years, collapse = "|"), ")$")
    annual_drivers[[label]] <- 100 * average_years(label, pattern, land_period)
  }
  land_shares <- as.matrix(as.data.frame(annual_drivers[startsWith(names(annual_drivers), "Land Cover: ")],
    check.names = FALSE))
  colnames(land_shares) <- gsub("^Land Cover: | \\(.*$", "", colnames(land_shares))

  # use the study's largest-share rule for the watershed average
  land <- land_shares
  land[is.na(land)] <- -Inf
  dominant <- colnames(land)[max.col(land, ties.method = "first")]
  dominant[rowSums(land > 0) == 0] <- NA_character_

  # the dominant class uses the years contributing to any compared class
  land_coverage <- year_coverage[startsWith(names(year_coverage), "Land Cover: ")]
  year_coverage[["Dominant Land Cover"]] <- list(period = land_period,
    years = lapply(seq_len(nrow(spatial)), function(row) {
      sort(unique(unlist(lapply(land_coverage, function(cover_class) cover_class$years[[row]]))))
    }))

  # match the study's rock groups even when tied rock types are listed in a different order
  rock_key <- function(labels) {
    vapply(strsplit(labels, ";", fixed = TRUE), function(types) {
      paste(sort(trimws(types)), collapse = "; ")
    }, character(1))
  }
  study_rocks <- c(
    "volcanic" = "Volcanic", "volcanic; plutonic" = "Volcanic",
    "sedimentary" = "Sedimentary", "sedimentary; metamorphic" = "Sedimentary",
    "sedimentary; carbonate_evaporite" = "Sedimentary",
    "volcanic; sedimentary; carbonate_evaporite" = "Sedimentary",
    "sedimentary; plutonic; carbonate_evaporite; metamorphic" = "Sedimentary",
    "plutonic" = "Plutonic", "plutonic; metamorphic" = "Plutonic",
    "volcanic; plutonic; metamorphic" = "Plutonic",
    "metamorphic" = "Metamorphic", "carbonate_evaporite; metamorphic" = "Metamorphic",
    "carbonate_evaporite" = "Carbonate Evaporite",
    "volcanic; carbonate_evaporite" = "Carbonate Evaporite"
  )
  rock_group <- unname(study_rocks[match(rock_key(spatial$major_rock), rock_key(names(study_rocks)))])
  mixed <- which(rock_group == "Sedimentary" & spatial$rock_sedimentary < 70)
  rock_group[mixed] <- "Mixed Sedimentary"

  # some saved permafrost means use percent while the bounds use fractions
  permafrost <- spatial$permafrost_mean
  tolerance <- 1e-10
  minimum <- spatial$permafrost_min - tolerance
  maximum <- spatial$permafrost_max + tolerance
  as_fraction <- permafrost / 100
  is_percent <- which((permafrost < minimum | permafrost > maximum) &
    as_fraction >= minimum & as_fraction <= maximum)
  permafrost[is_percent] <- as_fraction[is_percent]

  drivers <- data.frame(
    "Stream Name" = spatial$Stream_Name,
    Country = factor(country),
    "Observation Network" = factor(spatial$LTER),
    Latitude = spatial$Latitude,
    Longitude = spatial$Longitude,
    "Dominant Lithology" = factor(spatial$major_rock),
    "GRL Lithology Group" = factor(rock_group),
    "Dominant Land Cover" = factor(dominant),
    "Mean Elevation (m)" = spatial$elev_mean,
    "Drainage Area (km2)" = spatial$drainSqKm,
    "Dam Count" = spatial$dam_count,
    "Dam Density (count/1,000 km2)" = spatial$dam_density,
    "Dam Storage (million m3)" = spatial$dam_storage,
    "Dam Hydropower (MW)" = spatial$dam_power,
    "Nitrogen Fertilizer Index (kg/ha)" = spatial$fert_n_index,
    "Phosphorus Fertilizer Index (kg P2O5/ha)" = spatial$fert_p2o5_index,
    "Potassium Fertilizer Index (kg K2O/ha)" = spatial$fert_k2o_index,
    "Wastewater Plant Count" = spatial$ww_plants,
    "Wastewater Plant Density (count/1,000 km2)" = spatial$ww_density,
    "Population Served by Wastewater Plants" = spatial$ww_people,
    "Wastewater Discharge (m3/day)" = spatial$ww_discharge,
    "Primary Wastewater Treatment Plants" = spatial$ww_primary,
    "Secondary Wastewater Treatment Plants" = spatial$ww_secondary,
    "Advanced Wastewater Treatment Plants" = spatial$ww_advanced,
    "Distance to Outlet (km)" = spatial$outlet_dist_km,
    "Distance to Ocean (km)" = spatial$ocean_dist_km,
    "Mean Basin Slope (degrees)" = spatial$slope_mean,
    "Mean Permafrost Probability (fraction)" = permafrost,
    "Chemistry Data" = metadata$Chemistry_Source[source_row],
    "Discharge Data" = metadata$Discharge_Source[source_row],
    "Has Discharge" = ifelse(!is.na(metadata$Discharge_File_Name[source_row]) &
      nzchar(trimws(metadata$Discharge_File_Name[source_row])), "Yes", "No"),
    "WRTDS" = metadata$Use_WRTDS[source_row],
    "Watershed Boundary" = metadata$Watershed_Boundary_Source[source_row],
    "Drainage Area Source" = metadata$Drainage_Area_Source[source_row],
    check.names = FALSE
  )

  # attach annual predictors and their coverage to the site data
  drivers[names(annual_drivers)] <- annual_drivers

  # expose the underlying rock shares alongside the dominant class
  for (column in grep("^(rock)_", names(spatial), value = TRUE)) {
    label <- sub("^(rock)_", "\\1: ", column)
    label <- gsub("_", " ", label, fixed = TRUE)
    drivers[[paste0(label, " (%)")]] <- spatial[[column]]
  }
  site_order <- order(tolower(drivers[["Stream Name"]]))
  drivers <- drivers[site_order, ]
  attr(drivers, "year_coverage") <- lapply(year_coverage, function(coverage) {
    coverage$years <- setNames(coverage$years[site_order], drivers[["Stream Name"]])
    coverage
  })
  # retain the annual npp quality fields without adding them to the plot choices
  npp_gapfill <- grep("^npp_gapfill_[0-9]{4}$", names(spatial), value = TRUE)
  if (length(npp_gapfill)) {
    attr(drivers, "npp_gapfill") <- spatial[site_order, c("LTER", "Stream_Name", npp_gapfill)]
  }
  attr(drivers, "recovered_coordinate_sources") <- verified_coordinates

  drivers
}

# join the updated product tables by network and site name
summarize_updated_spatial <- function(release) {
  site_key <- function(d) paste(d$LTER, d$Stream_Name, sep = "\r")
  metadata <- release[[grep("^site-metadata/", names(release), value = TRUE)]]
  spatial <- metadata[c("LTER", "Stream_Name", "Latitude", "Longitude", "drainSqKm")]
  for (name in names(release)[!grepl("^site-metadata/", names(release))]) {
    product <- release[[name]]
    rows <- match(site_key(spatial), site_key(product))
    stopifnot(!anyDuplicated(site_key(product)), !anyNA(rows))
    columns <- setdiff(names(product), c(names(spatial), "Site_Flag"))
    spatial[columns] <- product[rows, columns, drop = FALSE]
  }

  # translate release field names to the existing summary inputs
  fields <- c(
    elev_mean = "elevation_mean_m", slope_mean = "basin_slope_mean_degree",
    permafrost_mean = "permafrost_mean_m", permafrost_min = "permafrost_min_m",
    permafrost_max = "permafrost_max_m", outlet_dist_km = "downstream_to_outlet_km",
    ocean_dist_km = "downstream_to_ocean_km", dam_count = "human_dam_barrier_count",
    dam_density = "human_dam_barrier_density_per_1000_km2",
    dam_storage = "human_dam_storage_capacity_mcm_total",
    dam_power = "human_dam_hydropower_capacity_mw_total",
    fert_n_index = "human_fertilizer_n_crop_rate_sum_index_mean_kg_ha",
    fert_p2o5_index = "human_fertilizer_p2o5_crop_rate_sum_index_mean_kg_ha",
    fert_k2o_index = "human_fertilizer_k2o_crop_rate_sum_index_mean_kg_ha",
    ww_plants = "human_wastewater_plant_count", ww_density = "human_wastewater_plant_density_per_1000_km2",
    ww_people = "human_wastewater_population_served_people_total",
    ww_discharge = "human_wastewater_discharge_m3_day_total",
    ww_primary = "human_wastewater_primary_treatment_count",
    ww_secondary = "human_wastewater_secondary_treatment_count",
    ww_advanced = "human_wastewater_advanced_treatment_count"
  )
  spatial[names(fields)] <- spatial[unname(fields)]
  patterns <- c(
    "^era5_land_precip_mm_" = "precip_", "^era5_land_temp_degC_" = "temp_",
    "^era5_land_evapotrans_mm_" = "et_", "^era5_land_potential_evap_mm_" = "pet_",
    "^era5_land_snow_cover_fraction_" = "snow_cover_",
    "^era5_land_snow_water_equiv_mm_" = "snow_swe_",
    "^bess_gpp_([0-9]{4})_gC_m2_year$" = "gpp_\\1",
    "^npp_([0-9]{4})_kgC_m2_year$" = "npp_\\1",
    "^npp_([0-9]{4})_gapfilled_input_days_pct$" = "npp_gapfill_\\1",
    "^greenup_cycle0_([0-9]{4})MMDD$" = "greenup_c0_\\1",
    "^human_population_total_people_" = "pop_",
    "^human_population_density_people_km2_" = "pop_density_",
    "^human_population_data_coverage_fraction_" = "pop_area_", "^rocks_" = "rock_"
  )
  for (pattern in names(patterns)) {
    names(spatial) <- sub(pattern, patterns[[pattern]], names(spatial))
  }
  cover <- c(bare = "Bare", crop = "Cropland", forest = "Forest",
    grass_shrub = "Grassland_Shrubland", ice_snow = "Ice_Snow", impervious = "Impervious",
    open_water = "Open_water", salt_water = "Salt_Water", tidal_wetland = "Tidal_Wetland",
    wetland_marsh = "Wetland_Marsh")
  for (class in names(cover)) {
    pattern <- paste0("^gee_glc_([0-9]{4})_", cover[[class]], "$")
    names(spatial) <- sub(pattern, paste0("lc_", class, "_\\1"), names(spatial))
  }
  summarize_spatial(spatial, metadata, npp_to_grams = 1000)
}

# run this script from the app folder with the updated spatial release rds
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Provide the updated spatial release RDS.")

  release <- readRDS(args[1])
  drivers <- summarize_updated_spatial(release)

  # keep both greenup cycles and their coverage flags for the six amazon sites
  greenup_key <- grep("^modis-greenup/", names(release), value = TRUE)
  stopifnot(length(greenup_key) == 1L)
  greenup <- release[[greenup_key]]
  sites <- c("Anori", "Jutica", "Paura", "Sao Jose do Amatari", "Tupe", "Xibeco")
  camrex <- greenup[greenup$LTER == "Amazon" & greenup$Stream_Name %in% sites, ]
  stopifnot(nrow(camrex) == length(sites), setequal(camrex$Stream_Name, sites))
  attr(drivers, "camrex_greenup") <- camrex
  saveRDS(drivers, "Spatial_20261102.rds")
}
