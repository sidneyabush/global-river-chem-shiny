# ---- server ----
# update maps, tables, and plots when users change selections
overview_server <- function(input, output, session) {
  # ---- data availability panel server ----

  # plot of number of sites with data for each solute over time
  output$plot1 <- renderPlot({

    solutes <- levels(master_chem_summary$variable)
    counts <- solute_observation_totals$total[match(solutes, solute_observation_totals$variable)]
    ggplot(master_chem_summary, aes(x = year, y = as.integer(variable), group = variable, col = num_streams)) +
      geom_line(linewidth = 8) +
      scale_y_continuous(breaks = seq_along(solutes), labels = solute_notation(solutes),
        expand = expansion(add = 0.6),
        sec.axis = dup_axis(name = "Observations", labels = format(counts, big.mark = ",", trim = TRUE))) +
      app_plot_theme() + labs(y = "decreasing # of observations", x = "Year", col = "Number of Sites") +
      theme(
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15),
        axis.title.y.left = element_text(angle = 90, margin = margin(r = 8)),
        axis.title.y.right = element_text(angle = 270, margin = margin(l = 8)),
        legend.title = element_text(size = 14), legend.text = element_text(size = 12),
        legend.position = "bottom",
        axis.line.x.bottom = element_line(colour = app_ink),
        axis.line.y.left = element_line(colour = app_ink,
          arrow = grid::arrow(ends = "first", length = grid::unit(2.5, "mm")))) +
      scale_x_continuous(labels = whole_number) +
      scale_color_gradientn(colours = app_numeric_colors, labels = whole_number,
        limits = c(0, max(master_chem_summary$num_streams)))

  }, width = function() {
    width <- input$solute_plot_width
    if (is.null(width)) width <- 480
    max(300, width)
  }, height = 420)

  # drivers table information
  output$table1 <- DT::renderDT({
    sources <- data_source_table
    # list the new products first and sort both groups by product name
    new_products <- c("BESSv2", "ERA5-Land", "LandScan Global", "Global Dam Watch v1.0",
      "NPKGRIDS", "HydroWASTE v1.0", "HydroRIVERS routing")
    is_new <- sources[["Data product"]] %in% new_products
    sources <- sources[order(!is_new, tolower(sources[["Data product"]])), ]
    highlight <- DT::JS(sprintf(
      "function(row, data, index) { if (index < %d) row.classList.add('new-product'); }",
      sum(is_new)))
    # expand explanations inside the matching variable cells
    human_impact_notes <- list(
      "LandScan Global" = "LandScan estimates how many people are typically present across a 24-hour day. Coverage is the percentage of the watershed with valid population estimates.",
      "Global Dam Watch v1.0" = tags$dl(class = "mb-0",
        tags$dt("Dam Count"),
        tags$dd("Number of mapped dams and other river barriers in the watershed."),
        tags$dt("Dam Density [count (1,000 km²)⁻¹]"),
        tags$dd("Number of mapped barriers per 1,000 km² of watershed area."),
        tags$dt("Dam Storage (million m³)"),
        tags$dd("Total reported reservoir storage capacity in the watershed."),
        tags$dt("Dam Hydropower (MW)"),
        tags$dd("Total reported hydropower generating capacity in the watershed.")
      ),
      "NPKGRIDS" = "The nitrogen, phosphate, and potash indices summarize crop-specific application rates and describe application intensity. They do not measure the total fertilizer applied or the amount reaching streams.",
      "HydroWASTE v1.0" = tagList(
        tags$p("Counts refer to mapped plants. Discharge is their effluent volume per day, and population served estimates how many people use the facilities."),
        tags$ul(
          tags$li(tags$strong("Primary"), " uses settling and skimming to remove solids and grease."),
          tags$li(tags$strong("Secondary"), " adds biological treatment to break down organic matter."),
          tags$li(tags$strong("Advanced"), " adds treatment such as extra filtration or targeted nitrogen and phosphorus removal.")
        )
      )
    )
    sources$Variable <- vapply(seq_len(nrow(sources)), function(row) {
      label <- sources$Variable[row]
      note <- human_impact_notes[[sources[["Data product"]][row]]]
      if (is.null(note)) return(htmltools::htmlEscape(label))
      as.character(tags$details(tags$summary(label), tags$div(class = "small mt-2", note)))
    }, character(1))
    sources$`Data product` <- vapply(sources$`Data product`, function(label) {
      source_link(product_urls[[label]], label)
    }, character(1))
    DT::datatable(sources, rownames = FALSE, escape = setdiff(names(sources), c("Data product", "Variable")),
      options = list(searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE,
        order = list(), createdRow = highlight))
  }, server = FALSE)

  # ---- site location panel server ----

  # share the selected network between its summary and site list
  network_sites <- reactive({
    sites <- all_drivers_table
    network <- input$site_network
    if (is.null(network) || !nzchar(network)) return(sites)
    sites[which(sites[["Observation Network"]] == network), , drop = FALSE]
  })

  directory_sites <- reactive({
    sites <- network_sites()
    if (isTruthy(input$site_country)) {
      sites <- sites[which(sites$Country == input$site_country), , drop = FALSE]
    }
    if (isTruthy(input$site_discharge)) {
      sites <- sites[which(sites[["Has Discharge"]] == input$site_discharge), , drop = FALSE]
    }
    if (isTruthy(input$site_wrtds)) {
      sites <- sites[which(sites[["WRTDS"]] == input$site_wrtds), , drop = FALSE]
    }
    selected <- intersect(input$site_solutes, unname(solute_choices))
    if (!length(selected)) return(sites)
    keep <- vapply(sites[["Stream Name"]], function(site) {
      all(selected %in% site_chemistry[[site]]$variable)
    }, logical(1))
    sites[keep, , drop = FALSE]
  })

  output$site_network_summary <- renderUI({
    req(input$site_network)
    sites <- network_sites()
    counts <- table(unlist(lapply(site_chemistry[sites[["Stream Name"]]], function(site) site$variable)))
    links <- vapply(source_columns, function(column) {
      source_entry(sites[[column]], source_labels[[column]])
    }, character(1))
    links <- links[nzchar(links)]
    tags$details(class = "mb-3",
      tags$summary(paste("Network coverage and sources ·", nrow(sites), "sites")),
      tags$p(class = "mt-2 mb-1", "Sites with records by solute"),
      if (length(counts)) tags$ul(lapply(names(counts), function(solute) {
        tags$li(paste0(solute_notation(solute), " · ", counts[[solute]], " sites"))
      })) else tags$p("No chemistry records"),
      if (length(links)) HTML(paste(links, collapse = "<br>")) else tags$p("No source links listed")
    )
  })

  map_color <- reactive({

    req(input$map_option)
    exploration[[as.character(input$map_option)]]

  })

  has_coordinates <- is.finite(all_drivers_map$Latitude) & is.finite(all_drivers_map$Longitude)
  mapped_rows <- reactive({
    values <- map_color()
    available <- if (is.numeric(values)) is.finite(values) else !is.na(values) & nzchar(as.character(values))
    has_coordinates & available
  })

  # use the distribution of mapped values to set the gradient stops
  map_gradient <- function(field, values) {
    if (!length(values)) return(list(keys = numeric(), scale = NA_real_))
    use_log <- grepl("^(Mean Elevation|Mean Permafrost Probability|Drainage Area|Peak Snow Water Equivalent|Population|Dam |Wastewater|Primary Wastewater|Secondary Wastewater|Advanced Wastewater|Distance to|Mean Basin Slope)", field) &&
      field != "Population Coverage (%)"
    if (!use_log) {
      return(list(keys = unique(seq(min(values), max(values), length.out = 5)), scale = NA_real_))
    }
    nonzero <- values[values != 0]
    if (!length(nonzero)) return(list(keys = 0, scale = NA_real_))
    middle <- quantile(nonzero, c(0.1, 0.5, 0.9, 0.975), names = FALSE)
    keys <- sort(unique(c(range(values), middle)))
    list(keys = keys, scale = unname(quantile(abs(nonzero), 0.25)))
  }

  # spread map categories across hues and brightness levels
  map_palette <- function(values, gradient) {
    if (is.factor(values)) {
      count <- nlevels(values)
      hues <- seq(220, 580, length.out = count + 1)[seq_len(count)]
      colors <- grDevices::hcl(h = hues, c = 72,
        l = rep_len(c(43, 63, 53), count))
      colorFactor(colors, domain = values, na.color = app_missing)
    } else {
      keys <- gradient$keys
      if (is.na(gradient$scale) || length(keys) < 2) {
        return(colorNumeric(app_numeric_colors, domain = keys, na.color = app_missing))
      }
      colors <- colorNumeric(c(app_numeric_colors, "#112b47"), domain = c(0, 1), na.color = app_missing)
      stops <- asinh(keys / gradient$scale)
      function(x) {
        position <- approx(stops, seq(0, 1, length.out = length(stops)),
          xout = asinh(x / gradient$scale), rule = 2)$y
        colors(position)
      }
    }
  }

  # use the same colors for the legend and the plotted sites
  map_legend <- function(field, values, gradient, palette) {
    title <- switch(field,
      "Evapotranspiration (mm yr⁻¹)" = "ET (mm yr⁻¹)",
      "Potential Evaporation (mm yr⁻¹)" = "PET (mm yr⁻¹)",
      "Peak Snow Water Equivalent (mm)" = "Peak Snow Water Equivalent (mm)",
      "Precipitation (mm yr⁻¹)" = "MAP (mm yr⁻¹)",
      "Mean Annual Temperature (°C)" = "MAT (°C)",
      "NPP (g C m⁻² yr⁻¹)" = "NPP (g C m⁻² yr⁻¹)",
      "GPP (g C m⁻² yr⁻¹)" = "GPP (g C m⁻² yr⁻¹)",
      field)
    if (field %in% unname(solute_choices)) title <- paste0(solute_notation(field), " (", solute_units(field), ")")
    if (is.factor(values)) {
      keys <- levels(droplevels(values))
      labels <- keys
    } else {
      keys <- gradient$keys
      labels <- whole_number(keys)
      if (endsWith(field, "%)") && length(labels)) labels <- paste0(labels, "%")
    }
    colors <- if (length(keys)) palette(keys) else character()
    if (is.numeric(values) && length(keys) > 1) {
      return(tagList(
        tags$h3(class = "h6 fw-bold", title),
        tags$div(class = "map-legend-gradient",
          tags$div(class = "map-legend-ramp", `aria-hidden` = "true",
            style = paste0("background: linear-gradient(to bottom, ", paste(colors, collapse = ", "), ");")),
          tags$div(class = "map-legend-values", lapply(labels, tags$span)))))
    }
    tagList(
      tags$h3(class = "h6 fw-bold", title),
      tags$ul(lapply(seq_along(labels), function(index) {
        tags$li(tags$span(class = "map-legend-swatch", `aria-hidden` = "true",
          style = paste0("background-color: ", colors[index])), labels[index])
      }))
    )
  }

  # keep the current map view when the selected variable changes
  output$availability_map <- renderLeaflet({
    located <- mapped_rows()
    sites <- exploration[located, ]
    variable <- as.character(input$map_option)
    values <- map_color()
    popup <- paste(paste0("Stream Name: ", htmltools::htmlEscape(sites[["Stream Name"]])),
      paste0("Country: ", htmltools::htmlEscape(as.character(sites$Country))),
      paste0("Observation Network: ", htmltools::htmlEscape(as.character(sites[["Observation Network"]]))), sep = "<br>")
    selected <- values[located]
    gradient <- if (is.numeric(selected)) map_gradient(variable, selected)
    palette <- map_palette(values, gradient)
    selected_text <- if (is.numeric(selected)) ifelse(is.na(selected), "No data", whole_number(selected)) else as.character(selected)
    if (!variable %in% c("Country", "Observation Network")) {
      label <- if (variable %in% unname(solute_choices)) spatial_label(variable) else variable
      popup <- paste0(popup, "<br>", htmltools::htmlEscape(label), ": ",
        htmltools::htmlEscape(selected_text))
    }
    coverage <- spatial_years[[variable]]
    if (!is.null(coverage)) {
      years <- vapply(coverage$years[sites[["Stream Name"]]], format_site_years, character(1))
      popup <- paste0(popup, "<br>Source period: ", coverage$period, "<br>Years used: ", years)
    }
    map <- site_map
    if (nrow(sites)) {
      map <- map %>%
        addCircleMarkers(data = sites, lat = ~Latitude, lng = ~Longitude,
          popup = popup,
          label = ~`Stream Name`, radius = 4, weight = 0.7,
          color = app_ink, opacity = 0.45, fillOpacity = 0.4,
          fillColor = palette(selected))
    }
    bounds <- isolate(input$availability_map_bounds)
    if (is.null(bounds)) {
      longitude <- range(all_drivers_map$Longitude[has_coordinates])
      latitude <- range(all_drivers_map$Latitude[has_coordinates])
      map <- fitBounds(map, longitude[1], latitude[1], longitude[2], latitude[2],
        options = list(padding = c(10, 10)))
    } else {
      map <- fitBounds(map, bounds$west, bounds$south, bounds$east, bounds$north)
    }
    map %>%
      htmlwidgets::onRender("
        function(el, x, data) {
          const legend = el.parentElement.querySelector('#availability_legend');
          if (legend) legend.innerHTML = data.legend;
          const map = this.getMap();
          function keepTilesInView() {
            const height = el.clientHeight;
            if (height <= 0) return;
            // leave room for pixel rounding at the top and bottom of the tiles
            map.setMinZoom(Math.max(0, Math.log2((height + 4) / 256)));
            map.panInsideBounds(map.options.maxBounds, {animate: false});
          }
          keepTilesInView();
          if (el.tileSizeObserver) el.tileSizeObserver.disconnect();
          el.tileSizeObserver = new ResizeObserver(keepTilesInView);
          el.tileSizeObserver.observe(el);
        }
      ", data = list(legend = as.character(map_legend(variable, selected, gradient, palette))))
  })

  # filter the prepared rows without rebuilding the expandable cells
  output$table2 <- DT::renderDT({
    rows <- match(directory_sites()[["Stream Name"]], directory_rows[["Stream Name"]])
    sites <- directory_rows[rows, , drop = FALSE]
    coverage_column <- match("Solute Coverage", names(sites)) - 1L
    count_column <- match("Solute Count", names(sites)) - 1L
    DT::datatable(sites, rownames = FALSE, escape = setdiff(names(sites), c("Solute Coverage", "Source Links")),
      options = list(scrollX = TRUE, pageLength = 50, order = list(list(0, "asc"), list(2, "asc")),
        columnDefs = list(list(targets = coverage_column, orderData = count_column),
          list(targets = count_column, visible = FALSE, searchable = FALSE)))) |>
      DT::formatRound(c("Latitude (°)", "Longitude (°)"), digits = 0)

  }
  )

  # ---- data exploration panel server ----
  # identify controls by field so values stay with the correct filter
  filter_id <- function(field) paste0("exploration_filter_", match(field, names(exploration)))
  selected_filters <- reactive({
    head(intersect(input$exploration_filters, exploration_filter_labels), 3)
  })

  output$exploration_filter_controls <- renderUI({
    controls <- lapply(selected_filters(), function(field) {
      id <- filter_id(field)
      label <- names(exploration_filter_labels)[match(field, exploration_filter_labels)]
      values <- exploration[[field]]
      current <- isolate(input[[id]])
      valid <- values[is.finite(values)]
      if (!length(valid)) return(tags$p(label, " · No data"))
      limits <- range(valid)
      if (limits[1] == limits[2]) return(tags$p(label, " · ", whole_number(limits[1]), " at every site with data"))
      # round outward so the default range includes every measured value
      limits <- c(floor(limits[1]), ceiling(limits[2]))
      if (is.null(current)) current <- limits
      sliderInput(id, tags$strong(paste0(label, ":")), min = limits[1], max = limits[2],
        value = current, step = 1, ticks = FALSE, width = "100%")
    })
    tags$div(style = "max-width: 42rem;", controls)
  })

  filtered_exploration <- reactive({
    sites <- exploration
    for (field in selected_filters()) {
      values <- sites[[field]]
      selected <- input[[filter_id(field)]]
      keep <- is.finite(values)
      if (length(selected) == 2) keep <- keep & values >= selected[1] & values <= selected[2]
      sites <- sites[keep, , drop = FALSE]
    }
    sites
  })

  # show the matching sites with known coordinates
  exploration_map_sites <- reactive({
    sites <- filtered_exploration()
    sites[is.finite(sites$Latitude) & is.finite(sites$Longitude), , drop = FALSE]
  })

  # calculate colors from matching sites that can be placed on the map
  exploration_map_style <- reactive({
    field <- req(input$exploration_map_option)
    values <- exploration_map_sites()[[field]]
    available <- if (is.numeric(values)) is.finite(values) else !is.na(values) & nzchar(as.character(values))
    shown <- values[available]
    gradient <- if (is.numeric(shown)) map_gradient(field, shown)
    palette <- map_palette(exploration[[field]], gradient)
    list(field = field, available = available, values = shown, gradient = gradient, palette = palette)
  })

  output$exploration_map_legend <- renderUI({
    style <- exploration_map_style()
    if (length(style$values)) map_legend(style$field, style$values, style$gradient, style$palette)
  })

  output$exploration_map_note <- renderText({
    shown <- length(exploration_map_style()$values)
    if (!shown) return("No matching sites have both coordinates and a value for the selected color field.")
    if (shown < nrow(filtered_exploration())) {
      return(paste(format(shown, big.mark = ","), "matching sites have coordinates and a value for the selected color field."))
    }
    NULL
  })

  output$exploration_map <- renderLeaflet({
    style <- exploration_map_style()
    sites <- exploration_map_sites()[style$available, , drop = FALSE]
    map <- site_map
    if (nrow(sites)) {
      values <- if (is.numeric(style$values)) whole_number(style$values) else as.character(style$values)
      field_label <- if (style$field %in% unname(solute_choices)) spatial_label(style$field) else style$field
      popup <- paste0("Stream Name: ", htmltools::htmlEscape(sites[["Stream Name"]]),
        "<br>", htmltools::htmlEscape(field_label), ": ", htmltools::htmlEscape(values))
      map <- map %>%
        addCircleMarkers(data = sites, lat = ~Latitude, lng = ~Longitude,
          label = ~`Stream Name`, popup = popup, radius = 4, weight = 0.7,
          color = app_ink, opacity = 0.45, fillOpacity = 0.55,
          fillColor = style$palette(style$values))
    }

    # start with the full extent and preserve the view as filters change
    center <- isolate(input$exploration_map_center)
    zoom <- isolate(input$exploration_map_zoom)
    if (!is.null(center) && !is.null(zoom)) {
      return(setView(map, center$lng, center$lat, zoom))
    }
    longitude <- range(exploration$Longitude[has_coordinates])
    latitude <- range(exploration$Latitude[has_coordinates])
    fitBounds(map, longitude[1], latitude[1], longitude[2], latitude[2],
      options = list(padding = c(10, 10)))
  })

  output$exploration_site_count <- renderText({
    paste(format(nrow(filtered_exploration()), big.mark = ","), "of",
      format(nrow(exploration), big.mark = ","), "sites match")
  })

  outputOptions(output, "exploration_filter_controls", suspendWhenHidden = FALSE)
  outputOptions(output, "exploration_site_count", suspendWhenHidden = FALSE)

  # for histogram plot
  solute_drivers_v3 <- reactive({

    req(input$driver_solute_dropdown)
    data.frame(Mean_Value = filtered_exploration()[[input$driver_solute_dropdown]])

  })

  # render plot
  output$driver_histogram <- renderPlot({

    validate(need(any(is.finite(solute_drivers_v3()$Mean_Value)), "No matching sites have data for this plot."))
    ggplot(solute_drivers_v3(), aes(x = Mean_Value)) + geom_histogram(bins = 50, fill = app_blue, colour = "white") + app_plot_theme() +
      labs(x = spatial_label(input$driver_solute_dropdown), y = "Count") +
      scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number)

  })

  # for x-y plot
  solute_drivers_v2 <- reactive({

    req(input$x_axis_dropdown, input$y_axis_dropdown, input$color_dropdown)
    sites <- filtered_exploration()
    points <- data.frame(Mean_Value = sites[[input$x_axis_dropdown]],
      Mean_Value2 = sites[[input$y_axis_dropdown]], Mean_Value3 = sites[[input$color_dropdown]])
    points[!is.na(points$Mean_Value3), , drop = FALSE]

  })

  # switch between the available plot views
  whichplot3 <- reactiveVal(TRUE)

  driver_solute_plot_noLR <- reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col = Mean_Value3)) + geom_point() +
    labs(x = spatial_label(input$x_axis_dropdown), y = spatial_label(input$y_axis_dropdown),
      col = input$color_dropdown) +
    app_plot_theme() + app_category_scale())



  driver_solute_plot_LR <- reactive(ggplot(solute_drivers_v2(), aes(Mean_Value, Mean_Value2, col = Mean_Value3)) + geom_point() +
    labs(x = spatial_label(input$x_axis_dropdown), y = spatial_label(input$y_axis_dropdown),
      col = input$color_dropdown) + app_plot_theme() + app_category_scale() +
    geom_smooth(method = "lm", se = F, col = app_ink) +
    ggpmisc::stat_poly_eq(aes(label = after_stat(whole_fit_label(b_0, b_1, r.squared))),
      output.type = "numeric", parse = FALSE, family = "Arial", size = 7, col = app_ink))

  observeEvent(input$LR_button, {
    whichplot3(!whichplot3())
  })

  which_graph3 <- reactive({
    if (whichplot3()) {
      driver_solute_plot_noLR()
    } else {
      driver_solute_plot_LR()
    }
  })

  # render plot
  output$driver_solute_plot <- renderPlot({

    points <- solute_drivers_v2()
    validate(need(any(is.finite(points$Mean_Value) & is.finite(points$Mean_Value2)),
      "No matching sites have data for this plot."))
    which_graph3() +
      scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number)

  })

  # show the years section only for plots with an annual spatial variable
  output$histogram_years_section <- renderUI({
    if (any(input$driver_solute_dropdown %in% names(spatial_years))) {
      tags$details(tags$summary("Years used by site"), DT::DTOutput("histogram_years"))
    }
  })

  relationship_variables <- reactive({
    c(input$x_axis_dropdown, input$y_axis_dropdown, input$color_dropdown)
  })

  output$relationship_years_section <- renderUI({
    if (any(relationship_variables() %in% names(spatial_years))) {
      tags$details(tags$summary("Years used by site"), DT::DTOutput("relationship_years"))
    }
  })

  # show site coverage for the annual variables selected in each plot
  output$histogram_years <- DT::renderDT({
    req(input$driver_solute_dropdown)
    details <- site_year_details(input$driver_solute_dropdown)
    validate(need(!is.null(details), "The selected variable has no annual spatial coverage to report."))
    details <- details[details[["Stream Name"]] %in% filtered_exploration()[["Stream Name"]], , drop = FALSE]
    DT::datatable(details, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  output$relationship_years <- DT::renderDT({
    req(input$x_axis_dropdown, input$y_axis_dropdown, input$color_dropdown)
    details <- site_year_details(relationship_variables())
    validate(need(!is.null(details), "The selected variables have no annual spatial coverage to report."))
    details <- details[details[["Stream Name"]] %in% filtered_exploration()[["Stream Name"]], , drop = FALSE]
    DT::datatable(details, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })

  # ---- site comparison panel server ----

  # ---- comparative boxplot of one solute across many sites ----
  observe({

    req(input$app_tab == "Site Comparison")
    sites_LTER3 <- master_chem %>%
      filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Country %in% input$dropdown_country3) %>%
      distinct(Stream_Name)

    selected <- intersect(isolate(input$dropdown_site3), sites_LTER3$Stream_Name)
    if (!length(selected)) selected <- head(sites_LTER3$Stream_Name, 1)
    updateSelectInput(session, "dropdown_site3", choices = sites_LTER3$Stream_Name,
      selected = selected)

  })


  # filter master chem by solute
  chem_v4 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute3)
  })

  # then by site selections
  chem_actual3 <- reactive({

    chem_v4() %>%
      dplyr::filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Stream_Name %in% input$dropdown_site3)
  })

  output$solute_boxplot <- renderPlot({

    ggplot(chem_actual3(), aes(x = Stream_Name, y = value)) + geom_boxplot(fill = app_light, colour = app_teal) + app_plot_theme() +
      labs(y = paste0(solute_notation(input$button_solute3), " (", solute_units(input$button_solute3), ")"), x = "Site") + scale_y_continuous(labels = whole_number) + theme(
        axis.text.x = element_text(angle = 45, hjust = 1))

  })

  # ---- time series plot with slider bar across many sites ----
  # update available sites when the country or filters change
  # list matching sites in the selected countries
  observe({

    req(input$app_tab == "Site Comparison")
    sites_LTER2 <- master_chem %>%
      filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Country %in% input$dropdown_country2) %>%
      distinct(Stream_Name)

    selected <- intersect(isolate(input$dropdown_site2), sites_LTER2$Stream_Name)
    if (!length(selected)) selected <- head(sites_LTER2$Stream_Name, 1)
    updateSelectInput(session, "dropdown_site2", choices = sites_LTER2$Stream_Name,
      selected = selected)

  })

  # filter master chem by solute
  chem_v3 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_solute2)
  })

  # then by site selections
  chem_actual2 <- reactive({

    chem_v3() %>%
      dplyr::filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Stream_Name %in% input$dropdown_site2)
  })

  min_date2 <- reactive({
    chem_actual2() %>%
      dplyr::summarise(min_overall = min(min)) %>%
      dplyr::slice_min(min_overall)
  })

  max_date2 <- reactive({
    chem_actual2() %>%
      dplyr::summarise(max_overall = max(max)) %>%
      dplyr::slice_max(max_overall)
  })

  observe({

    req(input$app_tab == "Site Comparison", nrow(chem_actual2()) > 0)
    updateSliderInput(session, "xslider2", min = as.Date(min_date2()$min_overall),
      max = as.Date(max_date2()$max_overall), value = c(as.Date(min_date2()$min_overall),
        as.Date(max_date2()$max_overall)))

  })

  # switch between the available plot views
  whichplot2 <- reactiveVal(TRUE)

  # all time series data
  all_data2 <- reactive(ggplot(chem_actual2(), aes(date, value, col = Stream_Name)) + labs(x = "Date", y = paste0(solute_notation(input$button_solute2), " (", solute_units(input$button_solute2), ")"), col = "Site") +
    geom_point() + app_plot_theme() + app_category_scale() + xlim(input$xslider2))

  # seasonal time series data
  seasonal_data2 <- reactive(ggplot(chem_actual2(), aes(factor(month(date)), value, fill = factor(Stream_Name))) +
    labs(x = "Month", y = paste0(solute_notation(input$button_solute2), " (", solute_units(input$button_solute2), ")"), fill = "Site") +
    geom_boxplot() + app_plot_theme() + app_category_scale())

  observeEvent(input$seasonal_button2, {
    whichplot2(!whichplot2())
  })

  which_graph2 <- reactive({
    if (whichplot2()) {
      all_data2()
    } else {
      seasonal_data2()
    }
  })

  # render plot
  output$timeseries_out2 <- renderPlot({

    which_graph2() + scale_y_continuous(labels = whole_number)

  })

  # ---- cq plot ----
  observe({

    req(input$app_tab == "Site Comparison")
    sites_LTER4 <- cq_all_nodups %>%
      filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Country %in% input$dropdown_country4, variable %in% input$button_solute4,
        Qcms > 0, value > 0) %>%
      distinct(Stream_Name)

    selected_sites <- intersect(isolate(input$dropdown_site4), sites_LTER4$Stream_Name)
    if (!length(selected_sites)) selected_sites <- head(sites_LTER4$Stream_Name, 1)
    updateSelectInput(session, "dropdown_site4", choices = sites_LTER4$Stream_Name,
      selected = selected_sites)

  })


  cq2 <- reactive({
    cq_all_nodups %>%
      dplyr::filter(variable == input$button_solute4, Qcms > 0, value > 0)
  })

  # then by site selections
  cq3 <- reactive({

    cq2() %>%
      dplyr::filter(Stream_Name %in% filtered_exploration()[["Stream Name"]], Stream_Name %in% input$dropdown_site4)
  })

  output$cq_plot <- renderPlot({

    validate(need(nrow(cq3()) > 0, "Choose a site with paired chemistry and flow for this solute."))
    ggplot(cq3(), aes(log(Qcms), log(value), col = Stream_Name)) + geom_point() +
      labs(x = "Log[discharge (m³ s⁻¹)]", y = paste0("Log[concentration (", solute_units(input$button_solute4), ")]"), col = "Site") +
      scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number) +
      geom_smooth(se = F, method = "lm") +
      ggpmisc::stat_poly_eq(aes(label = after_stat(whole_fit_label(b_0, b_1, r.squared))),
      output.type = "numeric", parse = FALSE, family = "Arial", size = 7) +
      app_plot_theme() + app_category_scale()

  })

  # ---- site specific time series panel server ----

  # list matching sites in the selected country
  observe({

    req(input$app_tab == "Site Specific Time Series", input$dropdown_country)
    sites_LTER <- master_chem %>%
      filter(Country == input$dropdown_country,
        Stream_Name %in% filtered_exploration()[["Stream Name"]]) %>%
      distinct(Stream_Name)

    selected <- intersect(isolate(input$dropdown_site), sites_LTER$Stream_Name)
    updateSelectInput(session, "dropdown_site", choices = sites_LTER$Stream_Name,
      selected = if (length(selected)) selected else head(sites_LTER$Stream_Name, 1))

  })

  # keep the selected solutes for the site time series
  chem_v2 <- reactive({
    master_chem %>%
      dplyr::filter(variable %in% input$button_spp)
  })

  # then filter solute information by selected site
  chem_actual <- reactive({

    chem_v2() %>%
      dplyr::filter(Stream_Name %in% input$dropdown_site,
        Stream_Name %in% filtered_exploration()[["Stream Name"]])
  })

  min_date <- reactive({
    chem_actual() %>%
      select("min") %>%
      distinct()
  })

  max_date <- reactive({
    chem_actual() %>%
      select("max") %>%
      distinct()
  })

  observe({

    req(input$app_tab == "Site Specific Time Series", nrow(chem_actual()) > 0)
    updateSliderInput(session, "xslider", min = as.Date(min_date()$min),
      max = as.Date(max_date()$max), value = c(as.Date(min_date()$min), as.Date(max_date()$max)))

  })

  # remember whether to show the seasonal or full time series
  # here we will use it to display either the full time series or seasonal data
  whichplot <- reactiveVal(TRUE)

  # define the two plots
  # first full time series
  all_data <- reactive(ggplot(chem_actual(), aes(date, value, col = solute_notation(variable))) + labs(x = "Date", y = "Concentration (mg L⁻¹)", col = "Solute") +
    geom_point() + app_plot_theme() + theme(legend.position = "bottom") + app_category_scale() +
    xlim(input$xslider))

  # then seasonal data
  seasonal_data <- reactive(ggplot(chem_actual(), aes(factor(month(date)), value, fill = solute_notation(variable))) +
    labs(x = "Month", y = "Concentration (mg L⁻¹)", fill = "Solute") +
    geom_boxplot() + app_plot_theme() + theme(legend.position = "bottom") + app_category_scale())

  # choose the time series view based on the toggle
  observeEvent(input$seasonal_button, {
    whichplot(!whichplot())
  })

  which_graph <- reactive({
    if (whichplot()) {
      all_data()
    } else {
      seasonal_data()
    }
  })

  # this renders the plot output based on the whichplot selection above (annual vs seasonal)
  output$timeseries_out <- renderPlot({

    which_graph() + scale_y_continuous(labels = whole_number)

  })

} # close server function
