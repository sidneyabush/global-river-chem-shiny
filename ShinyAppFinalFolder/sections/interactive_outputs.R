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
  filter_id <- function(field, prefix = "exploration_filter_") paste0(prefix, match(field, names(exploration)))
  selected_filters <- reactive({
    head(intersect(input$exploration_filters, exploration_filter_labels), 3)
  })

  numeric_filter_controls <- function(fields, prefix) {
    controls <- lapply(fields, function(field) {
      id <- filter_id(field, prefix)
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
  }

  output$exploration_filter_controls <- renderUI({
    numeric_filter_controls(selected_filters(), "exploration_filter_")
  })

  filter_site_ranges <- function(sites, fields, prefix) {
    for (field in fields) {
      values <- sites[[field]]
      selected <- input[[filter_id(field, prefix)]]
      keep <- is.finite(values)
      if (length(selected) == 2) keep <- keep & values >= selected[1] & values <= selected[2]
      sites <- sites[keep, , drop = FALSE]
    }
    sites
  }

  filtered_exploration <- reactive({
    filter_site_ranges(exploration, selected_filters(), "exploration_filter_")
  })

  # show the matching sites with known coordinates
  exploration_map_sites <- reactive({
    sites <- filtered_exploration()
    sites[is.finite(sites$Latitude) & is.finite(sites$Longitude), , drop = FALSE]
  })

  # use one color scale for all matching sites, including sites without coordinates
  exploration_map_style <- reactive({
    field <- req(input$exploration_map_option)
    values <- filtered_exploration()[[field]]
    available <- if (is.numeric(values)) is.finite(values) else !is.na(values) & nzchar(as.character(values))
    shown <- values[available]
    gradient <- if (is.numeric(shown)) map_gradient(field, shown)
    palette <- if (length(shown)) map_palette(exploration[[field]], gradient)
    list(field = field, values = shown, gradient = gradient, palette = palette)
  })

  # only sites with coordinates and a color value can appear as map markers
  colored_map_sites <- reactive({
    sites <- exploration_map_sites()
    values <- sites[[exploration_map_style()$field]]
    available <- if (is.numeric(values)) is.finite(values) else !is.na(values) & nzchar(as.character(values))
    sites[available, , drop = FALSE]
  })

  output$exploration_map_legend <- renderUI({
    style <- exploration_map_style()
    if (length(style$values)) map_legend(style$field, style$values, style$gradient, style$palette)
  })

  output$exploration_map_note <- renderText({
    shown <- nrow(colored_map_sites())
    if (!shown) return("No matching sites have both coordinates and a value for the selected color field.")
    if (shown < nrow(filtered_exploration())) {
      return(paste(format(shown, big.mark = ","), "matching sites have coordinates and a value for the selected color field."))
    }
    NULL
  })

  output$exploration_map <- renderLeaflet({
    style <- exploration_map_style()
    sites <- colored_map_sites()
    map <- site_map
    if (nrow(sites)) {
      color_values <- sites[[style$field]]
      values <- if (is.numeric(color_values)) whole_number(color_values) else as.character(color_values)
      field_label <- if (style$field %in% unname(solute_choices)) spatial_label(style$field) else style$field
      popup <- paste0("Stream Name: ", htmltools::htmlEscape(sites[["Stream Name"]]),
        "<br>", htmltools::htmlEscape(field_label), ": ", htmltools::htmlEscape(values))
      map <- map %>%
        addCircleMarkers(data = sites, lat = ~Latitude, lng = ~Longitude,
          label = ~`Stream Name`, popup = popup, radius = 4, weight = 0.7,
          color = app_ink, opacity = 0.45, fillOpacity = 0.55,
          fillColor = style$palette(color_values))
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

  histogram_variables <- reactive({
    head(intersect(input$driver_solute_dropdown, quant_drivers), 3)
  })

  # keep a separate axis and sample size for each selected variable
  histogram_values <- function(variable) {
    values <- filtered_exploration()[[variable]]
    values[is.finite(values)]
  }

  output$exploration_histograms <- renderUI({
    variables <- histogram_variables()
    if (!length(variables)) return(tags$p("Choose a variable to see its distribution."))
    panels <- lapply(seq_along(variables), function(index) {
      tags$div(
        tags$p(class = "small mb-0", textOutput(paste0("histogram_count_", index), inline = TRUE)),
        plotOutput(paste0("driver_histogram_", index), height = "300px")
      )
    })
    bslib::layout_column_wrap(width = "280px", !!!panels)
  })

  for (index in seq_len(3)) {
    local({
      slot <- index
      output[[paste0("histogram_count_", slot)]] <- renderText({
        variables <- histogram_variables()
        req(length(variables) >= slot)
        paste(whole_number(length(histogram_values(variables[slot]))), "of",
          whole_number(nrow(filtered_exploration())), "matching sites have data")
      })
      output[[paste0("driver_histogram_", slot)]] <- renderPlot({
        variables <- histogram_variables()
        req(length(variables) >= slot)
        variable <- variables[slot]
        values <- histogram_values(variable)
        validate(need(length(values) > 0L, "No matching sites have data for this variable."))
        ggplot(data.frame(value = values), aes(x = value)) +
          geom_histogram(bins = 50, fill = app_blue, colour = "white") +
          app_plot_theme() +
          labs(x = spatial_label(variable), y = "Sites") +
          scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number)
      })
    })
  }

  # a missing color value does not exclude a site from the scatterplot
  relationship_points <- reactive({
    req(input$x_axis_dropdown, input$y_axis_dropdown)
    style <- exploration_map_style()
    sites <- filtered_exploration()
    x <- sites[[input$x_axis_dropdown]]
    y <- sites[[input$y_axis_dropdown]]
    keep <- is.finite(x) & is.finite(y)
    values <- sites[[style$field]][keep]
    available <- if (is.numeric(values)) is.finite(values) else !is.na(values) & nzchar(as.character(values))
    colors <- rep(app_missing, length(values))
    if (any(available)) colors[available] <- style$palette(values[available])
    data.frame(x = x[keep], y = y[keep], color = colors)
  })

  output$relationship_site_count <- renderText({
    paste(whole_number(nrow(relationship_points())), "of",
      whole_number(nrow(filtered_exploration())), "matching sites have data for both axes")
  })

  show_regression <- reactiveVal(FALSE)
  observeEvent(input$LR_button, {
    show_regression(!show_regression())
  })

  output$driver_solute_plot <- renderPlot({
    points <- relationship_points()
    validate(need(nrow(points) > 0L, "No matching sites have data for both axes."))
    plot <- ggplot(points, aes(x, y)) +
      geom_point(aes(colour = color)) +
      scale_colour_identity() +
      labs(x = spatial_label(input$x_axis_dropdown), y = spatial_label(input$y_axis_dropdown)) +
      app_plot_theme() +
      scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number)

    if (show_regression() && nrow(points) >= 3L && length(unique(points$x)) > 1L) {
      plot <- plot +
        geom_smooth(method = "lm", se = FALSE, colour = app_ink) +
        ggpmisc::stat_poly_eq(aes(label = after_stat(whole_fit_label(b_0, b_1, r.squared))),
          output.type = "numeric", parse = FALSE, family = "Arial", size = 7, colour = app_ink)
    }
    plot
  })

  # ---- site comparison panel server ----
  comparison_filters <- reactive({
    head(intersect(input$comparison_filters, exploration_filter_labels), 3)
  })

  output$comparison_filter_controls <- renderUI({
    numeric_filter_controls(comparison_filters(), "comparison_filter_")
  })

  comparison_map_sites <- reactive({
    sites <- exploration
    if (isTruthy(input$comparison_country)) {
      sites <- sites[which(sites$Country == input$comparison_country), , drop = FALSE]
    }
    if (isTruthy(input$comparison_network)) {
      sites <- sites[which(sites[["Observation Network"]] == input$comparison_network), , drop = FALSE]
    }
    sites <- filter_site_ranges(sites, comparison_filters(), "comparison_filter_")
    sites[is.finite(sites$Latitude) & is.finite(sites$Longitude), , drop = FALSE]
  })

  picked_sites <- reactiveVal(character())
  comparison_selection <- reactive({
    intersect(picked_sites(), comparison_map_sites()[["Stream Name"]])
  })

  # remove hidden selections when the map filters change
  observeEvent(comparison_map_sites(), {
    picked_sites(comparison_selection())
  })

  observeEvent(input$comparison_map_marker_click, {
    site <- input$comparison_map_marker_click$id
    req(site %in% comparison_map_sites()[["Stream Name"]])
    selected <- comparison_selection()
    if (site %in% selected) {
      picked_sites(setdiff(selected, site))
    } else if (length(selected) < 3L) {
      picked_sites(c(selected, site))
    } else {
      showNotification("Remove a selected site before adding another.", type = "message")
    }
  })

  observeEvent(input$clear_comparison_sites, picked_sites(character()))

  comparison_colors <- reactive({
    sites <- comparison_selection()
    setNames(category_colors(length(sites)), sites)
  })

  output$comparison_selection <- renderUI({
    sites <- comparison_selection()
    colors <- comparison_colors()
    tags$div(class = "map-legend",
      tags$p(paste(length(sites), "of 3 sites selected")),
      if (length(sites)) tags$ul(lapply(sites, function(site) {
        tags$li(tags$span(class = "map-legend-swatch", `aria-hidden` = "true",
          style = paste0("background-color: ", colors[[site]])), site)
      }))
    )
  })

  output$comparison_map_count <- renderText({
    paste(whole_number(nrow(comparison_map_sites())), "matching sites on the map")
  })

  output$comparison_map <- renderLeaflet({
    sites <- comparison_map_sites()
    map <- site_map
    if (nrow(sites)) {
      colors <- unname(comparison_colors()[sites[["Stream Name"]]])
      selected <- !is.na(colors)
      colors[!selected] <- app_missing
      map <- map %>% addCircleMarkers(data = sites, lat = ~Latitude, lng = ~Longitude,
        layerId = ~`Stream Name`, label = ~`Stream Name`, radius = ifelse(selected, 7, 4),
        color = app_ink, weight = 1, opacity = 0.75,
        fillOpacity = ifelse(selected, 0.95, 0.5), fillColor = colors)
    }

    # keep the current view so clicking a site does not reset the map
    center <- isolate(input$comparison_map_center)
    zoom <- isolate(input$comparison_map_zoom)
    if (!is.null(center) && !is.null(zoom)) {
      return(setView(map, center$lng, center$lat, zoom))
    }
    longitude <- range(exploration$Longitude[has_coordinates])
    latitude <- range(exploration$Latitude[has_coordinates])
    fitBounds(map, longitude[1], latitude[1], longitude[2], latitude[2],
      options = list(padding = c(10, 10)))
  })

  # use the same colors on the map and across all comparison plots
  comparison_color_scale <- function() {
    scale_colour_manual(values = comparison_colors(), aesthetics = c("colour", "fill"))
  }

  comparison_chemistry <- function(solute) {
    selected <- comparison_selection()
    req(length(selected), solute)
    dplyr::filter(master_chem, Stream_Name %in% selected, variable == solute, is.finite(value))
  }

  # explain when some selected sites cannot appear in a particular plot
  comparison_data_note <- function(rows) {
    missing <- setdiff(comparison_selection(), rows$Stream_Name)
    if (length(missing)) paste("No data for this plot:", paste(missing, collapse = ", "))
  }

  # ---- solute distributions ----
  chem_actual3 <- reactive(comparison_chemistry(input$button_solute3))
  output$comparison_boxplot_note <- renderText(comparison_data_note(chem_actual3()))

  output$solute_boxplot <- renderPlot({
    validate(need(length(comparison_selection()), "Select sites on the map to compare them."))
    rows <- chem_actual3()
    validate(need(nrow(rows), "No selected sites have data for this solute."))
    ggplot(rows, aes(x = Stream_Name, y = value, fill = Stream_Name)) +
      geom_boxplot(show.legend = FALSE) + comparison_color_scale() + app_plot_theme() +
      labs(y = paste0(solute_notation(input$button_solute3), " (", solute_units(input$button_solute3), ")"), x = "Site") +
      scale_y_continuous(labels = whole_number) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # ---- time series ----
  chem_actual2 <- reactive(comparison_chemistry(input$button_solute2))
  show_time_series <- reactiveVal(TRUE)

  observeEvent(input$seasonal_button2, {
    show_time_series(!show_time_series())
  })

  observe({
    req(input$app_tab == "Site Comparison", length(comparison_selection()))
    rows <- chem_actual2()
    req(nrow(rows) > 0L)
    dates <- c(min(rows$min), max(rows$max))
    if (dates[1] == dates[2]) dates[2] <- dates[2] + 1
    updateSliderInput(session, "xslider2", min = dates[1], max = dates[2], value = dates)
  })

  comparison_time_rows <- reactive({
    rows <- chem_actual2()
    if (show_time_series() && length(input$xslider2) == 2L) {
      rows <- rows[rows$date >= input$xslider2[1] & rows$date <= input$xslider2[2], , drop = FALSE]
    }
    rows
  })
  output$comparison_timeseries_note <- renderText(comparison_data_note(comparison_time_rows()))

  output$timeseries_out2 <- renderPlot({
    validate(need(length(comparison_selection()), "Select sites on the map to compare them."))
    rows <- comparison_time_rows()
    validate(need(nrow(rows), "No selected sites have data for this solute in this view."))
    units <- paste0(solute_notation(input$button_solute2), " (", solute_units(input$button_solute2), ")")
    if (show_time_series()) {
      plot <- ggplot(rows, aes(date, value, colour = Stream_Name)) +
        geom_point() + labs(x = "Date", y = units, colour = "Site")
      if (length(input$xslider2) == 2L) plot <- plot + xlim(input$xslider2)
    } else {
      plot <- ggplot(rows, aes(factor(month(date)), value, fill = Stream_Name)) +
        geom_boxplot() + labs(x = "Month", y = units, fill = "Site")
    }
    plot + comparison_color_scale() + app_plot_theme() + scale_y_continuous(labels = whole_number)
  })

  # ---- concentration and discharge ----
  cq3 <- reactive({
    selected <- comparison_selection()
    req(length(selected), input$button_solute4)
    dplyr::filter(cq_all_nodups, Stream_Name %in% selected, variable == input$button_solute4,
      is.finite(Qcms), Qcms > 0, is.finite(value), value > 0)
  })
  output$comparison_cq_note <- renderText(comparison_data_note(cq3()))

  output$cq_plot <- renderPlot({
    validate(need(length(comparison_selection()), "Select sites on the map to compare them."))
    rows <- cq3()
    validate(need(nrow(rows), "No selected sites have paired chemistry and flow for this solute."))
    ggplot(rows, aes(log(Qcms), log(value), colour = Stream_Name)) + geom_point() +
      labs(x = "Log[discharge (m³ s⁻¹)]", y = paste0("Log[concentration (", solute_units(input$button_solute4), ")]"), colour = "Site") +
      scale_x_continuous(labels = whole_number) + scale_y_continuous(labels = whole_number) +
      geom_smooth(se = FALSE, method = "lm") +
      ggpmisc::stat_poly_eq(aes(label = after_stat(whole_fit_label(b_0, b_1, r.squared))),
        output.type = "numeric", parse = FALSE, family = "Arial", size = 7) +
      app_plot_theme() + comparison_color_scale()
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
