add_location_key <- function(ctdp, epsilon = 1e-3) {
  # Extract coords
  coords <- ctdp$data$deployments |>
    select(longitude, latitude) |>
    as.matrix()
  
  # Find adjacent deployments
  dist_matrix <- as.matrix(dist(coords))
  adjacency <- dist_matrix <= epsilon
  g <- graph_from_adjacency_matrix(adjacency, mode = "undirected", diag = FALSE)
  
  # Add locationKey based on connected components
  ctdp$data$deployments$locationKey <- as.factor(components(g)$membership)
  
  return(ctdp)
}

my_basemap <- function(data) {
  avg_lng <- mean(data$longitude, na.rm = TRUE)
  avg_lat <- mean(data$latitude, na.rm = TRUE)
  
  leaflet() |>
    addTiles(options = tileOptions(crossOrigin = TRUE)) |>
    setView(lng = avg_lng, lat = avg_lat, zoom = 13) |>
    # Add the download button only once here
    addEasyButton(easyButton(
      icon = "fa-download",
      title = "Download Map",
      onClick = JS("
          function(btn, map) {
            // Trigger a Shiny event
            Shiny.onInputChange('trigger_download', new Date());
          }
        ")
    ))
}

my_map <- function(data, input, layerID, cols, popup = NULL, update = TRUE) {
  
  if (layerID != "effort_map") {
    
    # filter on species
    my_species <- switch(
      layerID, 
      "map" = input$selected_species_map, 
      "map2A" = input$species_map2A, 
      "map2B" = input$species_map2B,
    )
    data <- data |> filter(scientificName == my_species)
    
  } 
  validate(
    need(nrow(data) > 0, "No data available for the selected combination of feature, species and/or year.")
  )
  
  # define title of the legend
  if (layerID != "effort_map") {
    feature_convert <- c("n_obs", "n_individuals", "rai", "rai_individuals")
    names(feature_convert) <- c("n_obs", "n_ind", "rai_obs", "rai_ind")
    title <- camtraptor:::get_legend_title(feature_convert[input$selected_feature])
  } else {
    title <- camtraptor:::get_legend_title("effort")
  }
  title <- camtraptor:::add_unit_to_legend_title(
    title,
    unit = "days",
    use_brackets = TRUE
  )
  
  max_scale <- NULL
  zero_values_icon_url <- "https://img.icons8.com/ios-glyphs/30/000000/multiply.png"
  zero_values_icon_size <- 10
  na_values_icon_url <- "https://img.icons8.com/ios-glyphs/30/FA5252/multiply.png"
  na_values_icon_size <- 10
  radius_range <- c(10, 50)
  
  # Normalize the feature values for dynamic radius scaling
  max_value <- max(data$n, na.rm = TRUE)
  min_value <- min(data$n, na.rm = TRUE)
  
  # Avoid division by zero if all values are equal
  if (max_value == min_value) {
    data <- data |>
      mutate(radius = 1)  # Default radius if all values are identical
  } else {
    data <- data |>
      mutate(radius = scales::rescale(n, to = c(1, 10)))  # Scale radius between 3 and 10
  }
  
  # Color scale based on quantiles
  # max number of species/obs (with possible upper limit  `max_absolute_scale`
  # in case absolute scale is used) to set number of ticks in legend
  max_n <- ifelse(
    is.null(max_scale),
    ifelse(!all(is.na(data$n)), max(data$n, na.rm = TRUE), 0), 
    max_scale)
  
  # define colour palette
  grad <- switch(
    layerID,
    "map" = colorRampPalette(c("#FFFFFF", cols[which(input$selected_species == my_species)])),
    "map2A" = colorRampPalette(c("#FFFFFF", cols[which(input$selected_species == my_species)])),
    "map2B" = colorRampPalette(c("#FFFFFF", cols[which(input$selected_species == my_species)])),
    "effort_map" = colorRampPalette(c("#FFFFFF", inbo_donkerblauw))
  )
  
  # Generate palette for numeric values
  pal <- leaflet::colorNumeric(
    palette = grad(100), 
    domain = c(0, max_n))
  
  # define bins for ticks of legend
  # bins <- ifelse(max_n < 6, as.integer(max_n) + 1, 6)
  bins <- 6
  
  # define size scale for avoiding too small or too big circles
  radius_max <- radius_range[2]
  radius_min <- radius_range[1]
  if (max_n != 0) {
    conv_factor <- (radius_max - radius_min) / max_n
  } else {
    conv_factor <- 0
  }
  
  # define legend values
  legend_values <- seq(from = 0, to = max_n, length.out = bins)
  
  # non_zero values deploys (n > 0 and is not NA)
  non_zero_values <- data |> dplyr::filter(.data$n > 0)
  # zero values
  zero_values <- data |> dplyr::filter(.data$n == 0)
  # NA values (only returned by get_n_species)
  na_values <- data |> dplyr::filter(is.na(.data$n))
  # make basic start map
  if (update) {
    leaflet_map <-
      leafletProxy(layerID) |>
      clearMarkers() |>
      clearMinicharts() |>
      removeControl(layerId = "legend")
  } else {
    leaflet_map <- my_basemap(data)
  }
  
  # add markers for deployments with zero values if needed
  if (nrow(zero_values) > 0) {
    # create icon for zero values
    zero_icons <- leaflet::icons(
      iconUrl = zero_values_icon_url,
      iconWidth = zero_values_icon_size,
      iconHeight = zero_values_icon_size
    )
    # add icons for zero values to the map
    leaflet_map <-
      leaflet_map |>
      leaflet::addMarkers(
        icon = zero_icons,
        data = zero_values,
        lng = ~longitude,
        lat = ~latitude
      )
  }
  
  # add markers for deployments with NA values if needed
  if (nrow(na_values) > 0) {
    # create icons for NA values
    na_icons <- leaflet::icons(
      iconUrl = na_values_icon_url,
      iconWidth = na_values_icon_size,
      iconHeight = na_values_icon_size
    )
    # add icons with NAs to the map
    leaflet_map <-
      leaflet_map |>
      leaflet::addMarkers(
        icon = na_icons,
        data = na_values,
        lng = ~longitude,
        lat = ~latitude
      )
  }
  if (nrow(non_zero_values) > 0) {
    leaflet_map <-
      leaflet_map |>
      leaflet::addCircleMarkers(
        data = non_zero_values,
        layerId = ~locationKey,
        lng = ~longitude,
        lat = ~latitude,
        radius = ~ifelse(is.na(n), radius_min, n * conv_factor + radius_min),
        color = ~pal(n),
        stroke = FALSE,
        fillOpacity = 0.8
      ) |>
      leaflet::addLegend(
        "bottomright",
        pal = pal,
        values = legend_values,
        title = title,
        opacity = 1,
        bins = bins,
        na.label = "",
        labFormat = camtraptor:::labelFormat_scale(max_scale = max_scale),
        layerId = "legend"
      )
    if (!is.null(popup)) {
      leaflet_map <-
        leaflet_map |>
        clearPopups() |>
        addPopups(
          lng = popup$lng,
          lat = popup$lat,
          popup = leafpop::popupGraph(popup$graph)
        )
    }
  }
  leaflet_map
}

my_pie_map <- function(data, input, layerID, cols, update = TRUE) {
  
  # filter on species
  data <- data |> filter(scientificName %in% input$selected_species)
  
  # to wide format
  data_wide <- data |> 
    pivot_wider(names_from = scientificName, values_from = n)
  
  validate(
    need(nrow(data) > 0, "No data available for the selected combination of feature, species and/or year.")
  )
  
  # define title of the legend
  feature_convert <- c("n_obs", "n_individuals", "rai", "rai_individuals")
  names(feature_convert) <- c("n_obs", "n_ind", "rai_obs", "rai_ind")
  title <- camtraptor:::get_legend_title(feature_convert[input$selected_feature])
  title <- camtraptor:::add_unit_to_legend_title(
    title,
    unit = "days",
    use_brackets = TRUE
  )
  
  # make basic start map
  if (update) {
    leaflet_map <-
      leafletProxy(layerID) |>
      clearMarkers() |>
      clearMinicharts() |>
      removeControl(layerId = "legend")
    
  } else {
    leaflet_map <- my_basemap(data)
  }
  
  data_mtx <- data_wide |> select(-c(locationKey:latitude)) |> as.matrix()
  totSum <- rowSums(data_mtx)
  max_radius <- 60 
  scaled_radius <- max_radius * sqrt(totSum) / sqrt(max(totSum))
  pie_cols <- cols[rank(input$selected_species)]
  
  leaflet_map <-
    leaflet_map |>
    addMinicharts(
      data_wide$longitude, data_wide$latitude,
      type = "pie",
      chartdata = data_mtx,
      colorPalette = pie_cols,
      width = scaled_radius,
      opacity = 0.8,
      popup = popupArgs(noPopup = TRUE)
    ) |>
    leaflet::addCircleMarkers(
      data = data_wide,
      layerId = ~locationKey,
      lng = ~longitude,
      lat = ~latitude,
      color = "transparent",
      fillColor = "transparent",
      opacity = 0,
      fillOpacity = 0
    )
  leaflet_map
}

add_plot_maximize_observer <- function(input,
                                       box_id,
                                       plot_name,
                                       non_max_height = "400px") {
  observeEvent(input[[box_id]]$maximized, {
    plot_height <- if (input[[box_id]]$maximized) {
      "100%"
    } else {
      non_max_height
    }
    
    js_call <- sprintf(
      "
      setTimeout(() => {
        $('#%s').css('height', '%s');
      }, 300)
      $('#%s').trigger('resize');
      ",
      plot_name,
      plot_height,
      plot_name
    )
    shinyjs::runjs(js_call)
  }, ignoreInit = TRUE)
}

get_custom_effort2 <- function(ctdp, start_ = NULL, end_ = NULL, group_by = NULL, unit = "days") {
  
  dep <- ctdp$data$deployments
  
  start_ <- if (is.null(start_)) as.Date(ctdp$temporal$start) else start_
  end_ <- if (is.null(end_)) as.Date(ctdp$temporal$end) else end_
  
  if (!is.null(group_by)) {
    dates <- seq(floor_date(start_, group_by), ceiling_date(end_, group_by), by = group_by)
    
    dep <- map_dfr(1:(length(dates) - 1), function(i) {
      interval_ <- interval(dates[i], dates[i + 1])
      
      dep |>
        filter(int_overlaps(interval(start, end), interval_)) |>
        mutate(
          begin = dates[i],
          start = pmax(start, dates[i]),  # Vectorized alternative to `if_else`
          end = pmin(end, dates[i + 1])
        )
    })
    
    out <- dep |>
      mutate(effort = as.numeric(difftime(end, start, units = unit))) |>
      group_by(begin, deploymentID) |>
      summarise(
        locationKey = first(locationKey),
        effort = sum(effort),
        .groups = "drop"
      ) |>
      mutate(unit = unit)
    
  } else {
    dep <- dep |>
      filter(int_overlaps(interval(start, end), interval(start_, end_))) |>
      mutate(effort = as.numeric(difftime(end, start, units = unit)))
    
    out <- dep |>
      group_by(deploymentID) |>
      summarise(
        locationKey = first(locationKey),
        effort = sum(effort),
        .groups = "drop"
      ) |>
      mutate(unit = unit)
  }
  
  return(out)
}

