server <- function(input, output, session) {
  
  main_tab <- reactive({ input$navbar })
  vis_tab   <- reactive({ input$vis_tab })
  
  cols <- c("#FFA500", "#800080", "#377EB8", "#E41A1C", "#4DAF4A", "#A65628", "#F781BF", "#FFFF33")
  col_other <- "#D3D3D3"
  
  ###################
  # Data Import Tab #
  ###################
  # Enable directory browsing
  roots <- c(wd = normalizePath("."))
  shinyDirChoose(input, "browse", roots = roots, session = session, allowDir = TRUE)
  
  # Observe Browse button click
  observeEvent(input$browse, {
    dir_selected <- parseDirPath(roots, input$browse)
    if (!is.null(dir_selected)) {
      updateTextInput(session, "dir_path", value = dir_selected)
    }
  })
  
  # Fetch Camtrap DP datasets from GBIF
  gbif_datasets <- reactive({
    datasets <- occ_count(facet = "datasetKey", protocol = "CAMTRAP_DP")
    safe_dataset_get <- possibly(dataset_get, otherwise = NULL)
    
    datasets <- datasets |>
      rowwise() |>
      mutate(metadata = list(safe_dataset_get(datasetKey))) |>
      unnest_wider(metadata)
    
    return(datasets)
  })
  
  # Update dropdown menu dynamically
  observe({
    datasets <- gbif_datasets()
    
    if (!is.null(datasets)) {
      dataset_choices <- setNames(datasets$datasetKey, datasets$title)
      updatePickerInput(session, "gbif_picker",
                        choices = dataset_choices,
                        selected = NULL)
    }
  })
  
  # Display metadata for the selected dataset
  output$dataset_info <- renderText({
    req(input$gbif_picker)
    dataset <- gbif_datasets() |> filter(datasetKey == input$gbif_picker)
    
    if (nrow(dataset) > 0) {
      paste0(
        "<B>Title:</B> ", dataset$title, "<br><br>",
        "<B>Description:</B> ", dataset$description, "<br><br>",
        "<B>License:</B> ", dataset$license, "<br><br>",
        "<B>DOI:</B> ", dataset$doi, "<br><br>",
        "<B>Citation:</B> ", dataset$citation[[1]][[1]]$text, "<br><br>"
      )
    } else {
      "No dataset selected."
    }
  })
  
  # Store imported data reactively
  imported_data <- reactiveVal(NULL)
  
  temp_gbif_dir <- tempdir()  # Temporary directory for GBIF data
  
  download_gbif_data <- function() {
    req(input$gbif_picker)
    dataset <- gbif_datasets() |> filter(datasetKey == input$gbif_picker)
    
    gbif_zip <- file.path(temp_gbif_dir, "gbif_download.zip")
    tryCatch({
      download.file(url = dataset$endpoints[[1]][[1]]$url, gbif_zip, method = "curl")
      unzip(gbif_zip, exdir = temp_gbif_dir)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Import Data with Progress Bar
  observeEvent(input$import, {
    dir_path <- if (input$import_tabs == "From GBIF") temp_gbif_dir else input$dir_path
    req(dir_path)  # Ensure directory path is available
    
    progress <- Progress$new()  # Create a new progress object
    progress$set(message = "Importing Data...", value = 0)
    
    if (input$import_tabs == "From GBIF") {
      progress$inc(0.2, detail = "Downloading GBIF data...")
      
      success <- download_gbif_data()
      if (!success) {
        progress$close()
        output$import_status <- renderUI({
          tags$p("❌ Download Failed: Check GBIF URL or network connection.", 
                 style = "color: red; font-weight: bold;")
        })
        return()
      }
    }
    
    progress$inc(0.3, detail = "Reading JSON file...")
    
    # Run async process
    data_future <- future({
      tryCatch({
        file_path <- file.path(dir_path, "datapackage.json")
        if (!file.exists(file_path)) stop("datapackage.json not found.")
        
        camtraptor::read_camtrap_dp(file_path)  # Read data
        
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # After future() completes
    data_future %...>% (function(ctdp) {
      progress$inc(0.6, detail = "Processing data...")
      
      if (is.null(ctdp)) {
        output$import_status <- renderUI({
          tags$p("❌ Import Failed: Check JSON structure or permissions.", 
                 style = "color: red; font-weight: bold;")
        })
      } else {
        filtered_data(NULL)  # Clear any previous filters
        imported_data(ctdp)  # Store data
        
        progress$inc(0.9, detail = "Finalizing import...")
        
        output$import_status <- renderUI({
          tags$p("✅ Import Successful!", style = "color: green; font-weight: bold;")
        })
      }
      
      progress$close()  # Close progress bar
      
    }) %...!% (function(err) {
      progress$close()
      output$import_status <- renderUI({
        tags$p(paste0("❌ Error: ", err$message), style = "color: red; font-weight: bold;")
      })
    })
  })
  
  # Display selected directory or GBIF temporary directory
  output$selected_dir <- renderText({
    if (input$import_tabs == "From GBIF") {
      paste("Using Temporary Directory:", temp_gbif_dir)
    } else if (input$dir_path != "") {
      paste("Selected Directory:", input$dir_path)
    } else {
      "No directory selected."
    }
  })
  
  # Data previews
  output$deployments_preview <- renderDataTable({
    datatable(
      head(imported_data()$data$deployments),
      options = list(
        paging = FALSE, 
        searching = FALSE,  
        info = FALSE
      )
    )
  })
  
  output$observations_preview <- renderDataTable({
    datatable(
      head(imported_data()$data$observations),
      options = list(
        paging = FALSE, 
        searching = FALSE,  
        info = FALSE
      )
    )
  })
  
  output$media_preview <- renderDataTable({
    datatable(
      head(imported_data()$data$media),
      options = list(
        paging = FALSE, 
        searching = FALSE,  
        info = FALSE
      )
    )
  })
  
  ################
  # Data filters #
  ################
  filtered_data <- reactiveVal(NULL)
  filter_history <- reactiveValues(stack = list())
  selected_shape <- reactiveVal(NULL)
  
  current_data <- reactive({
    if (!is.null(filtered_data())) {
      message("✅ Using filtered data")
      return(filtered_data())
    } else if (!is.null(imported_data())) {
      message("✅ Using imported data (no filters applied)")
      return(imported_data())
    } else {
      message("⚠️ No data loaded yet")
      return(NULL)
    }
  })
  #---- Dynamic filter function ----#
  apply_dynamic_filters <- function(input, df, input_prefix = "filter_") {
    selected_cols <- input[[paste0(input_prefix, "columns")]]
    
    dynamic_filters <- purrr::map(selected_cols, function(col) {
      input_id <- paste0(input_prefix, col)
      val <- input[[input_id]]
      if (is.null(val)) return(NULL)
      
      col_data <- df[[col]]
      col_type <- class(col_data)[1]
      
      if (col_type %in% c("character", "factor")) {
        quo(.data[[!!col]] %in% !!val)
        
      } else if (col_type == "numeric") {
        quo(between(.data[[!!col]], !!val[1], !!val[2]))
        
      } else if (col_type == "logical") {
        quo(.data[[!!col]] == !!val)
        
      } else if (col_type %in% c("POSIXct", "Date")) {
        lower_time <- as.POSIXct(input[[paste0(input_prefix, "timestamp_lower")]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        upper_time <- as.POSIXct(input[[paste0(input_prefix, "timestamp_upper")]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        
        if (!is.na(lower_time) & !is.na(upper_time)) {
          quo(between(.data[[!!col]], !!lower_time, !!upper_time))
        } else {
          NULL
        }
        
      } else {
        NULL
      }
    }) |> purrr::compact()
    
    return(dynamic_filters)
  }
  
  generate_dynamic_filter_ui <- function(selected_columns, df, input_prefix = "filter_") {
    purrr::map(selected_columns, function(col) {
      input_id <- paste0(input_prefix, col)
      col_type <- class(df[[col]])[1]
      
      if (col_type %in% c("character", "factor")) {
        selectizeInput(input_id, label = paste("Filter", col, ":"), 
                       choices = unique(df[[col]]), multiple = TRUE)
        
      } else if (col_type == "numeric") {
        sliderInput(input_id, label = paste("Filter", col, ":"), 
                    min = min(df[[col]], na.rm = TRUE),
                    max = max(df[[col]], na.rm = TRUE), 
                    value = range(df[[col]], na.rm = TRUE))
        
      } else if (col_type == "logical") {
        checkboxInput(input_id, label = paste("Filter", col, ":"), value = TRUE)
        
      } else if (col_type %in% c("POSIXct", "Date")) {
        tagList(
          textInput(paste0(input_id, "_lower"), label = paste("Start", col, ":"),
                    value = format(min(df[[col]], na.rm = TRUE), "%Y-%m-%d %H:%M:%S")),
          textInput(paste0(input_id, "_upper"), label = paste("End", col, ":"),
                    value = format(max(df[[col]], na.rm = TRUE), "%Y-%m-%d %H:%M:%S"))
        )
      } else {
        NULL
      }
    })
  }
  
  # Deployments
  filtered_deployments <- reactive({
    req(current_data())
    df <- current_data()$data$deployments
    dat_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    
    # Static filters
    static_filters <- list(
      if (!is.null(input$date_range)) {
        quo(int_overlaps(
          interval(as.Date(input$date_range[1]), as.Date(input$date_range[2])),
          interval(start, end)))
      }
    ) |> purrr::compact()
    
    # Dynamic filters
    dynamic_filters <- apply_dynamic_filters(input, df, "filter_deployment_")
    
    # Apply filters
    all_filters <- c(static_filters, dynamic_filters)
    df <- df |> dplyr::filter(!!!all_filters)
    
    if (!is.null(selected_shape())) {
      df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
      inside <- lengths(st_within(df_sf, selected_shape())) > 0
      df <- df[inside, ]
    }
    return(df)
  })
  
  output$dynamic_deployment_filters_ui <- renderUI({
    req(current_data(), input$filter_deployment_columns)
    df <- current_data()$data$deployments
    generate_dynamic_filter_ui(input$filter_deployment_columns, df, "filter_deployment_")
  })
  
  # ---- All filters in the map sidebar ---- #
  output$map_filter_ui <- renderUI({
    req(current_data())
    df <- current_data()$data$deployments
    
    lat_range <- range(df$latitude, na.rm = TRUE)
    lng_range <- range(df$longitude, na.rm = TRUE)
    
    layout_sidebar(
      class = "p-0",
      sidebar = sidebar(
        title = "Filters",
        position = "right",
        width = 500,
        airDatepickerInput("date_range", "Deployment period:",
                           range = TRUE,
                           value = c(current_data()$temporal$start, 
                                     current_data()$temporal$end),
                           placeholder = "Select a deployment period"),
        br(),
        pickerInput(
          inputId = "filter_deployment_columns",
          label = "Select columns to filter by:",
          choices = c("deploymentID", "locationID", "locationName",
                      "longitude", "latitude", "coordinateUncertainty", 
                      "setupBy", "cameraID", "cameraModel", "cameraInterval", 
                      "cameraHeight", "cameraTilt", "cameraHeading", 
                      "timestampIssues", "baitUse", "session", "array", 
                      "featureType", "habitat", "tags", "comments"),
          multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        br(),
        uiOutput("dynamic_deployment_filters_ui"),
        actionButton("apply_dep_filters", "Apply Filters"),
        br(),
        fluidRow(
          column(actionButton("reset_last_filter", "Reset Last Filter", class = "btn-warning"), 
                 width = 6),
          column(actionButton("reset_all_filters", "Reset All Filters", class = "btn-danger"), 
                 width = 6)
        ),
        uiOutput("deployments_filter_status")
      ),
      leafletOutput("filtered_map", height = "600px")
    )
  })
  
  output$filtered_map <- renderLeaflet({
    req(imported_data())
    
    df <- imported_data()$data$deployments
    df <- df |> filter(!is.na(longitude), !is.na(latitude))  # clean coords
    
    lngs <- range(df$longitude, na.rm = TRUE)
    lats <- range(df$latitude, na.rm = TRUE)
    
    leaflet() |>
      addTiles() |>
      fitBounds(lng1 = lngs[1], lat1 = lats[1], lng2 = lngs[2], lat2 = lats[2]) |>
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions()),
        polygonOptions = drawPolygonOptions(shapeOptions = drawShapeOptions()),
        editOptions = editToolbarOptions(edit = FALSE)
      )
  })
  
  # ---- Leaflet map updates only based on filtered data ---- #
  observe({
    deployments <- filtered_deployments() |> filter(!is.na(longitude), !is.na(latitude))
    if (nrow(deployments) == 0) {
      leafletProxy("filtered_map") |> clearMarkers()
      return()
    }
    
    leafletProxy("filtered_map") |>
      clearMarkers() |>
      addCircleMarkers(
        data = deployments,
        lng = ~longitude,
        lat = ~latitude,
        color = "red",
        fillOpacity = 0.8,
        radius = 6,
        label = ~locationID
      )
  })
  
  # ---- Filter circleMarkers ---- #
  observeEvent(input$filtered_map_draw_new_feature, {
    coords <- input$filtered_map_draw_new_feature$geometry$coordinates[[1]]
    lngs <- sapply(coords, `[[`, 1)
    lats <- sapply(coords, `[[`, 2)
    polygon <- st_polygon(list(cbind(lngs, lats))) |> st_sfc(crs = 4326)
    selected_shape(polygon)
  })
  
  # ---- Reset shape on delete ---- #
  observeEvent(input$filtered_map_draw_deleted_features, {
    selected_shape(NULL)
  })
  
  # ---- Update imported data after 'apply filter' click ---- #
  observeEvent(input$apply_dep_filters, {
    req(filtered_deployments(), current_data())
    DATA <- current_data()
    DATA$data$deployments <- filtered_deployments()
    
    filtered_data(DATA)
    
    output$deployments_filter_status <- renderUI({
      tags$p("✅ Deployment filters successfully applied!", style = "color: green; font-weight: bold;")
    })
  })
  
  # Media
  filtered_media <- reactive({
    req(current_data())
    df <- current_data()$data$media
    
    # Only dynamic filters
    all_filters <- apply_dynamic_filters(input, df, "filter_media_")
    
    # Apply filters
    df <- df |> dplyr::filter(!!!all_filters)
    return(df)
  })
  
  output$dynamic_media_filters_ui <- renderUI({
    req(current_data(), input$filter_media_columns)
    df <- current_data()$data$media
    generate_dynamic_filter_ui(input$filter_media_columns, df, "filter_media_")
  })
  
  # ---- Update imported data after 'apply filter' click ---- #
  observeEvent(input$apply_med_filters, {
    req(filtered_media(), current_data())
    DATA <- current_data()
    DATA$data$media <- filtered_media()
    
    # Save current state to history
    filter_history$stack <- append(filter_history$stack, list(current_data()))
    
    filtered_data(DATA)
    
    output$media_filter_status <- renderUI({
      tags$p("✅ Media filters successfully applied!", style = "color: green; font-weight: bold;")
    })
  })
  
  
  # Observations
  filtered_observations <- reactive({
    req(current_data())
    df <- current_data()$data$observations
    
    # Only dynamic filters
    all_filters <- apply_dynamic_filters(input, df, "filter_observation_")
    
    # Apply filters
    df <- df |> dplyr::filter(!!!all_filters)
    return(df)
  })
  
  output$dynamic_observation_filters_ui <- renderUI({
    req(current_data(), input$filter_observation_columns)
    df <- current_data()$data$observations
    generate_dynamic_filter_ui(input$filter_observation_columns, df, "filter_observation_")
  })
  
  # ---- Update imported data after 'apply filter' click ---- #
  observeEvent(input$apply_obs_filters, {
    req(filtered_observations(), current_data())
    DATA <- current_data()
    DATA$data$observations <- filtered_observations()
    
    # Save current state to history
    filter_history$stack <- append(filter_history$stack, list(current_data()))
    
    filtered_data(DATA)
    
    output$observations_filter_status <- renderUI({
      tags$p("✅ Observation filters successfully applied!", style = "color: green; font-weight: bold;")
    })
  })
  
  update_filter_status <- function(msg, color = "black", tab_id = NULL) {
    tab_id <- tab_id %||% isolate(input$filter_tab)  # Use input$filter_tab only if not explicitly passed
    
    status_ui <- tags$p(msg, style = paste("color:", color, "; font-weight: bold;"))
    
    if (tab_id == "Deployments") {
      output$deployments_filter_status <- renderUI({ status_ui })
    } else if (tab_id == "Media") {
      output$media_filter_status <- renderUI({ status_ui })
    } else if (tab_id == "Observations") {
      output$observations_filter_status <- renderUI({ status_ui })
    } else {
      print("⚠️ Unknown tab in update_filter_status()")
    }
  }
  
  # Reset last filter
  observeEvent(input$reset_last_filter, {
    if (length(filter_history$stack) > 0) {
      last <- tail(filter_history$stack, 1)[[1]]
      filter_history$stack <- head(filter_history$stack, -1)
      filtered_data(last)
      
      update_filter_status("↩️ Last filter reset.", color = "orange")
    }
  })
  
  # Reset all filters
  observeEvent(input$reset_all_filters, {
    filter_history$stack <- list()
    filtered_data(NULL)
    
    update_filter_status("🔄 All filters reset.", color = "red")
  })
  
  #######################
  # Data Explorer Tab #
  #######################
  output$deployments_table <- renderDataTable({
    datatable(
      as.data.frame(current_data()$data$deployments),
      extensions = 'Buttons',
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        paging = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      ),
      filter = 'top'
    )
  })
  
  output$observations_table <- renderDataTable({
    datatable(
      as.data.frame(current_data()$data$observations),
      extensions = 'Buttons',
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        paging = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      ),
      filter = 'top'
    )
  })
  
  output$media_table <- renderDataTable({
    datatable(
      as.data.frame(current_data()$data$media),
      extensions = 'Buttons',
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        paging = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10
      ),
      filter = 'top'
    )
  })
  
  ######################
  #                    #
  ######################
  ### ==== REACTIVE VALUE CACHES ====
  obs_table_cache     <- reactiveVal(NULL)
  effort_table_cache  <- reactiveVal(NULL)
  rec_table_cache     <- reactiveVal(NULL)
  feat_data_cache     <- reactiveVal(NULL)
  ts_data_cache       <- reactiveVal(NULL)
  effort_data_cache   <- reactiveVal(NULL)
  
  ### ==== OBSERVE TAB ENTRY TO FILL CACHES ====
  
  # Observations summary
  observeEvent(list(main_tab(), current_data()), {
    req(main_tab())
    if (main_tab() == "visualize") {
      if (is.null(obs_table_cache()) || !identical(attr(obs_table_cache(), "source"), current_data())) {
        data <- n_observations(current_data(), species = "all", group_by = "day")
        attr(data, "source") <- current_data()
        obs_table_cache(data)
      }
    }
  })
  
  # Effort summary
  observeEvent(list(main_tab(), current_data()), {
    req(main_tab())
    if (main_tab() == "visualize") {
      if (is.null(effort_table_cache()) || !identical(attr(effort_table_cache(), "source"), current_data())) {
        data <- get_custom_effort2(add_location_key(current_data()), group_by = "day", unit = "day")
        attr(data, "source") <- current_data()
        effort_table_cache(data)
      }
    }
  })
  
  # Cleaned record table
  observeEvent(list(main_tab(), current_data()), {
    req(main_tab())
    if (main_tab() == "visualize") {
      if (is.null(rec_table_cache()) || !identical(attr(rec_table_cache(), "source"), current_data())) {
        data <- camtrapDensity::subset_deployments(
          current_data(), !is.na(deploymentID) & !is.na(locationID)
          & !is.na(start) & !is.na(end) & !is.na(longitude) & !is.na(latitude)
        )
        data$data$observations <- data$data$observations |>
          filter(if_all(c(observationID, deploymentID, sequenceID, timestamp, scientificName), ~!is.na(.x)))
        result <- get_record_table(data)
        attr(result, "source") <- current_data()
        rec_table_cache(result)
      }
    }
  })
  
  # Feature data (obs + effort + RAI)
  observeEvent(list(
    main_tab(),
    current_data(),
    obs_table_cache(),
    effort_table_cache()
  ), {
    req(main_tab(), obs_table_cache(), effort_table_cache())
    
    if (main_tab() == "visualize") {
      if (is.null(feat_data_cache()) || 
          !identical(attr(feat_data_cache(), "source"), current_data())) {
        
        # Use cached observations and effort data
        obs <- obs_table_cache()
        eff <- effort_table_cache()
        
        feat_table <- obs |>
          left_join(eff, by = c("deploymentID", "begin")) |>
          filter(!is.na(locationKey)) |>
          group_by(date = begin, locationKey, scientificName) |>
          summarize(
            n_obs = sum(n_obs, na.rm = TRUE),
            n_ind = sum(n_ind, na.rm = TRUE),
            effort = sum(effort, na.rm = TRUE),
            .groups = "drop"
          ) |>
          mutate(
            rai_obs = ifelse(effort > 0, n_obs / effort, 0),
            rai_ind = ifelse(effort > 0, n_ind / effort, 0)
          )
        
        loc_coords <- add_location_key(current_data())$data$deployments |>
          filter(!is.na(locationKey)) |>
          group_by(locationKey) |>
          summarize(
            longitude = mean(longitude, na.rm = TRUE),
            latitude  = mean(latitude,  na.rm = TRUE),
            .groups = "drop"
          )
        
        result <- feat_table |>
          left_join(loc_coords, by = "locationKey") |>
          select(date, locationKey, longitude, latitude, scientificName, n_obs:rai_ind)
        
        attr(result, "source") <- current_data()
        feat_data_cache(result)
      }
    }
  })
  
  # Time series by species
  observeEvent(feat_data_cache(), {
    if (!is.null(feat_data_cache())) {
      ts <- feat_data_cache() |>
        group_by(date, scientificName) |>
        summarize(
          n_obs = sum(n_obs, na.rm = TRUE),
          n_ind = sum(n_ind, na.rm = TRUE),
          effort = sum(effort, na.rm = TRUE),
          rai_obs = mean(rai_obs, na.rm = TRUE),
          rai_ind = mean(rai_ind, na.rm = TRUE),
          .groups = "drop"
        )
      ts_data_cache(ts)
    }
  })
  
  # Effort summarized by date/location
  observeEvent(effort_table_cache(), {
    if (!is.null(effort_table_cache())) {
      eff_data <- effort_table_cache() |>
        group_by(date = begin, locationKey) |>
        summarize(effort = sum(effort, na.rm = TRUE), .groups = "drop")
      effort_data_cache(eff_data)
    }
  })
  
  ### ==== PUBLIC REACTIVES (READ ONLY) ====
  
  obs_table    <- reactive({ req(obs_table_cache()); obs_table_cache() })
  effort_table <- reactive({ req(effort_table_cache()); effort_table_cache() })
  rec_table    <- reactive({ req(rec_table_cache()); rec_table_cache() })
  feat_data    <- reactive({ req(feat_data_cache()); feat_data_cache() })
  ts_data      <- reactive({ req(ts_data_cache()); ts_data_cache() })
  effort_data  <- reactive({ req(effort_data_cache()); effort_data_cache() })
  
  observe({
    req(current_data())
    req(main_tab() == "visualize")
    
    species <- sort(unique(current_data()$data$observations$scientificName))
    updatePickerInput(
      session,
      "selected_species",
      choices = species,
      selected = species[1] 
    )
    
    date_range <- range(c(
      current_data()$data$deployments$start, 
      current_data()$data$deployments$end
    ), na.rm = TRUE)
    updateAirDateInput(
      session,
      "selected_daterange",
      value = date_range
    )
  })
  
  ######################
  # Camera trap effort #
  ######################
  # Prepare line data
  reactive_effort_linechart_data <- reactive({
    req(input$vis_tab == "Effort")
    req(input$effort_group_by %in% c("day", "week", "month"))
    req(input$selected_daterange)
    req(effort_data())
    
    effort_data() |>
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      group_by(date = floor_date(date, input$effort_group_by)) |>
      summarize(effort = sum(effort), .groups = "drop") |>
      mutate(x = as.numeric(as.POSIXct(date, tz = "UTC")) * 1000) |>
      rename(y = effort) |>
      select(x, y)
  })
  
  # Prepare boxplot data
  reactive_effort_boxplot_data <- reactive({
    req(input$vis_tab == "Effort")
    req(input$effort_group_by %in% c("week", "month"))
    req(input$selected_daterange)
    req(effort_data())
    
    effort_data() |>
      filter(
        effort > 0,
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      group_by(date = floor_date(date, input$effort_group_by), locationKey) |>
      summarize(effort = sum(effort)) |>
      group_by(date) |>
      summarize(
        min = min(effort),
        Q1 = quantile(effort, 0.25),
        median = median(effort),
        Q3 = quantile(effort, 0.75),
        max = max(effort)
      ) |>
      mutate(date = as.numeric(as.POSIXct(date, tz = "UTC")) * 1000) |>
      select(date, min, Q1, median, Q3, max)
  })
  
  # Reactive value to track the selected chart type
  chart_type_effort <- reactiveVal("line") 
  
  observe({
    req(chart_type_effort())
    
    group_bys <- c("Day" = "day", "Week" = "week", "Month" = "month")
    
    if (chart_type_effort() == "boxplot") {
      group_bys <- group_bys[-1]
    }
    
    updateSelectInput(
      session,
      "effort_group_by",
      choices = group_bys,
      selected = "month" 
    )
  })
  
  # Observe button click to toggle chart type and update label
  observeEvent(input$toggle_chart_effort, {
    new_type <- ifelse(chart_type_effort() == "line", "boxplot", "line")
    new_label <- ifelse(new_type == "boxplot", "Switch to Linechart", "Switch to Boxplot")
    
    chart_type_effort(new_type) 
    updateActionButton(session, "toggle_chart_effort", label = new_label)
  })
  
  # Highchart with boxplot series
  output$effort_chart <- renderHighchart({
    req(input$vis_tab == "Effort")
    req(reactive_effort_boxplot_data())
    req(reactive_effort_linechart_data())
    req(chart_type_effort())
    
    hc <- highchart(type = "stock") %>%
      hc_title(text = "Monitoring Effort (Days)") %>%
      hc_xAxis(type = "datetime") %>%
      hc_yAxis(
        title = list(text = "Days of Monitoring"),
        labels = list(enabled = TRUE),
        opposite = FALSE
      ) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_tooltip(
        shared = FALSE,
        useHTML = TRUE,
        formatter = JS("
          function() {
            if (this.series.type === 'boxplot') {
              return '<b>' + Highcharts.dateFormat('%b %Y', this.x) + '</b><br/>' +
                     'Min: ' + this.point.low + '<br/>' +
                     'Q1: ' + this.point.q1 + '<br/>' +
                     'Median: ' + this.point.median + '<br/>' +
                     'Q3: ' + this.point.q3 + '<br/>' +
                     'Max: ' + this.point.high;
            } else {
              return '<b>' + Highcharts.dateFormat('%b %Y', this.x) + '</b><br/>' +
                     'Effort: ' + this.y;
            }
          }
        ")
      )
    
    if (chart_type_effort() == "boxplot") {
      req(reactive_effort_boxplot_data())
      hc <- hc %>%
        hc_add_series(
          name = "Effort Distribution",
          type = "boxplot",
          data = list_parse2(reactive_effort_boxplot_data()),
          color = "#1f77b4",
          showInNavigator = TRUE
        )
    } else {
      req(reactive_effort_linechart_data())
      hc <- hc %>%
        hc_add_series(
          name = "Effort (Days)",
          type = "line",
          data = list_parse2(reactive_effort_linechart_data()),
          color = "#1f77b4",
          showInNavigator = TRUE
        )
    }
    return(hc)
  })
  
  # Reactive map data
  reactive_effort_map_data <- reactive({
    req(input$vis_tab == "Effort")
    req(input$selected_daterange)
    req(feat_data())
    
    feat_data() |> 
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      group_by(locationKey) |>
      summarize(
        across(c("effort"), ~sum(.x, na.rm = T)),
        across(c("longitude", "latitude"), ~mean(.x, na.rm = T))
      ) |>
      ungroup() |>
      rename(n = effort) |>
      select(locationKey, longitude, latitude, n)
  })
  
  reactive_effort_data <- reactive({
    req(input$selected_daterange)
    req(feat_data())
    feat_data() |> 
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      arrange(locationKey, date) |>
      group_by(locationKey) |>
      mutate(date_diff = as.integer(date - lag(date, default = first(date)))) |>
      mutate(active_period_id = cumsum(date_diff > 1) + 1) |> 
      group_by(locationKey, active_period_id) |>
      summarise(start_date = as.Date(min(date)), end_date = as.Date(max(date)), .groups = "drop")
  })
  
  output$effort_map <- renderLeaflet({ 
    req(reactive_effort_map_data())
    my_map(
      reactive_effort_map_data(), 
      input, "effort_map", cols, update = FALSE
    ) |> addMiniMap() 
  })
  
  ########################
  # Species observations #
  ########################
  reactive_lab <- reactive({
    req(input$selected_feature)
    feature_convert <- c("n_obs", "n_individuals", "rai", "rai_individuals")
    names(feature_convert) <- feature_choices <- c("n_obs", "n_ind", "rai_obs", "rai_ind")
    title <- camtraptor:::get_legend_title(feature_convert[input$selected_feature])
  })
  
  slice(count(camtrapdp::example_dataset()$data$observations, scientificName, sort = TRUE), 1:10)
  
  top10 <- reactive({ 
    req(current_data())
    current_data()$data$observations |> 
      filter(!is.na(scientificName)) |>
      count(scientificName, sort = TRUE) |>
      slice(1:10) |>
      pull(scientificName)
  })
  
  reactive_top10 <- reactive({
    req(
      input$vis_tab == "Species Observations",
      top10(),
      rec_table(), 
      effort_table(),
      input$selected_daterange, 
      input$selected_species, 
      input$selected_feature
    )
    
    species <- unique(c(input$selected_species, top10()))
    top10_rec <-
      rec_table() |>
      filter(
        Date >= input$selected_daterange[1], 
        Date <= input$selected_daterange[2],
        Species %in% species
      ) |>
      group_by(Species) |>
      summarize(n_obs = n(), n_ind = sum(n, na.rm = T)) |> 
      arrange(desc(n_obs))
    
    effort <- effort_table() |>
      filter(
        begin >= input$selected_daterange[1], 
        begin <= input$selected_daterange[2]
      ) %>%
      pull(effort) %>%
      sum(na.rm = TRUE)
    
    top10_rec <- top10_rec |> 
      mutate(rai_obs = n_obs/effort, rai_ind = n_ind/effort)
    
    top10_rec |>
      select(Species, sym(input$selected_feature)) |>
      rename(n = !!sym(input$selected_feature)) |>
      mutate(n = round(n, 4)) |>
      arrange(desc(n))
  })
  
  output$barchart <- renderHighchart({
    req(
      input$vis_tab == "Species Observations",
      reactive_top10(), 
      input$selected_species, 
      input$selected_daterange, 
      input$selected_feature
    )
    
    n_selected <- length(input$selected_species)
    
    data <- reactive_top10()
    data$cat <- ifelse(data$Species %in% input$selected_species, data$Species, "Not selected")
    data$cat <- factor(data$cat, levels = c(input$selected_species, "Not selected"))
    my_cols <- c(cols[1:n_selected], col_other)
    
    validate(need(nrow(data) > 0, "No data available for the selected species."))
    
    data |>
      mutate(Species = factor(Species) |> fct_reorder(n, .desc = TRUE)) |>
      hchart(
        type = "bar",
        hcaes(x = as.numeric(Species), y = n, group = cat, name = Species),
        color = my_cols
      ) |>
      hc_xAxis(title = list(text = "Top 10 + selected species"), type = "category") |> 
      hc_yAxis(title = list(text = reactive_lab()), stackLabels = list(enabled = TRUE)) |>
      hc_plotOptions(series = list(stacking = "normal")) |>
      hc_title(text = "Total observations by species") |> 
      hc_legend(enabled = FALSE) |>
      hc_exporting(enabled = TRUE)
  })
  
  
  # Reactive value for chart type
  chart_type_species <- reactiveVal("line")
  
  observe({
    req(chart_type_species())
    
    group_bys <- c("Day" = "day", "Week" = "week", "Month" = "month")
    
    if (chart_type_species() == "boxplot") {
      group_bys <- group_bys[-1]
    }
    
    updateSelectInput(
      session,
      "species_group_by",
      choices = group_bys,
      selected = "month" 
    )
  })
  
  # Reactive data for time series
  reactive_linechart_data <- reactive({
    req(input$vis_tab == "Species Observations")
    req(ts_data()) 
    req(input$selected_species)
    req(input$selected_daterange)
    req(input$selected_feature)
    req(input$species_group_by)
    
    ts <- ts_data() |>
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2],
        scientificName %in% input$selected_species
      ) |>
      mutate(scientificName = factor(
        scientificName, levels = input$selected_species)) |>
      group_by(date = floor_date(date, input$species_group_by), scientificName) |>
      summarize(n = sum(!!sym(input$selected_feature)), .groups = "drop")
    
    lapply(1:length(input$selected_species), function(i) {
      s <- input$selected_species[i]
      list(
        name = s,
        data = filter(ts, scientificName == s),
        color = cols[i]
      )
    })
  })
  
  # Reactive dropdown for boxplot species selection (appears only in boxplot mode)
  output$boxplot_species_selector <- renderUI({
    if (chart_type_species() == "boxplot") {
      fluidRow(
        div(
          style = "display: flex; align-items: center; padding-left: 20px;
                   min-width: 250px; height: 40px;",
          tags$label("Select a species:", style = "margin-right: 10px;"), 
          selectInput("selected_species_boxplot", choices = input$selected_species,
                      selected = input$selected_species[1], 
                      label = NULL, multiple = FALSE)
        )
      )
    }
  })
  
  # Reactive boxplot data for selected species
  reactive_boxplot_data <- reactive({
    req(input$vis_tab == "Species Observations")
    req(feat_data()) 
    req(input$selected_species)
    req(input$selected_daterange)
    req(input$selected_feature)
    req(input$species_group_by)
    
    s <- input$selected_species_boxplot
    col <- cols[which(input$selected_species == s)]
    data <- feat_data() |>
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2],
        scientificName == s, 
        effort > 0
      ) |>
      group_by(date = floor_date(date, input$species_group_by), locationKey) |>
      summarize(
        across(c("n_obs", "n_ind", "effort"), ~sum(.x, na.rm = T)),
        across(c("rai_obs", "rai_ind"), ~mean(.x, na.rm = T))
      ) |>
      rename(n = !!sym(input$selected_feature)) |>
      group_by(date) |>
      summarize(
        min = min(n),
        Q1 = quantile(n, 0.25),
        median = median(n),
        Q3 = quantile(n, 0.75),
        max = max(n),
        .groups = "drop"
      ) |>
      mutate(date = as.numeric(as.POSIXct(date, tz = "UTC")) * 1000) |>
      select(date, min, Q1, median, Q3, max)
    
    list(
      name = s,
      data = data,
      color = col
    )
  })
  
  # Observe button click to toggle between linechart and boxplot
  observeEvent(input$toggle_chart_species, {
    new_type <- ifelse(chart_type_species() == "line", "boxplot", "line")
    new_label <- ifelse(new_type == "boxplot", "Switch to Linechart", "Switch to Boxplot")
    
    chart_type_species(new_type)
    updateActionButton(session, "toggle_chart_species", label = new_label)
  })
  
  # Render dynamic Highchart
  output$species_chart <- renderHighchart({
    req(input$vis_tab == "Species Observations")
    req(reactive_boxplot_data())
    req(reactive_linechart_data())
    req(chart_type_species())
    
    hc <- highchart(type = "stock") |>
      hc_xAxis(type = "datetime") |>
      hc_yAxis(title = list(text = reactive_lab()), labels = list(enabled = TRUE), opposite = FALSE) |>
      hc_exporting(enabled = TRUE)
    
    if (chart_type_species() == "line") {
      # Render Line Chart with all selected species
      data <- reactive_linechart_data()
      for (series_info in data) {
        hc <- hc |>
          hc_add_series(
            name = series_info$name,
            data = list_parse2(
              data.frame(
                x = as.numeric(as.POSIXct(series_info$data$date, tz = "UTC")) * 1000,
                y = series_info$data$n
              )
            ),
            type = "line",
            color = series_info$color,
            showInNavigator = TRUE
          )
      }
    } else {
      # Render Boxplot for the selected species
      req(input$selected_species_boxplot)  # Ensure a species is selected
      series_info <- reactive_boxplot_data()
      
      hc <- hc |>
        hc_add_series(
          name = series_info$name,
          type = "boxplot",
          data = list_parse2(series_info$data),
          color = series_info$color,
          showInNavigator = TRUE
        )
    }
    
    return(hc)
  })
  
  # Reactive value for chart type
  map_type_species <- reactiveVal("map")
  
  # Reactive dropdown for boxplot species selection (appears only in boxplot mode)
  output$map_species_selector <- renderUI({
    if (map_type_species() == "map") {
      fluidRow(
        div(
          style = "display: flex; align-items: center; padding-left: 20px;
                   min-width: 250px; height: 40px;",
          tags$label("Select a species:", style = "margin-right: 10px;"), 
          selectInput("selected_species_map", choices = input$selected_species,
                      selected = input$selected_species[1], 
                      label = NULL, multiple = FALSE)
        )
      )
    }
  })
  
  # Observe button click to toggle between linechart and boxplot
  observeEvent(input$toggle_map_species, {
    new_type <- ifelse(map_type_species() == "map", "pie", "map")
    new_label <- ifelse(new_type == "pie", "Switch to Map", "Switch to Map + Pie Charts")
    
    map_type_species(new_type)
    updateActionButton(session, "toggle_map_species", label = new_label)
  })
  
  # Reactive map data
  reactive_map_data <- reactive({
    req(feat_data())
    req(input$selected_daterange)
    req(input$selected_feature)
    
    feat_data() |> 
      filter(
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      group_by(locationKey, scientificName) |>
      summarize(
        across(c("n_obs", "n_ind", "effort"), ~sum(.x, na.rm = T)),
        across(c("rai_obs", "rai_ind", "longitude", "latitude"), ~mean(.x, na.rm = T))
      ) |>
      ungroup() |>
      rename(n = !!sym(input$selected_feature)) |> 
      select(scientificName, locationKey, longitude, latitude, n)
  })
  
  reactive_popup_data <- reactive({
    req(feat_data())
    req(input$selected_daterange)
    req(input$selected_feature)
    req(input$selected_species)
    
    s <- input$selected_species
    if (map_type_species() == "map") {
      req(input$selected_species_map)
      s <- input$selected_species_map
    }
    
    data <- feat_data() |> 
      filter(
        scientificName %in% s,
        date >= input$selected_daterange[1], 
        date <= input$selected_daterange[2]
      ) |>
      rename(n = !!sym(input$selected_feature)) |> 
      select(scientificName, date, locationKey, longitude, latitude, n)
  })
  
  # Reactive value to store the clicked location
  clicked_location <- reactiveVal(NULL)
  
  # Observe map clicks and update the selected location
  observeEvent(input$map_marker_click, {
    loc <- input$map_marker_click$id  # Get clicked location ID
    clicked_location(loc)  # Store location in reactive value
    
    # Ensure location is valid before generating plot
    req(loc)
    
    # Filter data for clicked location
    location_data <- reactive_popup_data() |> filter(locationKey == loc)
    location_effort <- reactive_effort_data() |> filter(locationKey == loc)
    
    # Ensure there is data for the selected location
    req(nrow(location_data) > 0)
    
    # **Manually Assign Active Period IDs to Observations**
    location_data <- location_data |>
      rowwise() |>
      mutate(
        active_period_id = location_effort |>
          filter(date >= start_date & date <= end_date) |>
          pull(active_period_id) |>
          first()  # Take the first match (or NA if no match)
      ) |>
      ungroup() |>
      filter(!is.na(active_period_id))  # Keep only matched rows
    
    
    s <- unique(location_data$scientificName)
    if (length(s) == 1) {
      vals <- cols[which(unique(location_data$scientificName) == input$selected_species)]
    } else {
      vals <- cols[rank(unique(location_data$scientificName))]
    }
    
    output$popup_plot <- renderPlot({
      ggplot() +
        geom_segment(data = location_data, aes(
          x = date, xend = date, y = 0, yend = n, 
          color = scientificName), size = 1) +
        labs(x = "Date", y = reactive_lab()) +
        scale_color_manual(values = vals) +
        scale_x_date(labels = scales::label_date("%Y-%m-%d")) +
        facet_grid(scientificName~active_period_id, scales = "free_x",
                   labeller = labeller(active_period_id = function(x) paste0("Deployment ", x))) + 
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.spacing = unit(2, "lines"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          strip.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1))
    }, height = 600)
    
    # Download handler for the plot
    output$download_plot <- downloadHandler(
      filename = function() { paste0("plot_", loc, ".png") },
      content = function(file) {
        ggsave(file, plot = last_plot(), device = "png", width = 8, height = 6)
      }
    )
    
    # Show the popup dynamically when a location is clicked
    showModal(jqui_draggable(jqui_resizable(modalDialog(
      title = paste("Detailed Time Series for:", loc),
      fluidRow(
        column(12, plotOutput("popup_plot", height = "auto", width = "100%"))
      ),
      footer = tagList(
        downloadButton("download_plot", "Download Plot"),
        modalButton("Close")
      ),
      easyClose = TRUE,  # Allow closing by clicking outside
      size = "l"
    ))))
  })
  
  # Render dynamic Highchart
  output$map <- renderLeaflet({ my_basemap(reactive_map_data()) })
  observe({
    if (map_type_species() == "map") {
      req(input$selected_species_map, input$selected_daterange, input$selected_feature)
      my_map(reactive_map_data(), input, "map", cols)
    } else {
      req(input$selected_species, input$selected_daterange, input$selected_feature)
      my_pie_map(reactive_map_data(), input, "map", cols)
    }
  })
  
  # observe({
  #   updateSelectInput(session, "species_map2A", choices = input$selected_species)
  #   updateSelectInput(session, "species_map2B", choices = input$selected_species)
  # })
  # output$map2_ui <- renderUI({
  #   sync(
  #     my_map(reactive_map_data(), input, "map2A", cols, update = FALSE),
  #     my_map(reactive_map_data(), input, "map2B", cols, update = FALSE),
  #     no.initial.sync = TRUE
  #   )
  # })
  
  
  ####################
  # Species activity #
  ####################
  
  # Dynamically populate species and feature selectors
  reactive_act_data1 <- reactive({
    
    req(input$vis_tab == "Species Activity")
    req(input$selected_species)
    req(input$selected_daterange)
    
    data <- rec_table() |>
      filter(Species %in% input$selected_species,
             Date >= input$selected_daterange[1], 
             Date <= input$selected_daterange[2]
      )
    
    split_data <- split(data, data$Species)
    
    se.fit <- input$se1
    f <- function(x, se.fit) {
      if (se.fit) {
        fit <- fitact(x$solar, sample = "data", reps = 50)
      } else {
        fit <- fitact(x$solar)
      }
      as_tibble(fit@pdf)
    }
    
    f_acts <- lapply(split_data, f, se.fit)
    data_list <- lapply(1:length(input$selected_species), function(i) {
      s <- input$selected_species[i]
      list(
        name = names(f_acts)[i],
        data = f_acts[[s]],
        color = cols[i]
      )
    })
    return(data_list)
  })
  
  reactive_act_data2 <- reactive({
    
    req(input$vis_tab == "Species Activity")
    req(input$selected_species)
    req(input$selected_daterange)
    
    data <- rec_table() |>
      filter(Species %in% input$selected_species,
             Date >= input$selected_daterange[1], 
             Date <= input$selected_daterange[2]
      ) |>
      mutate(doy = 2 * pi * (yday(Date) / 366))
    
    split_data <- split(data, data$Species)
    
    se.fit <- input$se2
    f <- function(x, se.fit) {
      if (se.fit) {
        fit <- fitact(x$doy, sample = "data", reps = 50)
      } else {
        fit <- fitact(x$doy)
      }
      as_tibble(fit@pdf)
    }
    
    f_acts <- lapply(split_data, f, se.fit)
    data_list <- lapply(1:length(input$selected_species), function(i) {
      s <- input$selected_species[i]
      list(
        name = names(f_acts)[i],
        data = f_acts[[s]],
        color = cols[i]
      )
    })
    return(data_list)
  })
  
  output$areachart_daily <- renderHighchart({
    req(input$vis_tab == "Species Activity")
    req(reactive_act_data1())
    
    hc <- highchart() |>
      hc_chart(type = "arearange") |>
      hc_title(text = "Daily activity cycle") |>
      hc_xAxis(
        title = list(text = "Time of the day"),
        categories = seq(0, 2*pi, length.out = 513),
        tickPositions = c(0, pi/2, pi, 3*pi/2, 2*pi),
        labels = list(
          formatter = JS("
          function() {
            var pi = Math.PI;
            if (this.value === 0) return 'Midnight';
            if (Math.abs(this.value - pi / 2) < 1e-6) return 'Sunrise';
            if (Math.abs(this.value - pi) < 1e-6) return 'Midday';
            if (Math.abs(this.value - 3 * pi / 2) < 1e-6) return 'Sunset';
            if (Math.abs(this.value - 2 * pi) < 1e-6) return 'Midnight';
            return this.value.toFixed(2);
          }
        ")
        ),
        plotBands = list(
          list(
            from = 0,
            to = pi/2,
            color = "rgba(200, 200, 200, 0.5)",
            label = list(text = "Nighttime")
          ),
          list(
            from = pi/2,
            to = 3*pi/2,
            color = "rgba(200, 200, 200, 0)",
            label = list(text = "Daylight")
          ),
          list(
            from = 3*pi/2,
            to = 2*pi,
            color = "rgba(200, 200, 200, 0.5)",
            label = list(text = "Nighttime")
          )
        )
      ) |>
      hc_yAxis(title = list(text = ""))
    
    # Loop through the list and add series
    data_list <- reactive_act_data1()
    for (series_info in data_list) {
      if (input$se1 == TRUE) {
        # Add confidence interval (arearange)
        hc <- hc |>
          hc_add_series(
            name = paste(series_info$name, "Confidence Interval"),
            data = list_parse2(
              data.frame(
                x = series_info$data$x,
                low = series_info$data$lcl,
                high = series_info$data$ucl
              )
            ),
            type = "arearange",
            color = hex_to_rgba(series_info$color, 0.3) # Semi-transparent
          )
      }
      
      # Add mean prediction (line)
      hc <- hc |>
        hc_add_series(
          name = paste(series_info$name, "Mean Prediction"),
          data = list_parse2(
            data.frame(
              x = series_info$data$x,
              y = series_info$data$y
            )
          ),
          type = "line",
          color = series_info$color
        )
    }
    
    # Add tooltip
    hc <- hc |>
      hc_tooltip(
        shared = TRUE,
        valueDecimals = 2
      )
    
    # Render the chart
    hc |> hc_exporting(enabled = TRUE)
  })
  
  output$areachart_annual <- renderHighchart({
    req(input$vis_tab == "Species Activity")
    req(reactive_act_data2())
    
    hc <- highchart() |>
      hc_chart(type = "arearange") |>
      hc_title(text = "Annual activity cycle") |>
      hc_xAxis(
        title = list(text = "Time of the year"),
        categories = seq(0, 2*pi, length.out = 513),
        tickPositions = c(0, pi/2, pi, 3*pi/2, 2*pi),
        labels = list(
          formatter = JS("
          function() {
            var pi = Math.PI;
            if (this.value === 0) return '1 Jan.';
            if (Math.abs(this.value - pi / 2) < 1e-6) return '1 Apr.';
            if (Math.abs(this.value - pi) < 1e-6) return '1 Jul.';
            if (Math.abs(this.value - 3 * pi / 2) < 1e-6) return '1 Oct.';
            if (Math.abs(this.value - 2 * pi) < 1e-6) return '31 Dec.';
            return this.value.toFixed(2);
          }
        ")
        ),
        plotBands = list(
          list(
            from = 0,
            to = 2*pi*80/365,
            color = "rgba(200, 200, 200, 0.5)",
            label = list(text = "Winter")
          ),
          list(
            from = 2*pi*80/365,
            to = 2*pi*172/365,
            color = "rgba(200, 200, 200, 0)",
            label = list(text = "Spring")
          ),
          list(
            from = 2*pi*172/365,
            to = 2*pi*264/365,
            color = "rgba(200, 200, 200, 0.5)",
            label = list(text = "Summer")
          ),
          list(
            from = 2*pi*264/365,
            to = 2*pi*355/365,
            color = "rgba(200, 200, 200, 0)",
            label = list(text = "Autumn")
          ),
          list(
            from = 2*pi*355/365,
            to = 2*pi,
            color = "rgba(200, 200, 200, 0.5)",
            label = list(text = "")
          )
        )
      ) |>
      hc_yAxis(title = list(text = ""))
    
    # Loop through the list and add series
    data_list <- reactive_act_data2()
    for (series_info in data_list) {
      if (input$se2 == TRUE) {
        # Add confidence interval (arearange)
        hc <- hc |>
          hc_add_series(
            name = paste(series_info$name, "Confidence Interval"),
            data = list_parse2(
              data.frame(
                x = series_info$data$x,
                low = series_info$data$lcl,
                high = series_info$data$ucl
              )
            ),
            type = "arearange",
            color = hex_to_rgba(series_info$color, 0.3) # Semi-transparent
          )
      }
      
      # Add mean prediction (line)
      hc <- hc |>
        hc_add_series(
          name = paste(series_info$name, "Mean Prediction"),
          data = list_parse2(
            data.frame(
              x = series_info$data$x,
              y = series_info$data$y
            )
          ),
          type = "line",
          color = series_info$color
        )
    }
    
    # Add tooltip
    hc <- hc |>
      hc_tooltip(
        shared = TRUE,
        valueDecimals = 2
      )
    
    # Render the chart
    hc |> hc_exporting(enabled = TRUE)
  })
}
