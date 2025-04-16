ui <- page_navbar(
  id = "navbar",
  title = HTML("Camtrap DP Explorer <span style='font-size: 0.7em; font-weight: normal;'>(v0.1)</span>"),
  theme = bs_theme(brand = TRUE),
  
  ### 1. Top-level tabs -----------------------------------------
  nav_panel(
    "Import", value = "import", 
    useShinyjs(),
    # Import Options
    card(
      card_header("📂 Import Data"),
      card_body(
        tabsetPanel(
          id = "import_tabs",
          tabPanel("From Local Directory",
                   br(),
                   textInput("dir_path", "Enter directory path:", placeholder = "Select a folder..."),
                   # Horizontal Layout for Browse and Import Buttons
                   fluidRow(
                     column(6,
                            div(
                              style = "display: flex; gap: 10px;",
                              actionButton("browse", label = "Browse", 
                                           icon = icon("folder-open")),
                              
                              actionButton("import", label = "Import Data", 
                                           icon = icon("upload"))
                            )
                     )
                   ),
                   # Selected directory display
                   textOutput("selected_dir"),
                   # Import status message
                   uiOutput("import_status")
          ),
          tabPanel("From GBIF",
                   br(),
                   pickerInput("gbif_picker", "Select a dataset:", 
                               choices = NULL, multiple = FALSE, 
                               options = list(
                                 `live-search` = TRUE, 
                                 `placeholder` = "Search datasets...")),
                   br(),
                   htmlOutput("dataset_info"),
                   br(),
                   actionButton("import", label = "Import Data", 
                                icon = icon("upload")),
                   br(),
                   # Selected directory display
                   textOutput("selected_dir"),
                   # Import status message
                   uiOutput("import_status")
          )
        )
      )
    ),
    # Data Preview
    card(
      card_header("👀 Data Preview"),
      card_body(
        tabsetPanel(
          tabPanel("Deployments", dataTableOutput("deployments_preview")),
          tabPanel("Media", dataTableOutput("media_preview")),
          tabPanel("Observations", dataTableOutput("observations_preview"))
        )
      )
    )
  ),
  
  nav_panel(
    "Filter", value = "filter",
    card(
      card_header("🔍 Filter Data"),
      card_body(
        tabsetPanel(
          id = "filter_tab", 
          tabPanel("Deployments", p(
            h4("Deployment Filters",
               bslib::tooltip(
                 tags$span(class = "info-circle", bs_icon("info-circle", size = "0.7em")),
                 HTML(
                   "Media are filtered on associated <code>deploymentID</code>.<br>
                      Observations are filtered on associated <code>deploymentID</code>.<br>
                      Metadata <code>(x$spatial, x$temporal and x$taxonomic)</code> 
                     are updated to match the filtered deployments."
                 ),
                 placement = "bottom",
                 html = TRUE
               )
            ),
            uiOutput("map_filter_ui"),
            br(),
            # Filter status message
            uiOutput("deployments_filter_status")
          )),
          tabPanel("Media", p(
            fluidRow(
              tags$head(
                tags$style(HTML("
                  code {
                    color: #333333 !important;  /* Dark gray from Camtrap DP docs */
                    background-color: #f3f3f3 !important;  /* Light gray background */
                    padding: 2px 6px !important;
                    border-radius: 4px !important;
                    font-family: 'SFMono-Regular', Consolas, monospace !important;
                    border: 1px solid #e0e0e0 !important;
                  }
                  
                  .info-icon {
                    font-size: 50% !important;  /* 15% smaller */
                    vertical-align: middle;
                    margin-left: 0.3em;
                  }"))
              ),
              
              h4("Media Filters ",
                 bslib::tooltip(
                   tags$span(class = "info-circle", bs_icon("info-circle", size = "0.7em")),
                   HTML(
                     "Deployments are not filtered.<br>
                       Observations are filtered on <code>mediaID</code> 
                       and <code>eventID</code>.<br> Metadata is updated to match."
                   ),
                   placement = "bottom",
                   html = TRUE
                 )
              )
            ),
            pickerInput(
              inputId = "filter_media_columns",
              label = "Select columns to filter by:",
              choices = c("mediaID", "deploymentID", "sequenceID", "captureMethod", 
                          "timestamp", "filePath", "fileName", "fileMediatype", 
                          "exifData", "favourite", "comments"),
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE)
            ),
            br(),
            uiOutput("dynamic_media_filters_ui"),
            actionButton("apply_med_filters", 
                         label = "Apply Filters", 
                         icon = icon("filter")),
            br(), br(),
            actionButton("reset_last_filter", "Reset Last Filter", class = "btn-warning"),
            actionButton("reset_all_filters", "Reset All Filters", class = "btn-danger"),
            br(),
            # Filter status message
            uiOutput("media_filter_status")
          )),
          tabPanel("Observations", p(
            h4("Obeservation Filters",
               bslib::tooltip(
                 tags$span(class = "info-circle", bs_icon("info-circle", size = "0.7em")),
                 HTML(
                   "Deployments are not filtered.<br>
                      Media are filtered on associated <code>mediaID</code> (for media-based observations) and 
                      <code>eventID</code> (for event-based observations).<br> 
                      Filter on <code>observationLevel == 'media'</code> 
                      to only retain directly linked media.
                      Metadata <code>(x$taxonomic)</code> are updated to match the filtered obeservations."
                 ),
                 placement = "bottom",
                 html = TRUE
               )
            ),
            pickerInput(
              inputId = "filter_observation_columns",
              label = "Select columns to filter by:",
              choices = c(
                "observationID", "deploymentID", "sequenceID", "mediaID", 
                "timestamp", "observationType", "cameraSetup", "taxonID", 
                "taxonIDReference", "scientificName", "taxonRank", "count", 
                "countNew", "lifeStage", "sex", "behaviour", "individualID", 
                "speed", "radius", "angle", "classificationMethod", 
                "classifiedBy", "classificationTimestamp", 
                "classificationConfidence", "comments", 
                "vernacularNames.eng", "vernacularNames.nld"
              ),
              multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE)
            ),
            br(),
            uiOutput("dynamic_observation_filters_ui"),
            actionButton("apply_obs_filters", 
                         label = "Apply Filters", 
                         icon = icon("filter")),
            br(), br(),
            actionButton("reset_last_filter", "Reset Last Filter", class = "btn-warning"),
            actionButton("reset_all_filters", "Reset All Filters", class = "btn-danger"),
            br(),
            # Filter status message
            uiOutput("observations_filter_status")
          ))
        )
      )
    )
  ),
  
  nav_panel(
    "Explore", value = "explore",
    card(
      full_screen = TRUE,
      card_header("🗃 Explore Data"),
      card_body(
        tabsetPanel(
          tabPanel("Deployments", dataTableOutput("deployments_table")),
          tabPanel("Media", dataTableOutput("media_table")),
          tabPanel("Observations", dataTableOutput("observations_table"))
        )
      )
    )
  ),
  
  ### 2. Visualize navbar (with nested pages) -------------------
  nav_panel(
    "Visualize", value = "visualize",
    layout_sidebar(
      sidebar = sidebar(
        position = "left",
        width = 300,
        pickerInput(
          inputId = "selected_species",
          label = "Select Species:",
          choices = NULL, multiple = TRUE,
          options = list(`actions-box` = TRUE, `live-search` = TRUE)
        ),
        airDatepickerInput(
          inputId = "selected_daterange",
          label = "Select Date Range:",
          range = TRUE, value = NULL
        ),
        pickerInput(
          "selected_feature", "Select a feature:",
          choices = c("n_obs", "n_ind", "rai_obs", "rai_ind"),
          selected = "n_obs"
        )
      ),
      navset_tab(
        id = "vis_tab",
        # Effort Page
        nav_panel(
          "Effort",
          card(
            full_screen = TRUE,
            card_header("📈 Trend"),
            card_body(
              layout_sidebar(
                sidebar = sidebar(
                  position = "right",
                  width = 300,
                  open = FALSE,
                  actionButton("toggle_chart_effort", "Switch to Boxplot"),
                  selectInput("effort_group_by", label = "Group By", choices = NULL)
                ),
                highchartOutput("effort_chart", height = "300px")
              )
            )
          ),
          card(
            full_screen = TRUE,
            card_header("🗺 Map"),
            card_body(
              leafletOutput("effort_map", height = "600px")
            )
          )
        ),
        
        # Species Observations Page
        nav_panel(
          "Species Observations",
          layout_columns(
            col_widths = c(4, 8, 12),
            card(
              full_screen = TRUE,
              card_header("📊 Total Observations"),
              card_body(highchartOutput("barchart", height = "500px"))
            ),
            card(
              full_screen = TRUE,
              card_header("📈 Trend"),
              card_body(
                layout_sidebar(
                  sidebar = sidebar(
                    position = "right",
                    width = 300,
                    open = FALSE,
                    actionButton("toggle_chart_species", "Switch to Boxplot"),
                    selectInput("species_group_by", label = "Group By", choices = NULL),
                    uiOutput("boxplot_species_selector")
                  ),
                  highchartOutput("species_chart", height = "300px")
                )
              )
            ),
            
            card(
              full_screen = TRUE,
              card_header("🗺 Map"),
              card_body(
                layout_sidebar(
                  class = "p-0",
                  sidebar = sidebar(
                    position = "right",
                    width = 400,
                    open = FALSE,
                    actionButton("toggle_map_species", "Switch to Map + Pie Charts"),
                    uiOutput("map_species_selector")
                  ),
                  leafletOutput("map", height = "500px")
                )
              )
            )
          )
        ),
        
        # Species Activity Page
        nav_panel(
          "Species Activity",
          layout_columns(
            col_widths = c(6, 6),
            card(
              full_screen = TRUE,
              card_header("⏰ Daily Activity"),
              card_body(
                layout_sidebar(
                  sidebar = sidebar(
                    position = "right",
                    width = 300,
                    open = FALSE,
                    input_switch("se1", "Show Std. Errors", value = FALSE)
                  ),
                  highchartOutput("areachart_daily", height = "600px")
                )
              )
            ),
            card(
              full_screen = TRUE,
              card_header("📅 Annual Activity"),
              card_body(
                layout_sidebar(
                  sidebar = sidebar(
                    position = "right",
                    width = 300,
                    open = FALSE,
                    input_switch("se2", "Show Std. Errors", value = FALSE)
                  ),
                  highchartOutput("areachart_annual", height = "600px")
                )
              )
            )
          )
        ),
        
        # Species Density Page (Placeholder)
        nav_panel(
          "Species Density",
          card(
            card_header("📊 Coming Soon"),
            card_body(
              p("This module is under construction and will support density estimation by a REM.")
            )
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  
  nav_panel(
    "About",
    card(
      card_header("\u2139\ufe0f About This App"),
      card_body(
        p("The Camtrap viewR helps visualize and filter camera trap data.")
      )
    )
  ),
  
  nav_panel(
    div(icon("question-circle")),
    card(
      card_header("\u2753 Frequently Asked Questions"),
      card_body(
        p(strong("Q: How do I import data?")),
        p("Use the 'Import' tab in the sidebar."),
        br()
      )
    )
  )
)   
