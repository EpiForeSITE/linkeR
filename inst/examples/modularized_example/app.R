library(shiny)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(linkeR)

# Source module files
source("map_module.R")
source("table_module.R")

generate_test_data <- function() {
  sewershed_centroids <- data.frame(
    msd_name = c("SITE_A", "SITE_B", "SITE_C", "SITE_D", "SITE_E"),
    msd_shrtnm = c("Site A", "Site B", "Site C", "Site D", "Site E"),
    latitude = c(40.7608, 40.7128, 40.7580, 40.7489, 40.7306),
    longitude = c(-111.8910, -111.9060, -111.8700, -111.8900, -111.9100),
    concentration = c("Elevated", "Very Elevated", "Low", "Watch", "Very Low"),
    trend = c("Increasing", "Plateau/indeterminate", "Decreasing", "Increasing", "Insufficient data"),
    county = c("Salt Lake", "Salt Lake", "Salt Lake", "Salt Lake", "Salt Lake"),
    lhd = c("Salt Lake County Health", "Salt Lake County Health", "Salt Lake County Health", 
            "Salt Lake County Health", "Salt Lake County Health")
  )
  
  # Convert to sf object
  sewershed_centroids <- st_as_sf(sewershed_centroids, 
                                   coords = c("longitude", "latitude"),
                                   crs = 4326)
  
  return(sewershed_centroids)
}

# UI
ui <- fluidPage(
  titlePanel("linkeR Modular App Test - Observer Firing Validation"),
  
  tags$head(
    tags$style(HTML("
      .diagnostics {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        padding: 15px;
        margin: 10px 0;
      }
      .success { color: #28a745; font-weight: bold; }
      .warning { color: #ffc107; font-weight: bold; }
      .error { color: #dc3545; font-weight: bold; }
    "))
  ),
  
  div(class = "diagnostics",
    h4("Test Status"),
    p("This app tests whether linkeR observers fire correctly in a modular structure."),
    p("Click on map markers or table rows. You should see:"),
    tags$ul(
      tags$li(class = "success", "Console messages from linkeR observers"),
      tags$li(class = "success", "Registry callback firing"),
      tags$li(class = "success", "Components updating each other")
    ),
    verbatimTextOutput("test_status")
  ),
  
  fluidRow(
    column(6,
      h3("Map Module"),
      map_ui("test_map")
    ),
    column(6,
      h3("Table Module"),
      table_ui("test_table")
    )
  ),
  
  hr(),
  
  h3("Selection Details"),
  div(class = "diagnostics",
    verbatimTextOutput("selection_info")
  ),
  
  h3("Registry Diagnostics"),
  div(class = "diagnostics",
    verbatimTextOutput("diagnostics")
  )
)

# Server
server <- function(input, output, session) {
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  TEST APP STARTING\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
  
  # Generate test data
  sewershed_centroids <- generate_test_data()
  
  # Track events for testing
  event_log <- reactiveVal(character(0))
  
  add_event <- function(msg) {
    timestamp <- format(Sys.time(), "%H:%M:%OS3")
    log_entry <- paste0("[", timestamp, "] ", msg)
    cat(log_entry, "\n")
    # Use isolate to avoid reactive context issues during initialization
    isolate({
      event_log(c(event_log(), log_entry))
    })
  }
  
  add_event("Creating registry...")
  
  # Create registry with callback (same as user's app)
  registry <- linkeR::create_link_registry(
    session,
    on_selection_change = function(selected_id, selected_data, source_id, session) {
      msg <- sprintf("REGISTRY CALLBACK: ID='%s' Source='%s' Data=%s",
                     selected_id, 
                     source_id,
                     if(!is.null(selected_data)) selected_data$msd_shrtnm else "NULL")
      add_event(msg)
    }
  )
  
  add_event("Registry created successfully")
  
  # Call modules (passing registry like user's app)
  add_event("Initializing map module...")
  map_result <- map_server(
    "test_map",
    sewershed_centroids = sewershed_centroids,
    registry = registry
  )
  add_event("Map module initialized")
  
  add_event("Initializing table module...")
  table_result <- table_server(
    "test_table",
    table_data = sewershed_centroids,
    registry = registry
  )
  add_event("Table module initialized")
  
  # Test status output
  output$test_status <- renderPrint({
    events <- event_log()
    if (length(events) == 0) {
      cat("No events logged yet.\n")
    } else {
      cat("Recent events (last 10):\n")
      cat(paste(tail(events, 10), collapse = "\n"))
    }
  })
  
  # Selection info
  output$selection_info <- renderPrint({
    sel <- registry$get_selection()
    cat("Current Selection State:\n")
    cat("  Selected ID:", sel$selected_id %||% "None", "\n")
    cat("  Source:", sel$source %||% "None", "\n")
    
    if (!is.null(sel$selected_id)) {
      # Get the full data for this selection
      data <- sewershed_centroids %>% 
        filter(msd_name == sel$selected_id)
      if (nrow(data) > 0) {
        cat("\n  Site Details:\n")
        cat("    Name:", data$msd_shrtnm, "\n")
        cat("    Concentration:", data$concentration, "\n")
        cat("    Trend:", data$trend, "\n")
      }
    }
    
    cat("\n")
    cat("Event Log Count:", length(event_log()), "events\n")
    
    # Check if observers are firing
    events <- event_log()
    linker_events <- grep("linkeR observer fired", events, value = TRUE)
    manual_events <- grep("MANUAL observer fired", events, value = TRUE)
    callback_events <- grep("REGISTRY CALLBACK", events, value = TRUE)
    
    cat("\nObserver Activity:\n")
    cat("  linkeR observers:", length(linker_events), "times\n")
    cat("  Manual observers:", length(manual_events), "times\n")
    cat("  Registry callbacks:", length(callback_events), "times\n")
    
    if (length(callback_events) > 0) {
      cat("\nSUCCESS: Observers are firing!\n")
    } else if (length(manual_events) > 0) {
      cat("\nWARNING: Only manual observers firing, linkeR observers may not be working\n")
    } else {
      cat("\nERROR: No observers firing - try clicking something!\n")
    }
  })
  
  # Diagnostics output
  output$diagnostics <- renderPrint({
    linkeR::diagnose_registry(registry, session)
  })
  
  add_event("App initialization complete")
  cat("\n")
  cat("═══════════════════════════════════════════════════════════\n")
  cat("  APP READY - Try clicking on map markers or table rows\n")
  cat("═══════════════════════════════════════════════════════════\n\n")
}

shinyApp(ui, server)
