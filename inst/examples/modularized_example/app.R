library(shiny)
library(leaflet)
library(DT)
library(linkeR)
library(bslib)

# Source the modules
source("map_module.R")
source("table_module.R")

# --- Data Generation (copied from basic_app.R) ---
# Generate realistic wastewater monitoring data for Utah locations
generate_wastewater_data <- function() {
  # Real Utah city coordinates
  utah_locations <- data.frame(
    city = c("Salt Lake City", "Ogden", "Provo", "West Valley City", "Sandy", 
             "Orem", "West Jordan", "Layton", "Taylorsville", "Murray"),
    latitude = c(40.7608, 41.2230, 40.2338, 40.6916, 40.5649, 
                40.2969, 40.6097, 41.0602, 40.6677, 40.6669),
    longitude = c(-111.8910, -111.9738, -111.6585, -111.9391, -111.8389,
                 -111.6946, -111.9391, -111.9710, -111.9391, -111.8879),
    stringsAsFactors = FALSE
  )
  
  n_sites <- nrow(utah_locations)
  
  # Generate realistic wastewater monitoring data
  wastewater_data <- data.frame(
    id = paste0("WW_", sprintf("%03d", 1:n_sites)),
    facility_name = paste(utah_locations$city, "WWTP"),
    city = utah_locations$city,
    latitude = utah_locations$latitude,
    longitude = utah_locations$longitude,
    
    # Monitoring parameters
    covid_copies_per_ml = round(10^runif(n_sites, 2, 5.5)), # 100 to ~300,000
    flow_mgd = round(runif(n_sites, 0.5, 45), 1), # Million gallons per day
    population_served = round(runif(n_sites, 5000, 200000), -3),
    
    # Status based on COVID levels
    risk_level = factor(
      ifelse(10^runif(n_sites, 2, 5.5) > 10000, "High",
             ifelse(10^runif(n_sites, 2, 5.5) > 1000, "Medium", "Low")),
      levels = c("Low", "Medium", "High")
    ),
    
    # Sample timing
    last_sample_date = Sys.Date() - sample(0:7, n_sites, replace = TRUE),
    next_sample_date = Sys.Date() + sample(1:7, n_sites, replace = TRUE),
    
    # Additional parameters
    ph = round(runif(n_sites, 6.5, 8.5), 1),
    temperature_f = round(runif(n_sites, 45, 75), 1),
    
    stringsAsFactors = FALSE
  )
  
  return(wastewater_data)
}

# --- UI ---
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  titlePanel("linkeR Module Linking Demonstration"),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Dashboard Controls",
      p("This example demonstrates the issue of using linkeR across Shiny modules. 
         The linking is expected to fail because the component IDs are namespaced."),
      hr(),
      verbatimTextOutput("selection_info", placeholder = TRUE)
    ),
    
    div(
      fluidRow(
        column(7,
          h4("Wastewater Map (Module 1)"),
          # Call the map module UI
          mapUI("map_module")
        ),
        column(5,
          h4("Facility Data (Module 2)"),
          # Call the table module UI
          tableUI("table_module")
        )
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive data
  wastewater_data <- reactive({
    generate_wastewater_data()
  })
  
  # Create the link registry
  registry <- create_link_registry(session)

  # Call module servers
  mapServer("map_module", wastewater_data, registry)
  tableServer("table_module", wastewater_data, registry)
  
  # Selection info display
  output$selection_info <- renderText({
    if (!is.null(registry)) {
      selection <- registry$get_selection()
      if (!is.null(selection$selected_id)) {
        paste("Selected ID:", selection$selected_id, "\nSource:", selection$source)
      } else {
        "No facility selected"
      }
    } else {
      "Registry not initialized"
    }
  })
}

# Run the app
shinyApp(ui, server)
