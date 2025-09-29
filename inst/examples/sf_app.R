library(shiny)
library(leaflet)
library(DT)
library(linkeR)
library(bslib)
library(sf)

# Generate realistic wastewater monitoring data as sf objects for Utah locations
generate_wastewater_sf_data <- function() {
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
  
  # Create sf point geometries from coordinates
  points <- sf::st_sfc(
    lapply(seq_len(nrow(utah_locations)), function(i) {
      sf::st_point(c(utah_locations$longitude[i], utah_locations$latitude[i]))
    }),
    crs = 4326  # WGS84 coordinate reference system
  )
  
  # Generate realistic wastewater monitoring data as sf object
  wastewater_sf <- sf::st_sf(
    id = paste0("WW_", sprintf("%03d", 1:n_sites)),
    facility_name = paste(utah_locations$city, "WWTP"),
    city = utah_locations$city,
    
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
    
    # SF geometry column (this will be auto-converted by linkeR)
    geometry = points,
    
    stringsAsFactors = FALSE
  )
  
  return(wastewater_sf)
}

# Generate regular data frame for table (to show sf + regular data linking)
generate_table_data <- function() {
  data.frame(
    id = paste0("WW_", sprintf("%03d", 1:10)),
    facility_name = paste(c("Salt Lake City", "Ogden", "Provo", "West Valley City", "Sandy", 
                           "Orem", "West Jordan", "Layton", "Taylorsville", "Murray"), "WWTP"),
    compliance_status = sample(c("Compliant", "Warning", "Non-Compliant"), 10, replace = TRUE),
    last_inspection = Sys.Date() - sample(30:365, 10, replace = TRUE),
    permit_expires = Sys.Date() + sample(30:730, 10, replace = TRUE),
    violations_ytd = sample(0:5, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  titlePanel("Utah Wastewater Monitoring Dashboard (SF Integration Demo)"),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Dashboard Controls",
      width = 320,
      
      h5("About This Dashboard"),
      p("This dashboard demonstrates linkeR's sf integration. The map uses an sf object 
        with point geometries, while the table uses a regular data frame. linkeR automatically 
        extracts coordinates from the sf geometry column."),
      
      div(
        style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;",
        h6(icon("info-circle"), " SF Integration Demo"),
        tags$ul(
          tags$li("Map data: sf object with POINT geometries"),
          tags$li("Table data: Regular data.frame"),
          tags$li("Coordinates auto-extracted from geometry column"),
          tags$li("Seamless linking between sf and regular data")
        )
      ),
      
      hr(),
      
      h5("Risk Level Legend"),
      div(
        style = "margin: 10px 0;",
        span(style = "color: green; font-weight: bold;", "● Low Risk"), " < 1,000 copies/mL", br(),
        span(style = "color: orange; font-weight: bold;", "● Medium Risk"), " 1,000-10,000 copies/mL", br(),
        span(style = "color: red; font-weight: bold;", "● High Risk"), " > 10,000 copies/mL"
      ),
      
      hr(),
      
      h5("Selection Info"),
      verbatimTextOutput("selection_info", placeholder = TRUE),
      
      hr(),
      
      h5("SF Object Info"),
      verbatimTextOutput("sf_info", placeholder = TRUE)
    ),
    
    # Main content
    div(
      fluidRow(
        column(7,
          h4("Treatment Facility Locations", 
             span(style = "font-size: 14px; color: #666;", "(SF Object)")),
          leafletOutput("wastewater_map", height = "500px")
        ),
        column(5,
          h4("Compliance Data",
             span(style = "font-size: 14px; color: #666;", "(Regular Data Frame)")),
          DTOutput("compliance_table")
        )
      ),
      
      # Details panel for selected facility
      fluidRow(
        column(12,
          br(),
          uiOutput("facility_details")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Generate sf data for map
  wastewater_sf_data <- reactive({
    generate_wastewater_sf_data()
  })
  
  # Generate regular data frame for table
  compliance_data <- reactive({
    generate_table_data()
  })
  
  # Store registry in reactiveVal to manage it properly
  current_registry <- reactiveVal(NULL)
  
  output$wastewater_map <- renderLeaflet({
    data <- wastewater_sf_data()
    
    # Note: linkeR will automatically extract coordinates from the sf geometry
    # We still need to access them here for the initial leaflet rendering
    coords <- sf::st_coordinates(data)
    data_with_coords <- data
    data_with_coords$longitude <- as.numeric(coords[, 1])
    data_with_coords$latitude <- as.numeric(coords[, 2])

    # Color palette for risk levels
    risk_colors <- c("Low" = "green", "Medium" = "orange", "High" = "red")

      leaflet(data_with_coords) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~id, # Critical for linking!
        radius = ~sqrt(flow_mgd) * 2 + 5, # Size based on flow
        color = ~risk_colors[risk_level],
        fillColor = ~risk_colors[risk_level],
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2
      ) %>%
      setView(lng = -111.8910, lat = -40.7608, zoom = 8) # Center on Utah
  })

  # Render the compliance table (regular data frame)
  output$compliance_table <- renderDT({
    data <- compliance_data()

    datatable(
      data,
      selection = "single",
      rownames = FALSE,
      colnames = c("ID", "Facility", "Compliance", "Last Inspection", 
                   "Permit Expires", "Violations YTD"),
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        order = list(list(5, 'desc')) # Sort by violations descending
      )
    ) %>%
      formatStyle("compliance_status",
        backgroundColor = styleEqual(
          c("Compliant", "Warning", "Non-Compliant"), 
          c("lightgreen", "yellow", "lightcoral")
        )
      ) %>%
      formatStyle("violations_ytd",
        backgroundColor = styleInterval(
          c(0, 2), 
          c("lightgreen", "yellow", "lightcoral")
        )
      )
  })

  # Initialize registry once when the session starts
  observeEvent(session$clientData, {
    # Set up linking with sf object and regular data frame
    registry <- linkeR::link_plots(
      session,
      wastewater_map = wastewater_sf_data,    # SF OBJECT - coordinates auto-extracted!
      compliance_table = compliance_data,      # Regular data frame
      shared_id_column = "id",

      # Custom leaflet click handler
      leaflet_click_handler = function(map_proxy, selected_data, session) {
        if (!is.null(selected_data)) {
          if (!("longitude" %in% names(selected_data)) || 
              !("latitude" %in% names(selected_data))) {
           
            if (inherits(selected_data, "sf")) {
              # Extract coordinates from sf object
              coords <- sf::st_coordinates(selected_data)
              selected_data$longitude <- coords[, 1]
              selected_data$latitude <- coords[, 2]
            } else {
              cat("Selected data is not an sf object and lacks coordinates.\n")
              return()
            }
          }

          longitude <- as.numeric(selected_data$longitude)
          latitude <- as.numeric(selected_data$latitude)
          
          # Validate coordinates
          if (is.na(longitude) || is.na(latitude)) {
            cat("Invalid coordinates: lng =", longitude, ", lat =", latitude, "\n")
            return()
          }
          
          # Build popup content
          popup_content <- paste0(
            "<strong>", selected_data$facility_name, "</strong><br>",
            "<em>Data from SF object (processed by linkeR)</em><br>",
            "City: ", selected_data$city, "<br>",
            "Risk Level: ", selected_data$risk_level, "<br>",
            "COVID Copies/mL: ", format(selected_data$covid_copies_per_ml, big.mark = ","), "<br>",
            "Population Served: ", format(selected_data$population_served, big.mark = ","), "<br>",
            "Coordinates: ", round(longitude, 4), ", ", round(latitude, 4)
          )

          # Update map
          map_proxy %>%
            leaflet::setView(
              lng = longitude, 
              lat = latitude, 
              zoom = 12
            ) %>%
            leaflet::clearPopups() %>%
            leaflet::addPopups(
              lng = longitude,
              lat = latitude,
              popup = popup_content
            )
        } else {
          # Handle deselection
          map_proxy %>% leaflet::clearPopups()
        }
      },
      
      # Optional: global selection change callback
      on_selection_change = function(selected_id, selected_data, source_id, session) {
        if (!is.null(selected_id) && !is.null(selected_data)) {
          # Show which type of data was selected
          data_type <- if ("longitude" %in% names(selected_data)) {
            "(from SF object)"
          } else {
            "(from regular data frame)"
          }
          
          showNotification(
            paste("Selected:", selected_data$facility_name, data_type, "from", source_id),
            type = "message",
            duration = 3
          )
        }
      }
    )
    
    current_registry(registry)
  }, once = TRUE)
  
  # Selection info display
  output$selection_info <- renderText({
    # Use isolate to access current_registry outside of reactive context
    registry <- isolate(current_registry())
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
  
  # SF object information
  output$sf_info <- renderText({
    data <- wastewater_sf_data()
    if (inherits(data, "sf")) {
      geom_type <- sf::st_geometry_type(data)[1]
      crs_info <- sf::st_crs(data)$input
      n_features <- nrow(data)
      
      paste0(
        "Geometry Type: ", geom_type, "\n",
        "CRS: ", crs_info, "\n",
        "Features: ", n_features, "\n",
        "Status: Coordinates auto-extracted by linkeR"
      )
    } else {
      "Not an sf object"
    }
  })
  
  # Detailed facility information panel
  output$facility_details <- renderUI({
    # Use isolate to access current_registry
    registry <- isolate(current_registry())
    if (is.null(registry)) {
      return(div("Loading..."))
    }
    
    selection <- registry$get_selection()
    
    if (!is.null(selection$selected_id)) {
      # Get data from both sources to show combined information
      sf_data <- wastewater_sf_data()
      compliance_data_df <- compliance_data()
      
      # Extract coordinates from sf object for display and ensure they're numeric
      coords <- sf::st_coordinates(sf_data)
      sf_data$longitude <- as.numeric(coords[, 1])
      sf_data$latitude <- as.numeric(coords[, 2])
      sf_data_df <- sf::st_drop_geometry(sf_data)
      
      facility_sf <- sf_data_df[sf_data_df$id == selection$selected_id, ]
      facility_compliance <- compliance_data_df[compliance_data_df$id == selection$selected_id, ]
      
      if (nrow(facility_sf) > 0) {
        facility_sf <- facility_sf[1, ]
        
        # Ensure coordinates are numeric before rounding
        longitude <- as.numeric(facility_sf$longitude)
        latitude <- as.numeric(facility_sf$latitude)
        
        # Style based on risk level
        panel_color <- switch(as.character(facility_sf$risk_level),
          "Low" = "#d4edda",
          "Medium" = "#fff3cd", 
          "High" = "#f8d7da"
        )
        
        border_color <- switch(as.character(facility_sf$risk_level),
          "Low" = "#c3e6cb",
          "Medium" = "#ffeaa7",
          "High" = "#f5c6cb"
        )
        
        text_color <- switch(as.character(facility_sf$risk_level),
          "Low" = "#155724",
          "Medium" = "#856404",
          "High" = "#721c24"
        )
        
        div(
          style = paste0(
            "background-color: ", panel_color, "; ",
            "border: 2px solid ", border_color, "; ",
            "border-radius: 8px; ",
            "padding: 20px; ",
            "margin: 10px 0; ",
            "color: ", text_color, ";"
          ),
          
          fluidRow(
            column(3,
              h4(icon("building"), " Facility Details"),
              p(strong("Name: "), facility_sf$facility_name),
              p(strong("City: "), facility_sf$city),
              p(strong("ID: "), facility_sf$id),
              p(strong("Risk Level: "), 
                span(style = paste0("font-weight: bold; color: ", text_color), 
                    as.character(facility_sf$risk_level))),
              div(
                style = "background-color: rgba(0,0,0,0.1); padding: 5px; border-radius: 3px; font-size: 12px;",
                "Data from SF object"
              )
            ),
            
            column(3,
              h4(icon("flask"), " Water Quality"),
              p(strong("COVID Copies/mL: "), 
                format(facility_sf$covid_copies_per_ml, big.mark = ",")),
              p(strong("pH Level: "), facility_sf$ph),
              p(strong("Temperature: "), facility_sf$temperature_f, "°F"),
              p(strong("Flow Rate: "), facility_sf$flow_mgd, " MGD")
            ),
            
            column(3,
              h4(icon("users"), " Operations"),
              p(strong("Population Served: "), 
                format(facility_sf$population_served, big.mark = ",")),
              p(strong("Last Sample: "), 
                format(facility_sf$last_sample_date, "%B %d, %Y")),
              p(strong("Next Sample: "), 
                format(facility_sf$next_sample_date, "%B %d, %Y")),
              p(strong("Days Since Sample: "), 
                as.numeric(Sys.Date() - facility_sf$last_sample_date))
            ),
            
            column(3,
              h4(icon("map-marker-alt"), " Location & Compliance"),
              p(strong("Latitude: "), round(latitude, 4)),
              p(strong("Longitude: "), round(longitude, 4)),
              if (nrow(facility_compliance) > 0) {
                list(
                  p(strong("Compliance: "), facility_compliance$compliance_status),
                  p(strong("Violations YTD: "), facility_compliance$violations_ytd),
                  div(
                    style = "background-color: rgba(0,0,0,0.1); padding: 5px; border-radius: 3px; font-size: 12px;",
                    "Compliance data from regular data frame"
                  )
                )
              } else {
                p("No compliance data available")
              }
            )
          ),
          
          # Show sf integration info
          hr(),
          div(
            style = "background-color: rgba(0,0,0,0.1); padding: 10px; border-radius: 5px;",
            h5(icon("info-circle"), " SF Integration Demo"),
            p("This facility's location data comes from an sf object with POINT geometries. 
              linkeR automatically extracted the longitude (", round(longitude, 4), 
              ") and latitude (", round(latitude, 4), 
              ") coordinates from the geometry column for seamless integration with the leaflet map.")
          )
        )
      }
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #6c757d;",
        h4(icon("mouse-pointer"), " Select a Facility"),
        p("Click on a map marker or table row to view detailed facility information."),
        p("This demo shows linkeR linking an sf object (map) with a regular data frame (table).")
      )
    }
  })

   # Clean up observers when session ends
  session$onSessionEnded(function() {
    registry <- current_registry()
    if (!is.null(registry)) {
      registry$clear_all()
    }
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
