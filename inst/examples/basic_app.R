library(shiny)
library(leaflet)
library(DT)
library(linkeR)
library(bslib)

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

# UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cerulean"),
  
  titlePanel("Utah Wastewater Monitoring Dashboard"),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Dashboard Controls",
      width = 320,
      
      h5("About This Dashboard"),
      p("This dashboard shows wastewater monitoring data across Utah treatment facilities. 
        Click on map markers or table rows to explore facility details."),
      
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
      verbatimTextOutput("selection_info", placeholder = TRUE)
    ),
    
    # Main content
    div(
      fluidRow(
        column(7,
          h4("Treatment Facility Locations"),
          leafletOutput("wastewater_map", height = "500px")
        ),
        column(5,
          h4("Facility Data"),
          DTOutput("wastewater_table")
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
  
  # Generate data
  wastewater_data <- reactive({
    generate_wastewater_data()
  })
  
  # Store registry in reactiveVal to manage it properly
  current_registry <- reactiveVal(NULL)
  
  output$wastewater_map <- renderLeaflet({
    data <- wastewater_data()

    # Color palette for risk levels
    risk_colors <- c("Low" = "green", "Medium" = "orange", "High" = "red")

    leaflet(data) %>%
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

  # Render the data table
  output$wastewater_table <- renderDT({
    data <- wastewater_data()

    # Select key columns for the table
    table_data <- data[, c("facility_name", "city", "risk_level", 
                          "covid_copies_per_ml", "population_served", 
                          "last_sample_date")]

    datatable(
      table_data,
      selection = "single",
      rownames = FALSE,
      colnames = c("Facility", "City", "Risk Level", "COVID Copies/mL", 
                   "Population", "Last Sample"),
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        order = list(list(3, 'desc')) # Sort by COVID levels descending
      )
    ) %>%
      formatCurrency("covid_copies_per_ml", currency = "", digits = 0) %>%
      formatCurrency("population_served", currency = "", digits = 0) %>%
      formatStyle("risk_level",
        backgroundColor = styleEqual(c("Low", "Medium", "High"), 
                                   c("lightgreen", "orange", "lightcoral"))
      )
  })

  # Initialize registry once when the session starts
  observeEvent(session$clientData, {
    # Set up linking with custom click handlers that match the original behavior
    registry <- linkeR::link_plots(
      session,
      wastewater_map = wastewater_data,
      wastewater_table = wastewater_data,
      shared_id_column = "id",

      # Custom leaflet click handler that matches your original popup exactly
      leaflet_click_handler = function(map_proxy, selected_data, session) {
        if (!is.null(selected_data)) {
          # Recreate the EXACT same popup as your renderLeaflet
          popup_content <- paste0(
            "<strong>", selected_data$facility_name, "</strong><br>",
            "City: ", selected_data$city, "<br>",
            "Risk Level: ", selected_data$risk_level, "<br>",
            "COVID Copies/mL: ", format(selected_data$covid_copies_per_ml, big.mark = ","), "<br>",
            "Population Served: ", format(selected_data$population_served, big.mark = ",")
          )

          # Apply the same behavior as your original map clicks
          map_proxy %>%
            leaflet::setView(
              lng = selected_data$longitude, 
              lat = selected_data$latitude, 
              zoom = 12
            ) %>%
            leaflet::clearPopups() %>%
            leaflet::addPopups(
              lng = selected_data$longitude,
              lat = selected_data$latitude,
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
          showNotification(
            paste("Selected:", selected_data$facility_name, "from", source_id),
            type = "message",
            duration = 2
          )
        }
      }
    )
    
    current_registry(registry)
  }, once = TRUE)
  
  # Selection info display
  output$selection_info <- renderText({
    registry <- current_registry()
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
  
  # Detailed facility information panel
  output$facility_details <- renderUI({
    registry <- current_registry()
    if (is.null(registry)) {
      return(div("Loading..."))
    }
    
    selection <- registry$get_selection()
    
    if (!is.null(selection$selected_id)) {
      data <- wastewater_data()
      facility <- data[data$id == selection$selected_id, ]
      
      if (nrow(facility) > 0) {
        facility <- facility[1, ]
        
        # ... rest of your facility details UI code ...
        # (keep the existing facility details code unchanged)
        
        # Style based on risk level
        panel_color <- switch(as.character(facility$risk_level),
          "Low" = "#d4edda",
          "Medium" = "#fff3cd", 
          "High" = "#f8d7da"
        )
        
        border_color <- switch(as.character(facility$risk_level),
          "Low" = "#c3e6cb",
          "Medium" = "#ffeaa7",
          "High" = "#f5c6cb"
        )
        
        text_color <- switch(as.character(facility$risk_level),
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
              p(strong("Name: "), facility$facility_name),
              p(strong("City: "), facility$city),
              p(strong("ID: "), facility$id),
              p(strong("Risk Level: "), 
                span(style = paste0("font-weight: bold; color: ", text_color), 
                     as.character(facility$risk_level)))
            ),
            
            column(3,
              h4(icon("flask"), " Water Quality"),
              p(strong("COVID Copies/mL: "), 
                format(facility$covid_copies_per_ml, big.mark = ",")),
              p(strong("pH Level: "), facility$ph),
              p(strong("Temperature: "), facility$temperature_f, "°F"),
              p(strong("Flow Rate: "), facility$flow_mgd, " MGD")
            ),
            
            column(3,
              h4(icon("users"), " Operations"),
              p(strong("Population Served: "), 
                format(facility$population_served, big.mark = ",")),
              p(strong("Last Sample: "), 
                format(facility$last_sample_date, "%B %d, %Y")),
              p(strong("Next Sample: "), 
                format(facility$next_sample_date, "%B %d, %Y")),
              p(strong("Days Since Sample: "), 
                as.numeric(Sys.Date() - facility$last_sample_date))
            ),
            
            column(3,
              h4(icon("map-marker-alt"), " Location"),
              p(strong("Latitude: "), round(facility$latitude, 4)),
              p(strong("Longitude: "), round(facility$longitude, 4)),
            )
          ),
        )
      }
    } else {
      div(
        style = "text-align: center; padding: 40px; color: #6c757d;",
        h4(icon("mouse-pointer"), " Select a Facility"),
        p("Click on a map marker or table row to view detailed facility information.")
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
