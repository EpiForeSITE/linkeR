# Map module - mimics user's map_module.R structure
# Tests linkeR observer firing in module context

map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("combo_map"), height = "400px"),
    hr(),
    div(style = "margin-top: 10px;",
      checkboxInput(ns("show_labels"), "Show Labels", value = FALSE)
    ),
    verbatimTextOutput(ns("module_debug"))
  )
}

map_server <- function(id, sewershed_centroids, registry) {
  moduleServer(id, function(input, output, session) {
    cat("\n[MAP MODULE] Starting for id:", id, "\n")
    cat("[MAP MODULE] Session namespace:", session$ns(""), "\n")
    
    # Store selected site (mimics user's reactive)
    selected_msd <- reactiveVal(NULL)
    
    # Manual click counter for comparison
    manual_clicks <- reactiveVal(0)
    linker_clicks <- reactiveVal(0)
    
    # Register leaflet component with linkeR (BEFORE rendering)
    cat("[MAP MODULE] Registering leaflet component 'combo_map'...\n")
    
    linkeR::register_leaflet(
      session = session,
      registry = registry,
      leaflet_output_id = "combo_map",
      data_reactive = reactive({sewershed_centroids}),
      shared_id_column = "msd_name",
      click_handler = function(map_proxy, selected_data, session) {
        if (!is.null(selected_data) && nrow(selected_data) > 0) {
          cat("[MAP MODULE - linkeR] Click handler called for:", selected_data$msd_name, "\n")
          linker_clicks(linker_clicks() + 1)
          
          # Update reactive (mimics user's code)
          selected_msd(selected_data$msd_name)
          
          # Get coordinates - handle both sf and non-sf data
          if (inherits(selected_data, "sf")) {
            coords <- st_coordinates(selected_data)
            lng <- coords[1, 1]
            lat <- coords[1, 2]
          } else {
            # Data came from non-sf source (like DT), need to look up in original data
            orig_data <- sewershed_centroids[sewershed_centroids$msd_name == selected_data$msd_name, ]
            if (nrow(orig_data) > 0) {
              coords <- st_coordinates(orig_data)
              lng <- coords[1, 1]
              lat <- coords[1, 2]
            } else {
              cat("[MAP MODULE - linkeR] Warning: Could not find coordinates for", selected_data$msd_name, "\n")
              return()
            }
          }
          
          # Highlight selected marker
          map_proxy %>%
            leaflet::clearGroup("highlight") %>%
            leaflet::addCircleMarkers(
              lng = lng,
              lat = lat,
              group = "highlight",
              radius = 15,
              color = "red",
              fillColor = "red",
              fillOpacity = 0.5
            ) %>%
            leaflet::setView(lng = lng, lat = lat, zoom = 12)
        } else {
          cat("[MAP MODULE - linkeR] Click handler called with NULL/empty data\n")
          # Clear highlight when selection is cleared
          map_proxy %>%
            leaflet::clearGroup("highlight")
        }
      }
    )
    
    cat("[MAP MODULE] Leaflet component registered successfully\n")
    
    # Render the map (AFTER registration)
    output$combo_map <- renderLeaflet({
      cat("[MAP MODULE] Rendering leaflet map...\n")
      
      data <- sewershed_centroids
      
      # Extract coordinates
      coords <- st_coordinates(data)
      data$longitude <- coords[, 1]
      data$latitude <- coords[, 2]
      
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~msd_name,
          radius = 8,
          color = "blue",
          fillColor = "lightblue",
          fillOpacity = 0.7,
          label = ~msd_shrtnm
        ) %>%
        setView(lng = -111.89, lat = 40.74, zoom = 11)
    })
    
    # Manual observer for comparison (mimics user's workaround)
    observeEvent(input$combo_map_marker_click, {
      cat("[MAP MODULE - MANUAL] Manual observer fired!\n")
      manual_clicks(manual_clicks() + 1)
      
      clicked_id <- input$combo_map_marker_click$id
      cat("[MAP MODULE - MANUAL] Clicked ID:", clicked_id, "\n")
      
      # This demonstrates what the user had to do as a workaround
      # If linkeR is working, this should be redundant
    })
    
    # Labels toggle
    observe({
      # Extract coordinates once from sf object
      coords <- st_coordinates(sewershed_centroids)
      data_with_coords <- sewershed_centroids
      data_with_coords$longitude <- coords[, 1]
      data_with_coords$latitude <- coords[, 2]
      
      if (input$show_labels) {
        leafletProxy("combo_map") %>%
          clearMarkers() %>%
          addCircleMarkers(
            data = data_with_coords,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~msd_name,
            radius = 8,
            color = "blue",
            fillColor = "lightblue",
            fillOpacity = 0.7,
            label = ~msd_shrtnm,
            labelOptions = labelOptions(noHide = TRUE)
          )
      } else {
        leafletProxy("combo_map") %>%
          clearMarkers() %>%
          addCircleMarkers(
            data = data_with_coords,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~msd_name,
            radius = 8,
            color = "blue",
            fillColor = "lightblue",
            fillOpacity = 0.7,
            label = ~msd_shrtnm
          )
      }
    })
    
    # Debug output
    output$module_debug <- renderPrint({
      cat("Map Module Status:\n")
      cat("  Selected site:", selected_msd() %||% "None", "\n")
      cat("  linkeR clicks:", linker_clicks(), "\n")
      cat("  Manual clicks:", manual_clicks(), "\n")
      cat("  Input name:", session$ns("combo_map_marker_click"), "\n")
      
      if (linker_clicks() > 0) {
        cat("\nlinkeR observers are WORKING!\n")
      } else if (manual_clicks() > 0) {
        cat("\nManual observers work, but linkeR not firing yet\n")
      } else {
        cat("\n→ Click a marker to test\n")
      }
    })
    
    return(list(
      selected_msd = reactive(selected_msd())
    ))
  })
}
