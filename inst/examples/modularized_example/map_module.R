# /path/to/your/app/map_module.R

#' Map Module UI
#'
#' @param id A character string. The namespace ID.
#' @return A UI definition.
mapUI <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("wastewater_map"), height = "500px")
}

#' Map Module Server
#'
#' @param id A character string. The namespace ID.
#' @param data A reactive expression returning the data frame for the map.
mapServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$wastewater_map <- renderLeaflet({
      map_data <- data()
      
      # Color palette for risk levels
      risk_colors <- c("Low" = "green", "Medium" = "orange", "High" = "red")
      
      leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~id, # Critical for linking!
          radius = ~sqrt(flow_mgd) * 2 + 5,
          color = ~risk_colors[risk_level],
          fillColor = ~risk_colors[risk_level],
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2
        ) %>%
        setView(lng = -111.8910, lat = 40.7608, zoom = 8) # Centered on Utah
    })
  })
}
