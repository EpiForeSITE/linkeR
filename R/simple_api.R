#' Simple Plot Linking Function
#'
#' `link_plots` is a simple interface to link interactive plots and tables in Shiny.
#' This function automatically detects component types and sets up bidirectional linking.
#' For more robust applications, especially with complex naming schemes, consider using
#' \code{\link{register_leaflet}} and \code{\link{register_dt}} directly.
#'
#' @param session The Shiny session object
#' @param ... Named arguments where names are component output IDs and values are
#'   reactive data frames. Each data frame must contain the shared_id_column.
#'   For leaflet maps: can be sf objects (coordinates auto-extracted) or regular
#'   data frames with longitude/latitude columns.
#' @param shared_id_column Character string naming the column that contains unique
#'   identifiers present in all linked components.
#' @param leaflet_lng_col Character string naming the longitude column for leaflet maps.
#'   Defaults to "longitude". For sf objects, this will be the name of the created column.
#' @param leaflet_lat_col Character string naming the latitude column for leaflet maps.
#'   Defaults to "latitude". For sf objects, this will be the name of the created column.
#' @param leaflet_click_handler Optional function that handles leaflet marker clicks.
#'   This will be used for both direct clicks and when other components select this marker.
#'   Function should accept (map_proxy, selected_data, session).
#' @param dt_click_handler Optional function that handles DT row selections.
#'   This will be used for both direct clicks and when other components select this row.
#'   Function should accept (dt_proxy, selected_data, session).
#' @param on_selection_change Optional callback function that gets called when selection changes.
#'   Function should accept parameters: (selected_id, selected_data, source_component_id, session)
#' @return Invisibly returns the created registry object
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(leaflet)
#'   library(DT)
#'
#'   # Sample data
#'   sample_data <- data.frame(
#'     id = 1:10,
#'     name = paste("Location", 1:10),
#'     latitude = runif(10, 40.7, 40.8),
#'     longitude = runif(10, -111.95, -111.85),
#'     value = round(runif(10, 100, 1000))
#'   )
#'
#'   ui <- fluidPage(
#'     titlePanel("linkeR Example"),
#'     fluidRow(
#'       column(6, leafletOutput("my_map")),
#'       column(6, DTOutput("my_table"))
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     my_data <- reactive({
#'       sample_data
#'     })
#'
#'     output$my_map <- renderLeaflet({
#'       leaflet(my_data()) %>%
#'         addTiles() %>%
#'         addMarkers(
#'           lng = ~longitude,
#'           lat = ~latitude,
#'           layerId = ~id,
#'           popup = ~name
#'         )
#'     })
#'
#'     output$my_table <- renderDT({
#'       datatable(my_data()[, c("name", "value")], selection = "single")
#'     })
#'
#'     link_plots(
#'       session,
#'       my_map = my_data,
#'       my_table = my_data,
#'       shared_id_column = "id"
#'     )
#'   }
#'
#'   shinyApp(ui, server)
#' }
link_plots <- function(session, ..., shared_id_column,
                       leaflet_lng_col = "longitude",
                       leaflet_lat_col = "latitude",
                       leaflet_click_handler = NULL,
                       dt_click_handler = NULL,
                       on_selection_change = NULL) {
  # Validate inputs
  if (missing(session)) {
    stop("session argument is required")
  }
  if (missing(shared_id_column)) {
    stop("shared_id_column argument is required")
  }

  # Get component specifications
  components <- list(...)

  if (length(components) < 2) {
    stop("At least two components must be specified for linking")
  }

  # Create registry with callback
  registry <- create_link_registry(session, on_selection_change = on_selection_change)

  for (comp_name in names(components)) {
    comp_data <- components[[comp_name]]

    # Detect component type
    comp_type <- detect_component_type(comp_name, comp_data)

    # For leaflet components, check data compatibility early
    if (comp_type == "leaflet") {
      # Test data processing to give early feedback
      test_data <- isolate(comp_data())
      
      if (requireNamespace("sf", quietly = TRUE) && inherits(test_data, "sf")) {
        message("Detected sf object for ", comp_name, ". Will automatically extract coordinates.")
      } else {
        # Validate that regular data frame has required columns
        required_cols <- c(shared_id_column, leaflet_lng_col, leaflet_lat_col)
        missing_cols <- setdiff(required_cols, names(test_data))
        if (length(missing_cols) > 0) {
          stop(
            "Component '", comp_name, "' is missing required columns: ",
            paste(missing_cols, collapse = ", "),
            ". For sf objects, coordinates will be extracted automatically. ",
            "For regular data frames, ensure ", leaflet_lng_col, " and ", leaflet_lat_col, " columns exist."
          )
        }
      }

      # USE REGISTER_LEAFLET FUNCTION
      register_leaflet(
        registry = registry,
        leaflet_output_id = comp_name,
        data_reactive = comp_data,
        shared_id_column = shared_id_column,
        lng_col = leaflet_lng_col,
        lat_col = leaflet_lat_col,
        highlight_zoom = 12,
        click_handler = leaflet_click_handler
      )
      
    } else if (comp_type == "datatable") {
      # validate that component has shared_id_column
      if (!shared_id_column %in% names(isolate(comp_data()))) {
        stop(
          "Component '", comp_name, "' data must contain the shared_id_column: ",
          shared_id_column
        )
      }

      # USE REGISTER_DT FUNCTION
      register_dt(
        registry = registry,
        dt_output_id = comp_name,
        data_reactive = comp_data,
        shared_id_column = shared_id_column,
        click_handler = dt_click_handler
      )
    }
  }

  message(
    "Linked ", length(components), " components: ",
    paste(names(components), collapse = ", ")
  )

  invisible(registry)
}

#' Detect Component Type Based on Output ID Patterns
#'
#' `detect_component_type` is an internal function that attempts to automatically determine the type of
#' Shiny output component based on common naming patterns in the component ID.
#' This function uses simple heuristics to classify components as either
#' "leaflet" (for maps) or "datatable" (for tables), with "datatable" as the
#' default fallback.
#'
#' @param component_id Character string. The ID of the output component to classify.
#' @param data_reactive Reactive data object (currently unused in the function logic).
#'
#' @return Character string indicating the detected component type:
#'   \itemize{
#'     \item "leaflet" - for IDs containing "map" or "leaflet"
#'     \item "datatable" - for IDs containing "table" or "dt", or as default
#'   }
#'
#' @details
#' The function uses case-insensitive pattern matching on the component ID:
#' \itemize{
#'   \item IDs containing "map" or "leaflet" are classified as "leaflet"
#'   \item IDs containing "table" or "dt" are classified as "datatable"
#'   \item All other IDs default to "datatable" with a warning message
#' }
#'
#' @note
#' This is an internal function that provides basic auto-detection capabilities.
#' For more precise control over component types, use the explicit register_*
#' functions instead.
#'
#' @keywords internal
# Internal function to detect component type based on output ID patterns
detect_component_type <- function(component_id, data_reactive) {
  # Simple heuristic based on common naming patterns
  id_lower <- tolower(component_id)

  if (grepl("map|leaflet", id_lower)) {
    return("leaflet")
  } else if (grepl("table|dt", id_lower)) {
    return("datatable")
  } else {
    # Default assumption - could be made smarter
    warning(
      "Could not auto-detect type for '", component_id,
      "'. Assuming 'datatable'. Use register_* functions for explicit control."
    )
    return("datatable")
  }
}
