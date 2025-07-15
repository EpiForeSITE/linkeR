#' Simple Plot Linking Function
#'
#' A simple interface to link interactive plots and tables in Shiny.
#' This function automatically detects component types and sets up bidirectional linking.
#' Supports sf spatial objects for leaflet maps - coordinates will be automatically extracted.
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
#' \dontrun{
#' # Basic usage with default behaviors
#' link_plots(
#'   session,
#'   myMap = reactive({
#'     map_data
#'   }),
#'   myTable = reactive({
#'     table_data
#'   }),
#'   shared_id_column = "location_id"
#' )
#'
#' # With custom leaflet click behavior
#' link_plots(
#'   session,
#'   myMap = reactive({
#'     map_data
#'   }),
#'   myTable = reactive({
#'     table_data
#'   }),
#'   shared_id_column = "location_id",
#'   leaflet_click_handler = function(map_proxy, selected_data, session) {
#'     # Custom popup and zoom behavior
#'     map_proxy %>%
#'       leaflet::setView(lng = selected_data$longitude, lat = selected_data$latitude, zoom = 15) %>%
#'       leaflet::addPopups(
#'         lng = selected_data$longitude,
#'         lat = selected_data$latitude,
#'         popup = paste0("<b>", selected_data$name, "</b><br>Custom info here")
#'       )
#'   }
#' )
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
    } else {
      # validate that component has shared_id_column (existing validation)
      if (!shared_id_column %in% names(isolate(comp_data()))) {
        stop(
          "Component '", comp_name, "' data must contain the shared_id_column: ",
          shared_id_column
        )
      }
    }

    # Configure component-specific settings
    config <- list()
    if (comp_type == "leaflet") {
      config$lng_col <- leaflet_lng_col
      config$lat_col <- leaflet_lat_col
      config$highlight_zoom <- 12
      config$click_handler <- leaflet_click_handler
    } else if (comp_type == "datatable") {
      config$click_handler <- dt_click_handler
    }

    # Register component
    registry$register_component(
      component_id = comp_name,
      type = comp_type,
      data_reactive = comp_data,
      shared_id_column = shared_id_column,
      config = config
    )
  }

  message(
    "Linked ", length(components), " components: ",
    paste(names(components), collapse = ", ")
  )

  invisible(registry)
}

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
