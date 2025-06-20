#' Simple Plot Linking Function
#'
#' A simple interface to link interactive plots and tables in Shiny.
#' This function automatically detects component types and sets up bidirectional linking.
#'
#' @param session The Shiny session object
#' @param ... Named arguments where names are component output IDs and values are
#'   reactive data frames. Each data frame must contain the shared_id_column.
#' @param shared_id_column Character string naming the column that contains unique
#'   identifiers present in all linked components.
#' @param leaflet_lng_col Character string naming the longitude column for leaflet maps.
#'   Defaults to "longitude".
#' @param leaflet_lat_col Character string naming the latitude column for leaflet maps.
#'   Defaults to "latitude".
#' @return Invisibly returns the created registry object
#' @export
#' @examples
#' \dontrun{
#' # In your Shiny server function
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
#' }
link_plots <- function(session, ..., shared_id_column,
                       leaflet_lng_col = "longitude",
                       leaflet_lat_col = "latitude") {
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

  # Create registry
  registry <- create_link_registry(session)

  # Register each component
  for (comp_name in names(components)) {
    comp_data <- components[[comp_name]]

    # Detect component type (this is a simplified heuristic)
    comp_type <- detect_component_type(comp_name, comp_data)

    # Configure component-specific settings
    config <- list()
    if (comp_type == "leaflet") {
      config$lng_col <- leaflet_lng_col
      config$lat_col <- leaflet_lat_col
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
