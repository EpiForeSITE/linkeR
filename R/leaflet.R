#' Register a Leaflet Component
#'
#' Register a Leaflet map for linking with other components.
#'
#' @param registry A link registry created by \code{create_link_registry()}
#' @param leaflet_output_id Character string: the outputId of your leafletOutput
#' @param data_reactive Reactive expression returning the data frame for the map
#' @param shared_id_column Character string: name of the ID column
#' @param lng_col Character string: name of longitude column (default: "longitude")
#' @param lat_col Character string: name of latitude column (default: "latitude")
#' @param highlight_zoom Numeric: zoom level when highlighting (default: 12)
#' @export
register_leaflet <- function(registry, leaflet_output_id, data_reactive,
                             shared_id_column, lng_col = "longitude",
                             lat_col = "latitude", highlight_zoom = 12) {
  # Check if leaflet is available
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package is required for leaflet linking. Please install it with: install.packages('leaflet')")
  }

  # Configure leaflet-specific settings
  config <- list(
    lng_col = lng_col,
    lat_col = lat_col,
    highlight_zoom = highlight_zoom,
    highlight_icon = leaflet::awesomeIcons(
      icon = "star",
      library = "glyphicon",
      markerColor = "red",
      iconColor = "#FFFFFF"
    )
  )

  # Register with the registry
  registry$register_component(
    component_id = leaflet_output_id,
    type = "leaflet",
    data_reactive = data_reactive,
    shared_id_column = shared_id_column,
    config = config
  )
}

# Implementation of leaflet-specific observers (internal function)
setup_leaflet_observers <- function(component_id, session, components, shared_state) {
  # Observer for map marker clicks
  observer1 <- shiny::observeEvent(session$input[[paste0(component_id, "_marker_click")]],
    {
      clicked_event <- session$input[[paste0(component_id, "_marker_click")]]
      shiny::req(clicked_event, !is.null(clicked_event$id))

      clicked_marker_id <- clicked_event$id

      # Update shared state
      shared_state$selected_id <- clicked_marker_id
      shared_state$selection_source <- component_id
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # Observer for responding to selections from other components
  observer2 <- shiny::observeEvent(shared_state$selected_id,
    {
      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        selected_id <- shared_state$selected_id
        update_leaflet_selection(component_id, selected_id, session, components)
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  return(list(observer1, observer2))
}

# Internal function to update leaflet selection
update_leaflet_selection <- function(component_id, selected_id, session, components) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    return()
  }

  component_info <- components[[component_id]]
  current_data <- component_info$data_reactive()

  # Validate required columns exist
  required_cols <- c(
    component_info$shared_id_column,
    component_info$config$lng_col,
    component_info$config$lat_col
  )

  if (!all(required_cols %in% names(current_data))) {
    warning(
      "Required columns missing from leaflet data: ",
      paste(setdiff(required_cols, names(current_data)), collapse = ", ")
    )
    return()
  }

  # Get map proxy
  map_proxy <- leaflet::leafletProxy(component_id, session = session)

  # Clear previous highlights
  highlight_group_name <- paste0("highlight_", component_id)
  map_proxy %>% leaflet::clearGroup(group = highlight_group_name)

  if (!is.null(selected_id)) {
    # Find the point to highlight
    point_to_highlight <- current_data[
      current_data[[component_info$shared_id_column]] == selected_id,
    ]

    if (nrow(point_to_highlight) > 0) {
      point_to_highlight <- point_to_highlight[1, ]

      # Add highlight marker and fly to location
      map_proxy %>%
        leaflet::addAwesomeMarkers(
          data = point_to_highlight,
          lng = ~ get(component_info$config$lng_col),
          lat = ~ get(component_info$config$lat_col),
          layerId = paste0("highlight_", selected_id),
          icon = component_info$config$highlight_icon,
          group = highlight_group_name,
          popup = ~ paste0("Selected: ", htmltools::htmlEscape(as.character(get(component_info$shared_id_column))))
        ) %>%
        leaflet::flyTo(
          lng = point_to_highlight[[component_info$config$lng_col]],
          lat = point_to_highlight[[component_info$config$lat_col]],
          zoom = component_info$config$highlight_zoom
        )
    }
  }
}
