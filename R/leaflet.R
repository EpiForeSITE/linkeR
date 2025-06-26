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

setup_leaflet_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  # Observer for map marker clicks
  observer1 <- shiny::observeEvent(session$input[[paste0(component_id, "_marker_click")]],
    {
      clicked_event <- session$input[[paste0(component_id, "_marker_click")]]
      shiny::req(clicked_event, !is.null(clicked_event$id))

      clicked_marker_id <- clicked_event$id

      # Get the selected data
      component_info <- components[[component_id]]
      current_data <- component_info$data_reactive()
      selected_data <- current_data[current_data[[component_info$shared_id_column]] == clicked_marker_id, ]
      selected_data <- if (nrow(selected_data) > 0) selected_data[1, ] else NULL

      # Get map proxy and clear any existing popups
      map_proxy <- leaflet::leafletProxy(component_id, session = session)
      map_proxy %>% leaflet::clearPopups()

      # Apply the same behavior as linked clicks for consistency
      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        # Custom handler
        component_info$config$click_handler(map_proxy, selected_data, session)
      } else {
        # Default handler - same as what linked clicks use
        apply_default_leaflet_behavior(map_proxy, selected_data, component_info)
      }

      if (!is.null(registry) && !is.null(registry$set_selection)) {
        registry$set_selection(clicked_marker_id, component_id)
      } else {
        # Fallback to direct update if registry not available, this won't trigger selection callback functions
        shared_state$selected_id <- clicked_marker_id
        shared_state$selection_source <- component_id
      }
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

        # For linked selections, use the update function
        update_leaflet_selection(component_id, selected_id, session, components)
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  return(list(observer1, observer2))
}

# Helper function for consistent default leaflet behavior
apply_default_leaflet_behavior <- function(map_proxy, selected_data, component_info) {
  if (!is.null(selected_data)) {
    # Create default popup content
    popup_content <- paste0("<b>Selected</b><br>")
    popup_content <- paste0(popup_content, "ID: ", selected_data[[component_info$shared_id_column]], "<br>")

    # Add the popup and fly to location
    map_proxy %>%
      leaflet::setView(
        lng = selected_data[[component_info$config$lng_col]],
        lat = selected_data[[component_info$config$lat_col]],
        zoom = component_info$config$highlight_zoom
      ) %>%
      leaflet::addPopups(
        lng = selected_data[[component_info$config$lng_col]],
        lat = selected_data[[component_info$config$lat_col]],
        popup = popup_content
      )
  } else {
    # Handle deselection
    map_proxy %>% leaflet::clearPopups()
  }
}

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

  # Clear existing popups first
  map_proxy %>% leaflet::clearPopups()

  if (!is.null(selected_id)) {
    # Find the selected data
    selected_data <- current_data[current_data[[component_info$shared_id_column]] == selected_id, ]
    selected_data <- if (nrow(selected_data) > 0) selected_data[1, ] else NULL

    # Use user's custom click handler if provided
    if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
      # Call the user's custom handler
      component_info$config$click_handler(map_proxy, selected_data, session)
    } else {
      # Use consistent default behavior
      apply_default_leaflet_behavior(map_proxy, selected_data, component_info)
    }
  } else {
    # Handle deselection
    if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
      # Let user's handler deal with deselection
      component_info$config$click_handler(map_proxy, NULL, session)
    } else {
      # Default deselection (already cleared popups above)
    }
  }
}
