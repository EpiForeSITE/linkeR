#' Register a Leaflet Component
#'
#' `register_leaflet` registers a Leaflet map for linking with other components.
#'
#' @param registry A link registry created by \code{create_link_registry()}
#' @param leaflet_output_id Character string: the outputId of your leafletOutput
#' @param data_reactive Reactive expression returning the data frame for the map
#' @param shared_id_column Character string: name of the ID column
#' @param lng_col Character string: name of longitude column (default: "longitude")
#' @param lat_col Character string: name of latitude column (default: "latitude")
#' @param highlight_zoom Numeric: zoom level when highlighting (default: 12)
#' @param click_handler Optional function: custom click handler for row selection
#' @export
register_leaflet <- function(registry, leaflet_output_id, data_reactive,
                             shared_id_column, lng_col = "longitude",
                             lat_col = "latitude", highlight_zoom = 12,
                             click_handler = NULL) {
  # Check if leaflet is available
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("leaflet package is required for leaflet linking. Please install it with: install.packages('leaflet')")
  }

  # Validate registry
  if (is.null(registry) || !is.list(registry) || !("register_component" %in% names(registry))) {
    stop("registry must be a valid link registry created by create_link_registry()")
  }

  if (!is.character(leaflet_output_id) || length(leaflet_output_id) != 1) {
    stop("leaflet_output_id must be a string")
  }

  if (!is.reactive(data_reactive)) {
    stop("data_reactive must be a reactive expression returning a data frame")
  }

  processed_data_reactive <- reactive({
    data <- data_reactive()  # Get data from the reactive
    process_sf_data(data, lng_col, lat_col)
  })

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
    ),
    original_data_reactive = data_reactive,  # Keep reference to original for custom handlers
    click_handler = click_handler  # ADD THIS LINE
  )

  # Register with the registry using processed data
  registry$register_component(
    component_id = leaflet_output_id,
    type = "leaflet",
    data_reactive = processed_data_reactive,
    shared_id_column = shared_id_column,
    config = config
  )
}

#' Setup Leaflet Map Observers
#'
#' `setup_leaflet_observers` creates two observers for handling Leaflet map interactions in a linked component system.
#' The first observer handles direct marker clicks on the map, while the second observer
#' responds to selection changes from other linked components.
#'
#' @param component_id Character string. The unique identifier for the Leaflet component.
#' @param session Shiny session object for the current user session.
#' @param components List containing component configuration data including data reactives
#'   and shared ID columns.
#' @param shared_state Reactive values object containing selected_id and selection_source
#'   for coordinating selections across components.
#' @param on_selection_change Function to call when selection changes (currently unused).
#' @param registry Optional registry object with set_selection method for managing
#'   selections. If NULL, falls back to direct shared_state updates.
#'
#' @return List containing two observer objects:
#'   \item{observer1}{Handles marker click events on the map}
#'   \item{observer2}{Responds to selection changes from other components}
#'
#' @details
#' The marker click observer:
#' - Extracts clicked marker ID from the click event
#' - Retrieves corresponding data row from the component's data
#' - Clears existing popups and applies click behavior (custom or default)
#' - Updates selection state through registry or direct shared_state modification
#'
#' The selection response observer:
#' - Only responds to selections from other components (not self-selections)
#' - Updates the map visualization to reflect the new selection
#'
#' @examples
#' \dontrun{
#' observers <- setup_leaflet_observers(
#'   component_id = "map1",
#'   session = session,
#'   components = components_list,
#'   shared_state = shared_values,
#'   on_selection_change = NULL,
#'   registry = selection_registry
#' )
#' }
setup_leaflet_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  # Observer for map marker clicks
  observer1 <- shiny::observeEvent(session$input[[paste0(component_id, "_marker_click")]],
    {
      clicked_event <- session$input[[paste0(component_id, "_marker_click")]]
      shiny::req(clicked_event, !is.null(clicked_event$id))

      clicked_marker_id <- clicked_event$id

      # Get the component info
      component_info <- components[[component_id]]
      
      # ALWAYS use the processed data for selection
      current_data <- component_info$data_reactive()  # This is the processed data

      selected_data <- current_data[current_data[[component_info$shared_id_column]] == clicked_marker_id, ]
      selected_data <- if (nrow(selected_data) > 0) selected_data[1, ] else NULL

      # Get map proxy and clear any existing popups
      map_proxy <- leaflet::leafletProxy(component_id, session = session)
      map_proxy %>% leaflet::clearPopups()

      handler_selected_data <- selected_data
      
      # Apply click behavior
      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        # Custom handler gets processed data (with lng/lat columns)
        component_info$config$click_handler(map_proxy, handler_selected_data, session)
      } else {
        # Default handler with processed data
        apply_default_leaflet_behavior(map_proxy, selected_data, component_info)
      }

      if (!is.null(registry) && !is.null(registry$set_selection)) {
        registry$set_selection(clicked_marker_id, component_id)
      } else {
        # Fallback to direct update if registry not available
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

        # Use the processed data for updates
        update_leaflet_selection(component_id, selected_id, session, components)
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  return(list(observer1, observer2))
}

#' Apply Default Leaflet Behavior for Selection Events
#'
#' `apply_default_leaflet_behavior` is a helper function that provides consistent default behavior for leaflet maps
#' when handling selection events. It manages popup display and map navigation
#' based on the selection state.
#'
#' @param map_proxy A leaflet map proxy object used to update the map
#' @param selected_data A data frame or list containing the selected row/item data.
#'   If NULL, indicates deselection occurred.
#' @param component_info A list containing component configuration information:
#'   \describe{
#'     \item{shared_id_column}{Character. Name of the column containing unique identifiers}
#'     \item{config}{List containing:
#'       \describe{
#'         \item{lng_col}{Character. Name of the longitude column}
#'         \item{lat_col}{Character. Name of the latitude column}
#'         \item{highlight_zoom}{Numeric. Zoom level to use when highlighting selection}
#'       }
#'     }
#'   }
#'
#' @return Returns the modified map proxy object with updated view and popups
#'
#' @details
#' When \code{selected_data} is provided:
#' \itemize{
#'   \item Creates a popup with "Selected" header and ID information
#'   \item Sets map view to the selected location coordinates
#'   \item Applies the configured highlight zoom level
#' }
#' When \code{selected_data} is NULL (deselection):
#' \itemize{
#'   \item Removes all existing popups from the map
#' }
#'
#' @examples
#' \dontrun{
#' # Apply default behavior when item is selected
#' apply_default_leaflet_behavior(map_proxy, selected_row, component_info)
#'
#' # Apply default behavior when item is deselected
#' apply_default_leaflet_behavior(map_proxy, NULL, component_info)
#' }
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

#' Update Leaflet Map Selection
#'
#' `update_leaflet_selection` updates a Leaflet map component to reflect a new selection state. This function
#' handles both selection and deselection events, applying either custom user-defined
#' click handlers or default behaviors.
#'
#' @param component_id Character string. The ID of the Leaflet map component to update.
#' @param selected_id Character string or NULL. The ID of the selected item. If NULL,
#'   indicates deselection.
#' @param session Shiny session object. The current Shiny session.
#' @param components List. A named list containing component information, where each
#'   element contains component configuration including data_reactive, shared_id_column,
#'   and config settings.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Validates that the leaflet package is available
#'   \item Checks that required columns (shared_id_column, lng_col, lat_col) exist in the data
#'   \item Clears existing popups on the map
#'   \item For selections: finds the selected data row and applies either custom click handler or default behavior
#'   \item For deselections: delegates to custom handler or performs default cleanup
#' }
#'
#' Required columns in the component data:
#' \itemize{
#'   \item \code{shared_id_column}: Column containing unique identifiers for map features
#'   \item \code{lng_col}: Column containing longitude coordinates
#'   \item \code{lat_col}: Column containing latitude coordinates
#' }
#'
#' @return NULL (invisibly). The function is called for its side effects on the Leaflet map.
#'
#' @note If the leaflet package is not available, the function returns early without error.
#'   Missing required columns will generate a warning and cause early return.
update_leaflet_selection <- function(component_id, selected_id, session, components) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    return()
  }

  component_info <- components[[component_id]]
  current_data <- component_info$data_reactive()

  # For sf integration: ensure we have processed data with lng/lat columns
  if (!all(c(component_info$config$lng_col, component_info$config$lat_col) %in% names(current_data))) {
    # Try to process the data if it's an sf object
    if (requireNamespace("sf", quietly = TRUE) && inherits(current_data, "sf")) {
      current_data <- process_sf_data(current_data, component_info$config$lng_col, component_info$config$lat_col)
    } else {
      # Validate required columns exist
      required_cols <- c(
        component_info$shared_id_column,
        component_info$config$lng_col,
        component_info$config$lat_col
      )
      
      missing_cols <- setdiff(required_cols, names(current_data))
      if (length(missing_cols) > 0) {
        warning(
          "Required columns missing from leaflet data: ",
          paste(missing_cols, collapse = ", "),
          ". Available columns: ", paste(names(current_data), collapse = ", ")
        )
        return()
      }
    }
  }

  # Get map proxy
  map_proxy <- leaflet::leafletProxy(component_id, session = session)

  # Clear existing popups first
  map_proxy %>% leaflet::clearPopups()

  if (!is.null(selected_id)) {
    # Find the selected data
    selected_data <- current_data[current_data[[component_info$shared_id_column]] == selected_id, ]
    selected_data <- if (nrow(selected_data) > 0) selected_data[1, ] else NULL

    # For custom handlers, prepare data appropriately
    handler_selected_data <- selected_data
    if (!is.null(component_info$config$click_handler) && 
        !is.null(component_info$config$original_data_reactive)) {
      # Get original data for custom handlers that might expect sf structure
      original_data <- component_info$config$original_data_reactive()

      if (inherits(original_data, "sf")) {

        # Add coordinates to original data for handlers
        coords <- sf::st_coordinates(original_data)
        original_data$longitude <- coords[, 1]
        original_data$latitude <- coords[, 2]
        original_data <- sf::st_drop_geometry(original_data)
      }
      handler_row <- original_data[original_data[[component_info$shared_id_column]] == selected_id, ]
      if (nrow(handler_row) > 0) {
        handler_selected_data <- handler_row[1, ]
      }
    }

    # Use user's custom click handler if provided
    if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
      # Call the user's custom handler
      component_info$config$click_handler(map_proxy, handler_selected_data, session)
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

#' Process SF Data for Leaflet Integration
#'
#' `process_sf_data` is a helper function to extract coordinates from an sf object or ensure lng/lat columns exist in a data frame.
#'
#' @param data Data frame or sf object. If sf object, coordinates will be extracted.
#' @param lng_col Character string. Name for the longitude column (default: "longitude")
#' @param lat_col Character string. Name for the latitude column (default: "latitude")
#'
#' @return Data frame with explicit lng/lat columns, ready for leaflet integration
#'
#' @details
#' This function handles three scenarios:
#' \itemize{
#'   \item SF objects: Extracts coordinates using sf::st_coordinates() and creates lng/lat columns
#'   \item Regular data frames with existing lng/lat columns: Returns unchanged
#'   \item Regular data frames without lng/lat columns: Issues warning and returns unchanged
#' }
#'
#' For sf objects, the function:
#' \itemize{
#'   \item Extracts point coordinates from the geometry column
#'   \item Adds coordinates as new columns with the specified names
#'   \item Preserves the original geometry column for advanced spatial operations
#'   \item Returns an sf object with both geometry and coordinate columns
#' }
#'
#' @examples
#' \dontrun{
#' # Process sf object
#' sf_data <- sf::st_read("path/to/shapefile.shp")
#' processed <- process_sf_data(sf_data, "lon", "lat")
#'
#' # Regular data frame passes through unchanged
#' regular_df <- data.frame(id = 1:3, longitude = c(-74, -75, -76), latitude = c(40, 41, 42))
#' processed <- process_sf_data(regular_df, "longitude", "latitude")
#' }
#' 
#' @keywords internal
#' @export
process_sf_data <- function(data, lng_col = "longitude", lat_col = "latitude") {
  # Check if this is an sf object
  if (requireNamespace("sf", quietly = TRUE) && inherits(data, "sf")) {
    # Handle sf objects
    tryCatch({

      # For POLYGON/LINESTRING geometries, use centroids for coordinates
      if (any(sf::st_geometry_type(data) != "POINT")) {
        message("Non-POINT geometries detected. Using centroids for coordinates.")
        centroids <- sf::st_centroid(data)
        coords <- sf::st_coordinates(centroids)
      } else {
        coords <- sf::st_coordinates(data)
      }
      
      # Check if we have point geometries (X, Y coordinates)
      if (ncol(coords) < 2) {
        warning("SF object does not appear to contain point geometries with X/Y coordinates")
        return(data)
      }
      
      # Keep the original sf object but add coordinate columns
      data[[lng_col]] <- as.numeric(coords[, 1])  # X = longitude
      data[[lat_col]] <- as.numeric(coords[, 2])   # Y = latitude

      return(data)
    }, error = function(e) {
      warning("Failed to process sf object: ", e$message, ". Returning original data.")
      return(data)
    })
    
  } else {
    # Regular data frame - check if lng/lat columns exist
    if (lng_col %in% names(data) && lat_col %in% names(data)) {
      # Already has the required columns - ensure they're numeric
      data[[lng_col]] <- as.numeric(data[[lng_col]])
      data[[lat_col]] <- as.numeric(data[[lat_col]])
      return(data)
    } else {
      # Missing required columns
      missing_cols <- setdiff(c(lng_col, lat_col), names(data))
      warning(
        "Data is missing required coordinate columns: ", 
        paste(missing_cols, collapse = ", "),
        ". Available columns: ", paste(names(data), collapse = ", ")
      )
      return(data)
    }
  }
}
