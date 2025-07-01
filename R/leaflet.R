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

#' Setup Leaflet Map Observers
#'
#' Creates two observers for handling Leaflet map interactions in a linked component system.
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

#' Apply Default Leaflet Behavior for Selection Events
#'
#' This helper function provides consistent default behavior for leaflet maps
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
#' Updates a Leaflet map component to reflect a new selection state. This function
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
