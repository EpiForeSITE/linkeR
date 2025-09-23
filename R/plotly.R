#' Register a Plotly Component
#'
#' `register_plotly` registers a Plotly plot for linking with other components.
#' Supports both click and brush selections for enhanced interactivity.
#'
#' @param session Shiny session object. The session from the module where the plotly is used. This could be global session in non-modular apps.
#' @param registry A link registry created by [create_link_registry()]
#' @param plotly_output_id Character string: the outputId of your plotlyOutput
#' @param data_reactive Reactive expression returning the data frame for the plot
#' @param shared_id_column Character string: name of the ID column
#' @param source_id Character string: unique source identifier for the plotly plot (used in event_data calls)
#' @param click_handler Optional function: custom click handler for point selection, must have args (plot_proxy, selected_data, session), overrides all default behavior
#' @param brush_handler Optional function: custom brush handler for multiple selection, must have args (plot_proxy, selected_data, session), overrides all default behavior
#' @returns NULL (invisible). This function is called for its side effects of registering the component.
#' @export
#' @examples
#' \dontrun{
#'   # Create a mock session for the example
#'   session <- shiny::MockShinySession$new()
#'
#'   # Create a registry
#'   registry <- create_link_registry(session)
#'
#'   # Sample reactive data
#'   my_data <- shiny::reactive({
#'     data.frame(
#'       id = 1:5,
#'       name = c("A", "B", "C", "D", "E"),
#'       x_val = 1:5,
#'       y_val = c(2, 5, 3, 8, 7)
#'     )
#'   })
#'
#'   # Register a plotly component
#'   register_plotly(session, registry, "my_plot", my_data, "id", "my_plot_source")
#'
#'   # Verify registration
#'   print(registry$get_components())
#' }
register_plotly <- function(session, registry, plotly_output_id, data_reactive, shared_id_column,
                           source_id, click_handler = NULL, brush_handler = NULL) {
  # Check if plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for plotly linking. Please install it with: install.packages('plotly')")
  }

  if (is.null(registry) || !is.list(registry) || !("register_component" %in% names(registry))) {
    stop("registry must be a valid link registry created by create_link_registry()")
  }

  if (!is.character(plotly_output_id) || length(plotly_output_id) != 1) {
    stop("plotly_output_id must be a string")
  }

  if (!is.reactive(data_reactive)) {
    stop("data_reactive must be a reactive expression returning a data frame")
  }
  
  if (!is.character(source_id) || length(source_id) != 1) {
    stop("source_id must be a string")
  }

  # Register with the registry
  registry$register_component(
    session = session,
    component_id = plotly_output_id,
    type = "plotly",
    data_reactive = data_reactive,
    shared_id_column = shared_id_column,
    config = list(
      source_id = source_id,
      click_handler = click_handler,
      brush_handler = brush_handler
    )
  )
}

#' Setup Plotly Plot Observers
#'
#' `setup_plotly_observers` creates observers for handling Plotly plot interactions in a linked component system.
#' This includes observers for both click events (single selection) and brush events (multiple selection).
#'
#' @param component_id Character string. The unique identifier for the Plotly component.
#' @param session Shiny session object for the current user session.
#' @param components List containing component configuration data including data reactives
#'   and shared ID columns.
#' @param shared_state Reactive values object containing selected_id and selection_source
#'   for coordinating selections across components.
#' @param on_selection_change Function to call when selection changes (currently unused).
#' @param registry Optional registry object with set_selection method for managing
#'   selections. If NULL, falls back to direct shared_state updates.
#'
#' @return List containing observer objects for click and brush events
#'
#' @details
#' The click observer:
#' - Listens for plotly_click events using the component's source_id
#' - Extracts clicked point key/ID from the event data
#' - Updates selection state through registry or direct shared_state modification
#'
#' The brush observer:
#' - Listens for plotly_selected events (brush selections) using the component's source_id
#' - Extracts all selected point keys/IDs from the event data
#' - Updates multiple selection state through registry
#'
#' @examples
#' \dontrun{
#' observers <- setup_plotly_observers(
#'   component_id = "plot1",
#'   session = session,
#'   components = components_list,
#'   shared_state = shared_values,
#'   on_selection_change = NULL,
#'   registry = selection_registry
#' )
#' }
setup_plotly_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  component_info <- components[[component_id]]
  source_id <- component_info$config$source_id
  
  if (is.null(source_id)) {
    warning("source_id not found in plotly component config for component: ", component_id)
    return(list())
  }
  
  observers <- list()
  
  # Observer for plotly click events (single selection)
  observers$click_observer <- shiny::observeEvent(plotly::event_data("plotly_click", source = source_id),
    {
      clicked_data <- plotly::event_data("plotly_click", source = source_id)
      
      if (!is.null(clicked_data) && !is.null(clicked_data$key)) {
        clicked_id <- as.character(clicked_data$key)
        
        # Get the component data for custom handlers
        current_data <- component_info$data_reactive()
        selected_data <- current_data[current_data[[component_info$shared_id_column]] == clicked_id, ]
        selected_data <- if (nrow(selected_data) > 0) selected_data[1, ] else NULL
        
        # Apply click behavior
        if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
          # Custom click handler
          component_info$config$click_handler(NULL, selected_data, session)
        }
        
        # Update selection state
        if (!is.null(registry) && !is.null(registry$set_selection)) {
          registry$set_selection(clicked_id, component_id)
        } else {
          shared_state$selected_id <- clicked_id
          shared_state$selected_ids <- clicked_id
          shared_state$selection_source <- component_id
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # Observer for plotly brush/selection events (multiple selection)
  observers$brush_observer <- shiny::observeEvent(plotly::event_data("plotly_selected", source = source_id),
    {
      selected_data_event <- plotly::event_data("plotly_selected", source = source_id)
      
      if (!is.null(selected_data_event) && !is.null(selected_data_event$key)) {
        selected_ids <- as.character(selected_data_event$key)
        
        # Get the component data for custom handlers
        current_data <- component_info$data_reactive()
        selected_data <- current_data[current_data[[component_info$shared_id_column]] %in% selected_ids, ]
        
        # Apply brush behavior
        if (!is.null(component_info$config$brush_handler) && is.function(component_info$config$brush_handler)) {
          # Custom brush handler
          component_info$config$brush_handler(NULL, selected_data, session)
        }
        
        # Update selection state
        if (length(selected_ids) > 1) {
          # Multiple selections - use new multiple selection method
          if (!is.null(registry) && !is.null(registry$set_multiple_selection)) {
            registry$set_multiple_selection(selected_ids, component_id)
          } else if (!is.null(registry) && !is.null(registry$set_selection)) {
            # Fallback to single selection (first item) for backward compatibility
            registry$set_selection(selected_ids[1], component_id)
          } else {
            shared_state$selected_id <- selected_ids[1]
            shared_state$selected_ids <- selected_ids
            shared_state$selection_source <- component_id
          }
        } else if (length(selected_ids) == 1) {
          # Single selection
          if (!is.null(registry) && !is.null(registry$set_selection)) {
            registry$set_selection(selected_ids[1], component_id)
          } else {
            shared_state$selected_id <- selected_ids[1]
            shared_state$selected_ids <- selected_ids[1]
            shared_state$selection_source <- component_id
          }
        } else {
          # No selection / deselection
          if (!is.null(registry) && !is.null(registry$set_multiple_selection)) {
            registry$set_multiple_selection(character(0), component_id)
          } else if (!is.null(registry) && !is.null(registry$set_selection)) {
            registry$set_selection(NULL, component_id)
          } else {
            shared_state$selected_id <- NULL
            shared_state$selected_ids <- character(0)
            shared_state$selection_source <- component_id
          }
        }
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )
  
  # Observer for responding to selections from other components (visual updates)
  observers$response_observer <- shiny::observeEvent(shared_state$selected_id,
    {
      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        # Note: Plotly doesn't provide built-in methods to programmatically update selections
        # Users can implement custom visual feedback in their click/brush handlers if needed
        # This observer is here for consistency with other components
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  
  # Observer for responding to multiple selections from other components
  observers$multiple_response_observer <- shiny::observeEvent(shared_state$selected_ids,
    {
      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id &&
        !is.null(shared_state$selected_ids)) {
        # Note: Plotly doesn't provide built-in methods to programmatically update selections
        # Users can implement custom visual feedback in their click/brush handlers if needed
        # This observer is here for consistency with other components
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  
  return(observers)
}