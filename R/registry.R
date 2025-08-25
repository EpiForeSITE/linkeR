#' Create a Link Registry for Shiny Component Coordination
#'
#' `create_link_registry` creates a registry system that manages linked interactions between multiple
#' Shiny components, allowing them to share selection state and coordinate
#' their behavior.
#'
#' @param session A Shiny session object, required for server-side reactivity
#' @param on_selection_change Optional callback function that gets called when
#'   selection changes. Should accept parameters: selected_id, selected_data,
#'   source_component_id, and session
#'
#' @return A link_registry object with the following methods:
#' \describe{
#'   \item{register_component(session, component_id, type, data_reactive, shared_id_column, config)}{
#'     Register a new component with the registry. Parameters:
#'     \itemize{
#'       \item session: Shiny session object for namespacing. Can be global session in non-modular apps.
#'       \item component_id: Unique string identifier for the component
#'       \item type: Component type (e.g., "table", "plot")
#'       \item data_reactive: Reactive expression returning the component's data
#'       \item shared_id_column: Name of the column used for linking selections
#'       \item config: Optional list of component-specific configuration
#'     }
#'   }
#'   \item{clear_all()}{Remove all registered components and reset shared state}
#'   \item{set_selection(selected_id, source_component_id)}{
#'     Programmatically update the selection state
#'   }
#'   \item{get_selection()}{Get current selection as list with selected_id and source}
#'   \item{get_on_selection_change()}{Return the on_selection_change callback function}
#'   \item{get_components()}{Get registry components info (for debugging)}
#'   \item{get_shared_state()}{Get current shared state (for debugging)}
#' }
#'
#' @details
#' The registry maintains a shared state across all registered components,
#' automatically setting up observers to synchronize selections. When a
#' selection changes in one component, all other registered components
#' are updated to reflect the same selection.
#'
#' Components are automatically cleaned up when re-registered to prevent
#' memory leaks from orphaned observers.
#'
#' @export
#' @examples
#' \dontrun{
#' # In your Shiny server function
#' registry <- create_link_registry(
#'   session = session,
#'   on_selection_change = function(id, data, source, session) {
#'     message("Selection changed to ID: ", id, " from: ", source)
#'   }
#' )
#'
#' # Register components
#' registry$register_component("table1", "table", reactive(my_data), "id")
#' registry$register_component("plot1", "plot", reactive(my_data), "id")
#' }
#'
#' @seealso [setup_component_observers()] for component observer setup
create_link_registry <- function(session, on_selection_change = NULL) {
  # Validate inputs
  if (missing(session)) {
    stop("session argument is required")
  }

  # Capture the main app session with an unambiguous name
  top_level_session <- session

  # Private registry state
  components <- list()
  shared_state <- shiny::reactiveValues()
  observers <- list() # Store observers to prevent garbage collection

  # Registry methods
  registry <- list(
    # Register a new component
    register_component = function(session, component_id, type, data_reactive,
                                  shared_id_column, config = list()) {
      # Validation
      if (!is.character(component_id) || length(component_id) != 1) {
        stop("component_id must be a string")
      }
      if (!is.reactive(data_reactive)) {
        stop("data_reactive must be a reactive expression")
      }
      if (!is.character(shared_id_column) || length(shared_id_column) != 1) {
        stop("shared_id_column must be a string")
      }

      namespaced_id <- session$ns(component_id)

      # Destroy existing observers for this component if they exist
      if (namespaced_id %in% names(observers)) {
        for (obs in observers[[namespaced_id]]) {
          if (!is.null(obs) && !is.null(obs$destroy)) {
            obs$destroy()
          }
        }
      }

      # Store component information
      components[[namespaced_id]] <<- list(
        type = type,
        data_reactive = data_reactive,
        shared_id_column = shared_id_column,
        config = config
      )

      # Set up component-specific observers and store them
      observers[[namespaced_id]] <<- setup_component_observers(
        namespaced_id, type, top_level_session, components, shared_state, on_selection_change,
        registry = list(set_selection = registry$set_selection)
      )

      invisible(TRUE)
    },

    # Clear all observers (useful when switching configurations)
    clear_all = function() {
      for (comp_observers in observers) {
        for (obs in comp_observers) {
          if (!is.null(obs) && !is.null(obs$destroy)) {
            obs$destroy()
          }
        }
      }
      observers <<- list()
      components <<- list()
      # Reset shared state
      shared_state$selected_id <<- NULL
      shared_state$selection_source <<- NULL
    },

    # Update selection programmatically
    set_selection = function(selected_id, source_component_id = "programmatic") {
      # Only proceed if selection actually changed
      if (identical(shared_state$selected_id, selected_id) &&
        identical(shared_state$selection_source, source_component_id)) {
        return(invisible(NULL)) # No change, exit early
      }

      shared_state$selected_id <- selected_id
      shared_state$selection_source <- source_component_id

      # Call user callback if provided
      if (!is.null(on_selection_change) && is.function(on_selection_change)) {
        # Get selected data
        selected_data <- NULL
        if (!is.null(selected_id)) {
          # Find the data from any component (they should all have the same ID)
          for (comp_id in names(components)) {
            comp_data <- components[[comp_id]]$data_reactive()
            match_row <- comp_data[comp_data[[components[[comp_id]]$shared_id_column]] == selected_id, ]
            if (nrow(match_row) > 0) {
              selected_data <- match_row[1, ]
              break
            }
          }
        }

        tryCatch(
          {
            on_selection_change(selected_id, selected_data, source_component_id, top_level_session)
          },
          error = function(e) {
            warning("Error in on_selection_change callback: ", e$message)
          }
        )
      }
    },

    # Get currently selected ID
    get_selection = function() {
      list(
        selected_id = shared_state$selected_id,
        source = shared_state$selection_source
      )
    },

    # Get on_selection_change callback
    get_on_selection_change = function() {
      on_selection_change
    },

    # Get registry information (for debugging)
    get_components = function() {
      # Return a simplified version to avoid environment issues
      lapply(components, function(comp) {
        list(
          type = comp$type,
          shared_id_column = comp$shared_id_column,
          config = comp$config
          # Don't return data_reactive as it's an environment
        )
      })
    },

    # Get shared state (for debugging)
    get_shared_state = function() {
      tryCatch(
        {
          list(
            selected_id = shared_state$selected_id,
            selection_source = shared_state$selection_source
          )
        },
        error = function(e) {
          list(selected_id = NULL, selection_source = NULL)
        }
      )
    }
  )

  structure(registry, class = "link_registry")
}

#' Set up observers for different component types
#'
#' `setup_component_observers` is an internal function that creates and configures observers for different types of
#' interactive components based on the specified component type. It acts as a
#' dispatcher that calls the appropriate setup function for each supported component.
#'
#' @param component_id Character string. Unique identifier for the component.
#' @param type Character string. The type of component to set up observers for.
#'   Currently supports "leaflet" and "datatable".
#' @param session Shiny session object. The current Shiny session.
#' @param components List. Collection of all components in the application.
#' @param shared_state Reactive values object. Shared state across components.
#' @param on_selection_change Function. Callback function to execute when
#'   selection changes occur.
#' @param registry Optional. Registry object for component management. Default is NULL.
#'
#' @return List of observer objects created for the specified component type. Throws Error if an unsupported component type is provided.
#'
#' @keywords internal
# Internal function to set up observers for each component type
setup_component_observers <- function(component_id, type, session, components, shared_state, on_selection_change, registry = NULL) {
  observers <- switch(type,
    "leaflet" = setup_leaflet_observers(component_id, session, components, shared_state, on_selection_change, registry),
    "datatable" = setup_datatable_observers(component_id, session, components, shared_state, on_selection_change, registry),
    stop("Unsupported component type: ", type)
  )

  return(observers)
}
