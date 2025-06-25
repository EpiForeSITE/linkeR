#' Create a Link Registry
#'
#' Creates a registry to manage links between interactive components in Shiny.
#'
#' @param session The Shiny session object
#' @param on_selection_change Optional callback function that gets called when selection changes.
#'   Function should accept parameters: (selected_id, selected_data, source_component_id, session)
#' @return A registry object with methods for managing component links
#' @export
create_link_registry <- function(session, on_selection_change = NULL) {
  # Validate inputs
  if (missing(session)) {
    stop("session argument is required")
  }

  # Private registry state
  components <- list()
  shared_state <- shiny::reactiveValues()
  observers <- list()  # Store observers to prevent garbage collection

  # Registry methods
  registry <- list(
    # Register a new component
    register_component = function(component_id, type, data_reactive,
                                  shared_id_column, config = list()) {
      # Validation
      if (!is.character(component_id) || length(component_id) != 1) {
        stop("component_id must be a single character string")
      }
      if (!is.reactive(data_reactive)) {
        stop("data_reactive must be a reactive expression")
      }
      if (!is.character(shared_id_column) || length(shared_id_column) != 1) {
        stop("shared_id_column must be a single character string")
      }

      # Destroy existing observers for this component if they exist
      if (component_id %in% names(observers)) {
        for (obs in observers[[component_id]]) {
          if (!is.null(obs) && !is.null(obs$destroy)) {
            obs$destroy()
          }
        }
      }

      # Store component information
      components[[component_id]] <<- list(
        type = type,
        data_reactive = data_reactive,
        shared_id_column = shared_id_column,
        config = config
      )

      # Set up component-specific observers and store them
      observers[[component_id]] <<- setup_component_observers(
        component_id, type, session, components, shared_state, on_selection_change
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

        tryCatch({
          on_selection_change(selected_id, selected_data, source_component_id, session)
        }, error = function(e) {
          warning("Error in on_selection_change callback: ", e$message)
        })
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
      tryCatch({
        list(
          selected_id = shared_state$selected_id,
          selection_source = shared_state$selection_source
        )
      }, error = function(e) {
        list(selected_id = NULL, selection_source = NULL)
      })
    }
  )

  structure(registry, class = "link_registry")
}

# Internal function to set up observers for each component type
setup_component_observers <- function(component_id, type, session, components, shared_state, on_selection_change) {
  observers <- switch(type,
    "leaflet" = setup_leaflet_observers(component_id, session, components, shared_state, on_selection_change),
    "datatable" = setup_datatable_observers(component_id, session, components, shared_state, on_selection_change),
    stop("Unsupported component type: ", type)
  )

  return(observers)
}
