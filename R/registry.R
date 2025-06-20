#' Create a Link Registry
#'
#' Creates a registry to manage links between interactive components in Shiny.
#'
#' @param session The Shiny session object
#' @return A registry object with methods for managing component links
#' @export
#' @examples
#' \dontrun{
#' # In your Shiny server function
#' registry <- create_link_registry(session)
#' }
create_link_registry <- function(session) {
  # Validate inputs
  if (missing(session)) {
    stop("session argument is required")
  }

  # Private registry state
  components <- list()
  shared_state <- shiny::reactiveValues()
  observers <- list()

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

      # Store component information
      components[[component_id]] <<- list(
        type = type,
        data_reactive = data_reactive,
        shared_id_column = shared_id_column,
        config = config
      )

      # Set up component-specific observers
      setup_component_observers(component_id, type, session, components, shared_state)

      invisible(TRUE)
    },

    # Get registry information (for debugging)
    get_components = function() {
      components
    },

    # Get shared state (for debugging)
    get_shared_state = function() {
      shiny::reactiveValuesToList(shared_state)
    }
  )

  structure(registry, class = "link_registry")
}

# Internal function to set up observers for each component type
setup_component_observers <- function(component_id, type, session, components, shared_state) {
  switch(type,
    "leaflet" = setup_leaflet_observers(component_id, session, components, shared_state),
    "datatable" = setup_datatable_observers(component_id, session, components, shared_state),
    stop("Unsupported component type: ", type)
  )
}
