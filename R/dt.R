#' Register a DT DataTable Component
#'
#' Register a DT datatable for linking with other components.
#'
#' @param registry A link registry created by \code{create_link_registry()}
#' @param dt_output_id Character string: the outputId of your DT::DTOutput
#' @param data_reactive Reactive expression returning the data frame for the table
#' @param shared_id_column Character string: name of the ID column
#' @param click_handler Optional function: custom click handler for row selection
#' @export
register_dt <- function(registry, dt_output_id, data_reactive, shared_id_column,
                        click_handler = NULL) {
  # Check if DT is available
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT package is required for DT table linking. Please install it with: install.packages('DT')")
  }

  if (is.null(registry) || !is.list(registry) || !("register_component" %in% names(registry))) {
    stop("registry must be a valid link registry created by create_link_registry()")
  }

  if (!is.character(dt_output_id) || length(dt_output_id) != 1) {
    stop("dt_output_id must be a string")
  }

  if (!is.reactive(data_reactive)) {
    stop("data_reactive must be a reactive expression returning a data frame")
  }

  # Register with the registry
  registry$register_component(
    component_id = dt_output_id,
    type = "datatable",
    data_reactive = data_reactive,
    shared_id_column = shared_id_column,
    config = list(
      click_handler = click_handler
    )
  )
}

# Implementation of DT-specific observers (internal function)
#' Setup DataTable Observers
#'
#' Sets up reactive observers for a DataTable component to handle user interactions
#' and state changes. This function establishes the necessary event handlers for
#' selection changes and synchronizes the component with the shared application state.
#'
#' @param component_id Character string. Unique identifier for the DataTable component.
#' @param session Shiny session object. The current Shiny session for reactive context.
#' @param components List. Collection of UI components in the application.
#' @param shared_state Reactive values object. Shared state container for cross-component communication.
#' @param on_selection_change Function. Callback function to execute when table selection changes.
#' @param registry List or NULL. Optional registry for component management. Defaults to NULL.
#'
#' @return NULL. This function is called for its side effects of setting up observers.
#'
#' @details
#' This function creates reactive observers that monitor DataTable interactions and
#' update the shared state accordingly. It handles selection events and ensures
#' proper synchronization between the DataTable component and other application components.
#'
#' @examples
#' \dontrun{
#' setup_datatable_observers(
#'   component_id = "my_table",
#'   session = session,
#'   components = ui_components,
#'   shared_state = app_state,
#'   on_selection_change = function(selected_rows) {
#'     # Handle selection change
#'   }
#' )
#' }
setup_datatable_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  # Use session userData to store the flag - this persists across observer calls
  flag_name <- paste0(component_id, "_updating_selection")

  # The flag exists to prevent infinite loops when multiple components interact.
  # For example, when user clicks Map A:
    # 1. Map A calls registry$set_selection("BIZ_123", "map_a")
    # 2. This triggers DT's observer2 (response observer)
    # 3. DT sets: session$userData[["table_updating_selection"]] <- TRUE
    # 4. DT calls: DT::selectRows(proxy, selected = row_number)
    # 5. DT selection event fires, but observer1 checks the flag
    # 6. Since flag is TRUE, observer1 returns early (no infinite loop!)
    # 7. After 100ms: flag is reset to FALSE

  # Observer for table row selections (USER CLICKS ONLY)
  observer1 <- shiny::observeEvent(session$input[[paste0(component_id, "_rows_selected")]],
    {
      # Check session-level flag
      if (isTRUE(session$userData[[flag_name]])) {
        return()
      }

      selected_rows <- session$input[[paste0(component_id, "_rows_selected")]]

      if (length(selected_rows) > 0) {
        # Get the component info
        component_info <- components[[component_id]]
        current_data <- component_info$data_reactive()

        # Get the selected ID (use first selection if multiple)
        selected_id <- current_data[[component_info$shared_id_column]][selected_rows[1]]

        # THIS IS CORRECT - USER CLICK SHOULD CALL set_selection
        if (!is.null(registry) && !is.null(registry$set_selection)) {
          registry$set_selection(selected_id, component_id)
        } else {
          shared_state$selected_id <- selected_id
          shared_state$selection_source <- component_id
        }
      } else {
        # Clear selection
        if (!is.null(registry) && !is.null(registry$set_selection)) {
          registry$set_selection(NULL, component_id)
        } else {
          shared_state$selected_id <- NULL
          shared_state$selection_source <- component_id
        }
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # Observer for responding to selections from other components (VISUAL UPDATES ONLY)
  observer2 <- shiny::observeEvent(shared_state$selected_id,
    {
      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        selected_id <- shared_state$selected_id

        # Set session-level flag to prevent recursive calls
        session$userData[[flag_name]] <- TRUE

        # THIS SHOULD ONLY UPDATE VISUAL STATE - NO set_selection CALLS!
        update_dt_selection(component_id, selected_id, session, components)

        # Reset flag after a short delay to allow DT event to be processed and ignored
        later::later(function() {
          session$userData[[flag_name]] <- FALSE
        }, delay = 0.1) # 100ms delay
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  return(list(observer1, observer2))
}

# Simplify update_dt_selection back to basic version:
#' Update DT Selection Based on Shared ID
#'
#' Updates the selection state of a DataTable (DT) component when a shared ID
#' is selected or deselected from another linked component. This function handles
#' both custom click handlers and default selection behavior.
#'
#' @param component_id Character string. The ID of the DT component to update.
#' @param selected_id The shared ID value to select. If NULL, deselects all rows.
#' @param session Shiny session object for the current user session.
#' @param components List containing component configuration information, including
#'   data reactives, shared ID columns, and optional custom click handlers.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Validates that the DT package is available
#'   \item Retrieves current data from the component's reactive data source
#'   \item Validates that the shared ID column exists in the data
#'   \item Creates a DT proxy for programmatic table manipulation
#'   \item Finds the matching row based on the shared ID
#'   \item Executes either custom click handler or default selection behavior
#' }
#'
#' @section Custom Click Handlers:
#' If a custom click handler is provided in the component configuration
#' (\code{component_info$config$click_handler}), it will be called with
#' the DT proxy, selected data (or NULL for deselection), and session.
#' Otherwise, default row selection/deselection is performed.
#'
#' @return NULL (invisible). Function is called for side effects only.
#'
#' @examples
#' \dontrun{
#' # Update DT selection when ID "123" is selected
#' update_dt_selection("my_table", "123", session, components)
#' 
#' # Deselect all rows
#' update_dt_selection("my_table", NULL, session, components)
#' }
update_dt_selection <- function(component_id, selected_id, session, components) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    return()
  }

  component_info <- components[[component_id]]
  current_data <- component_info$data_reactive()

  # Validate shared ID column exists
  if (!component_info$shared_id_column %in% names(current_data)) {
    warning("Shared ID column '", component_info$shared_id_column, "' not found in DT data for component: ", component_id)
    return()
  }

  # Get DT proxy
  dt_proxy <- DT::dataTableProxy(component_id, session = session)

  if (!is.null(selected_id)) {
    # Find matching row
    row_idx <- which(current_data[[component_info$shared_id_column]] == selected_id)

    if (length(row_idx) > 0) {
      selected_data <- current_data[row_idx[1], ]

      # Use user's custom click handler if provided
      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        component_info$config$click_handler(dt_proxy, selected_data, session)
      } else {
        # Default behavior: just select the row
        # The flag in the observer should prevent the event from being processed
        DT::selectRows(dt_proxy, selected = row_idx[1])
      }
    }
  } else {
    # Handle deselection
    if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
      component_info$config$click_handler(dt_proxy, NULL, session)
    } else {
      # Default deselection behavior
      DT::selectRows(dt_proxy, selected = integer(0))
    }
  }
}
