#' Register a DT DataTable Component
#'
#' Register a DT datatable for linking with other components.
#'
#' @param registry A link registry created by \code{create_link_registry()}
#' @param dt_output_id Character string: the outputId of your DT::DTOutput
#' @param data_reactive Reactive expression returning the data frame for the table
#' @param shared_id_column Character string: name of the ID column
#' @export
register_dt <- function(registry, dt_output_id, data_reactive, shared_id_column) {
  # Check if DT is available
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("DT package is required for DT table linking. Please install it with: install.packages('DT')")
  }

  # Register with the registry
  registry$register_component(
    component_id = dt_output_id,
    type = "datatable",
    data_reactive = data_reactive,
    shared_id_column = shared_id_column,
    config = list()
  )
}

# Implementation of DT-specific observers (internal function)
setup_datatable_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  # Use session userData to store the flag - this persists across observer calls
  flag_name <- paste0(component_id, "_updating_selection")

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
