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
setup_datatable_observers <- function(component_id, session, components, shared_state) {
  # Access the parent environment to get the registry components and shared_state
  registry_env <- parent.frame()

  # Observer for table row selections
  observer1 <- shiny::observeEvent(session$input[[paste0(component_id, "_rows_selected")]],
    {
      selected_rows <- session$input[[paste0(component_id, "_rows_selected")]]

      if (length(selected_rows) > 0) {
        # Get the component info
        component_info <- components[[component_id]]
        current_data <- component_info$data_reactive()

        # Get the selected ID (use first selection if multiple)
        selected_id <- current_data[[component_info$shared_id_column]][selected_rows[1]]
        # Update shared state
        shared_state$selected_id <- selected_id
        shared_state$selection_source <- component_id
      } else {
        # Clear selection
        shared_state$selected_id <- NULL
        shared_state$selection_source <- component_id
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  # Observer for responding to selections from other components
  observer2 <- shiny::observeEvent(shared_state$selected_id,
    {
      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        selected_id <- shared_state$selected_id
        update_dt_selection(component_id, selected_id, session, components)
      }
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  return(list(observer1, observer2))
}

# Internal function to update DT selection
update_dt_selection <- function(component_id, selected_id, session, components) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    return()
  }

  component_info <- components[[component_id]]
  current_data <- component_info$data_reactive()

  # Validate shared ID column exists
  if (!component_info$shared_id_column %in% names(current_data)) {
    warning(
      "Shared ID column '", component_info$shared_id_column,
      "' not found in DT data for component: ", component_id
    )
    return()
  }

  # Get DT proxy
  dt_proxy <- DT::dataTableProxy(component_id, session = session)

  if (!is.null(selected_id)) {
    # Find matching row
    row_idx <- which(current_data[[component_info$shared_id_column]] == selected_id)

    if (length(row_idx) > 0) {
      # Select the matching row
      DT::selectRows(dt_proxy, selected = row_idx[1])
    }
  } else {
    # Clear selection
    DT::selectRows(dt_proxy, selected = integer(0))
  }
}
