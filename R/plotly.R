#' Extract ID from Plotly Event Data
#'
#' Robust extraction of shared ID from plotly click events, handling multiple traces
#' and different plotly configurations.
#'
#' @param event_data Plotly event data from event_data()
#' @param component_info Component information from registry
#' @return The extracted ID or NULL if extraction fails
#' @keywords internal
extract_plotly_id <- function(event_data, component_info) {
  if (is.null(event_data) || nrow(event_data) == 0) {
    return(NULL)
  }
  
  # Method 1: Try customdata (most reliable for multiple traces)
  if ("customdata" %in% names(event_data) && !is.null(event_data$customdata)) {
    id_val <- event_data$customdata[1]
    if (!is.null(id_val) && !is.na(id_val)) {
      cat("DEBUG: Got ID from customdata:", id_val, "\n")
      return(id_val)
    }
  }
  
  # Method 2: Try key (works for single trace)
  if ("key" %in% names(event_data) && !is.null(event_data$key)) {
    id_val <- event_data$key[1]
    if (!is.null(id_val) && !is.na(id_val)) {
      cat("DEBUG: Got ID from key:", id_val, "\n")
      return(id_val)
    }
  }
  
  # Method 3: Coordinate-based lookup (fallback)
  if ("x" %in% names(event_data) && "y" %in% names(event_data)) {
    current_data <- component_info$data_reactive()
    clicked_x <- event_data$x[1]
    clicked_y <- event_data$y[1]
    
    cat("DEBUG: Attempting coordinate-based lookup for x=", clicked_x, ", y=", clicked_y, "\n")
    
    # Find matching point by coordinates (with tolerance for floating point)
    for (i in seq_len(nrow(current_data))) {
      # We need to know which columns represent x and y
      # This is tricky since we don't know the plot structure
      # For now, try to find exact matches in numeric columns
      numeric_cols <- sapply(current_data, is.numeric)
      
      for (x_col in names(current_data)[numeric_cols]) {
        for (y_col in names(current_data)[numeric_cols]) {
          if (x_col != y_col && 
              abs(current_data[[x_col]][i] - clicked_x) < 1e-10 && 
              abs(current_data[[y_col]][i] - clicked_y) < 1e-10) {
            id_val <- current_data[[component_info$shared_id_column]][i]
            cat("DEBUG: Got ID from coordinates (", x_col, ",", y_col, "):", id_val, "\n")
            return(id_val)
          }
        }
      }
    }
  }
  
  # Method 4: Last resort - pointNumber mapping (unreliable with multiple traces)
  if ("pointNumber" %in% names(event_data)) {
    current_data <- component_info$data_reactive()
    point_number <- event_data$pointNumber[1] + 1  # plotly is 0-indexed, R is 1-indexed
    
    if (point_number > 0 && point_number <= nrow(current_data)) {
      id_val <- current_data[[component_info$shared_id_column]][point_number]
      cat("DEBUG: Got ID from pointNumber (UNRELIABLE):", id_val, "\n")
      warning("Using pointNumber for plotly ID extraction - this may be unreliable with multiple traces. Consider using customdata or key parameters.")
      return(id_val)
    }
  }
  
  cat("DEBUG: All ID extraction methods failed\n")
  return(NULL)
}

#' Prepare Plotly for Linking
#'
#' Utility function to automatically add required parameters to a plotly object
#' for reliable linking, regardless of plot structure (single/multiple traces).
#'
#' @param plotly_obj A plotly object created with plot_ly()
#' @param id_column Character string: name of the ID column in the data
#' @param source Character string: plotly source identifier
#' @returns Modified plotly object with linking parameters added
#' @export
#' @examples
#' \dontrun{
#' # Instead of manually adding customdata/key:
#' p <- plot_ly(data = my_data, x = ~x_col, y = ~y_col, color = ~category)
#' p <- prepare_plotly_linking(p, "my_id_column", "my_source")
#' }
prepare_plotly_linking <- function(plotly_obj, id_column, source) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required")
  }
  
  # Get the original data from the plotly object
  if (is.null(plotly_obj$x$attrs) || length(plotly_obj$x$attrs) == 0) {
    warning("Cannot extract data from plotly object. Please add customdata = ~", id_column, " manually.")
    return(plotly_obj)
  }
  
  # Add customdata to the first (main) attribute set
  # This should work even with multiple traces
  if (is.null(plotly_obj$x$attrs[[1]]$customdata)) {
    # Create the customdata reference
    id_formula <- as.formula(paste("~", id_column))
    plotly_obj$x$attrs[[1]]$customdata <- id_formula
    
    message("Added customdata = ~", id_column, " to plotly object")
  }
  
  # Ensure source is set
  if (is.null(plotly_obj$x$source)) {
    plotly_obj$x$source <- source
    message("Added source = '", source, "' to plotly object")
  }
  
  return(plotly_obj)
}

#' Register a Plotly Component
#'
#' `register_plotly` registers a Plotly component for linking with other components.
#'
#' @param session Shiny session object
#' @param registry A link registry created by [create_link_registry()]
#' @param plotly_output_id Character string: the outputId of your plotlyOutput
#' @param data_reactive Reactive expression returning the data frame for the plot
#' @param shared_id_column Character string: name of the ID column
#' @param event_types Character vector: plotly event types to listen for
#' @param source Character string: plotly source identifier for event tracking
#' @param click_handler Optional function: custom click handler for selection
#' @returns NULL (invisible). This function is called for its side effects.
#' @export
register_plotly <- function(session, registry, plotly_output_id, data_reactive, shared_id_column,
                          event_types = c("plotly_click"), source = NULL,
                          click_handler = NULL) {
  # Check if plotly is available
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for Plotly component linking. Please install it with: install.packages('plotly')")
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

  # Default source to plotly_output_id if not provided
  if (is.null(source)) {
    source <- plotly_output_id
  }
  
  # Provide helpful guidance for plotly linking
  message("plotly component '", plotly_output_id, "' registered for linking.")
  message("For reliable linking, ensure your plot_ly() call includes:")
  message("  - customdata = ~", shared_id_column, "  (preferred for all plot types)")
  message("  - OR key = ~", shared_id_column, "       (works for single-trace plots)")
  message("  - source = '", source, "'")
  message("Example: plot_ly(data = your_data, ..., customdata = ~", shared_id_column, ", source = '", source, "')")

  # Register with the registry
  registry$register_component(
    session = session,
    component_id = plotly_output_id,
    type = "plotly",
    data_reactive = data_reactive,
    shared_id_column = shared_id_column,
    config = list(
      event_types = event_types,
      source = source,
      click_handler = click_handler
    )
  )
}

#' Setup Plotly Observers
#'
#' Sets up reactive observers for a Plotly component to handle user interactions.
#'
#' @param component_id Character string. Unique identifier for the Plotly component.
#' @param session Shiny session object. The current Shiny session for reactive context.
#' @param components List. Collection of UI components in the application.
#' @param shared_state Reactive values object. Shared state container for cross-component communication.
#' @param on_selection_change Function. Callback function to execute when plot selection changes.
#' @param registry List or NULL. Optional registry for component management. Defaults to NULL.
#' @return NULL. This function is called for its side effects of setting up observers.
setup_plotly_observers <- function(component_id, session, components, shared_state, on_selection_change, registry = NULL) {
  # Use session userData to store the flag - this persists across observer calls
  flag_name <- paste0(component_id, "_updating_selection")

  # Get component configuration
  component_info <- components[[component_id]]
  if (is.null(component_info)) {
    warning("Component info not found for plotly component: ", component_id)
    return()
  }

  event_types <- component_info$config$event_types
  if (is.null(event_types)) {
    event_types <- c("plotly_click")
  }
  source <- component_info$config$source
  if (is.null(source)) {
    source <- component_id
  }

  # Observer for plotly events (USER CLICKS ONLY)
  observer1 <- shiny::observeEvent({
    # Listen to the plotly event data based on the first event type
    plotly::event_data(event_types[1], source = source)
  }, {
    # Check session-level flag
    if (isTRUE(session$userData[[flag_name]])) {
      return()
    }

    event_data <- plotly::event_data(event_types[1], source = source)

    if (!is.null(event_data) && nrow(event_data) > 0) {
      selected_id <- NULL
      
      # Enhanced ID extraction with better error handling
      selected_id <- extract_plotly_id(event_data, component_info)
      
      if (!is.null(selected_id)) {
        cat("DEBUG: Successfully extracted ID:", selected_id, "\n")
      } else {
        cat("DEBUG: Failed to extract ID from plotly event\n")
        cat("DEBUG: Event data structure:", paste(capture.output(str(event_data)), collapse = "\n"), "\n")
      }

      if (!is.null(selected_id)) {
        # THIS IS CORRECT - USER CLICK SHOULD CALL set_selection
        if (!is.null(registry) && !is.null(registry$set_selection)) {
          registry$set_selection(selected_id, component_id)
        } else {
          shared_state$selected_id <- selected_id
          shared_state$selection_source <- component_id
        }
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
      cat("DEBUG: Plotly external selection change detected. Selected ID:", shared_state$selected_id, "\n")
      cat("DEBUG: Plotly Source component:", shared_state$selection_source, "Current component:", component_id, "\n")

      # Only respond if selection came from a different component
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        selected_id <- shared_state$selected_id
        cat("DEBUG: Plotly updating visual selection to ID:", selected_id, "\n")

        # Set session-level flag to prevent recursive calls
        session$userData[[flag_name]] <- TRUE

        # THIS SHOULD ONLY UPDATE VISUAL STATE - NO set_selection CALLS!
        update_plotly_selection(component_id, selected_id, session, components)

        # Reset flag after a short delay to allow plotly event to be processed and ignored
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

#' Update Plotly Selection Based on Shared ID
#'
#' Updates the selection state of a Plotly component when a shared ID
#' is selected or deselected from another linked component.
#'
#' @param component_id Character string. The ID of the Plotly component to update.
#' @param selected_id The shared ID value to select. If NULL, deselects all points.
#' @param session Shiny session object for the current user session.
#' @param components List containing component configuration information.
#' @return NULL (invisible). Function is called for side effects only.
update_plotly_selection <- function(component_id, selected_id, session, components) {
  cat("DEBUG: update_plotly_selection called for component:", component_id, "with ID:", selected_id, "\n")

  if (!requireNamespace("plotly", quietly = TRUE)) {
    cat("DEBUG: plotly package not available\n")
    return()
  }

  component_info <- components[[component_id]]
  if (is.null(component_info)) {
    cat("DEBUG: Component info not found for:", component_id, "\n")
    return()
  }

  current_data <- component_info$data_reactive()
  cat("DEBUG: Plotly current data has", nrow(current_data), "rows\n")

  # Validate shared ID column exists
  if (!component_info$shared_id_column %in% names(current_data)) {
    warning("Shared ID column '", component_info$shared_id_column, "' not found in Plotly data for component: ", component_id)
    return()
  }

  # Get plotly proxy
  plot_proxy <- plotly::plotlyProxy(component_id, session = session)
  cat("DEBUG: Plotly proxy created\n")

  if (!is.null(selected_id)) {
    # Find matching row
    row_idx <- which(current_data[[component_info$shared_id_column]] == selected_id)
    cat("DEBUG: Plotly found", length(row_idx), "matching rows for ID:", selected_id, "\n")

    if (length(row_idx) > 0) {
      selected_data <- current_data[row_idx[1], ]
      cat("DEBUG: Plotly selecting row index:", row_idx[1], "\n")

      # Use user's custom click handler if provided
      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        cat("DEBUG: Using custom Plotly click handler\n")
        component_info$config$click_handler(plot_proxy, selected_data, session)
      } else {
        # Default behavior: highlight the selected point
        cat("DEBUG: Using default Plotly selection, highlighting point:", row_idx[1] - 1, "\n") # plotly is 0-indexed
        apply_default_plotly_behavior(plot_proxy, selected_data, session, component_id)
      }
    } else {
      cat("DEBUG: Plotly no matching rows found for ID:", selected_id, "\n")
    }
  } else {
    cat("DEBUG: Plotly clearing selection\n")
    # Handle deselection
    if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
      component_info$config$click_handler(plot_proxy, NULL, session)
    } else {
      # Default deselection behavior
      apply_default_plotly_behavior(plot_proxy, NULL, session, component_id)
    }
  }
}

#' Apply Default Plotly Behavior for Selection Highlighting
#'
#' Applies the default visual highlighting behavior for a Plotly plot
#' when a data point is selected or deselected.
#'
#' @param plot_proxy plotlyProxy object for the target plot.
#' @param selected_data Data frame row containing the selected data point, or NULL for deselection.
#' @param session Shiny session object for the current user session.
#' @param component_id Character string. ID of the plotly component for reference.
#' @return NULL (invisible). Function is called for side effects only.
apply_default_plotly_behavior <- function(plot_proxy, selected_data, session, component_id) {
  cat("DEBUG: apply_default_plotly_behavior called\n")

  # Get component info to determine current data and find the point index
  component_info <- session$userData[["linkeR_components"]][[component_id]]
  if (is.null(component_info)) {
    cat("DEBUG: Could not find component info for visual updates\n")
    return()
  }

  current_data <- component_info$data_reactive()
  
  if (!is.null(selected_data)) {
    # Find the index of the selected point
    selected_id <- selected_data[[component_info$shared_id_column]]
    point_index <- which(current_data[[component_info$shared_id_column]] == selected_id)[1] - 1  # plotly is 0-indexed
    
    cat("DEBUG: Highlighting point at index:", point_index, "for ID:", selected_id, "\n")
    
    # Create arrays for all points
    n_points <- nrow(current_data)
    sizes <- rep(8, n_points)  # default size
    colors <- rep("steelblue", n_points)  # default color
    
    # Highlight the selected point
    if (!is.na(point_index) && point_index >= 0 && point_index < n_points) {
      sizes[point_index + 1] <- 15  # make selected point larger (R is 1-indexed)
      colors[point_index + 1] <- "red"  # make selected point red
    }
    
    tryCatch({
      plotly::plotlyProxyInvoke(
        plot_proxy,
        "restyle",
        list(
          "marker.size" = list(sizes),
          "marker.color" = list(colors),
          "marker.line.width" = list(ifelse(seq_len(n_points) == (point_index + 1), 2, 0)),
          "marker.line.color" = list(ifelse(seq_len(n_points) == (point_index + 1), "darkred", "transparent"))
        ),
        list(0)  # trace index
      )
    }, error = function(e) {
      cat("DEBUG: Error applying plotly highlight:", e$message, "\n")
    })
  } else {
    # Reset all points to default appearance
    cat("DEBUG: Clearing plotly highlights\n")
    n_points <- nrow(current_data)
    
    tryCatch({
      plotly::plotlyProxyInvoke(
        plot_proxy,
        "restyle",
        list(
          "marker.size" = list(rep(8, n_points)),
          "marker.color" = list(rep("steelblue", n_points)),
          "marker.line.width" = list(rep(0, n_points)),
          "marker.line.color" = list(rep("transparent", n_points))
        ),
        list(0)  # trace index
      )
    }, error = function(e) {
      cat("DEBUG: Error clearing plotly highlight:", e$message, "\n")
    })
  }
}