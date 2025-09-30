#' Extract ID from Plotly Event Data
#'
#' Extracts shared ID from plotly click events using multiple strategies.
#'
#' @param event_data Plotly event data from event_data()
#' @param component_info Component information from registry
#' @return The extracted ID or NULL if extraction fails
#' @keywords internal
extract_plotly_id <- function(event_data, component_info) {
  if (is.null(event_data) || nrow(event_data) == 0) {
    return(NULL)
  }
  
  # Try customdata (most reliable for multiple traces)
  if ("customdata" %in% names(event_data) && !is.null(event_data$customdata)) {
    id_val <- event_data$customdata[1]
    if (!is.null(id_val) && !is.na(id_val)) {
      return(id_val)
    }
  }
  
  # Try key (works for single trace)
  if ("key" %in% names(event_data) && !is.null(event_data$key)) {
    id_val <- event_data$key[1]
    if (!is.null(id_val) && !is.na(id_val)) {
      return(id_val)
    }
  }
  
  # Smart coordinate-based lookup (fallback)
  if ("x" %in% names(event_data) && "y" %in% names(event_data)) {
    current_data <- component_info$data_reactive()
    clicked_x <- event_data$x[1]
    clicked_y <- event_data$y[1]
    
    id_val <- smart_coordinate_lookup(current_data, clicked_x, clicked_y, component_info$shared_id_column)
    if (!is.null(id_val)) {
      return(id_val)
    }
  }
  
  # Last resort - pointNumber mapping (unreliable with multiple traces)
  if ("pointNumber" %in% names(event_data)) {
    current_data <- component_info$data_reactive()
    point_number <- event_data$pointNumber[1] + 1
    
    if (point_number > 0 && point_number <= nrow(current_data)) {
      id_val <- current_data[[component_info$shared_id_column]][point_number]
      warning("Using pointNumber for plotly ID extraction - this may be unreliable with multiple traces. Consider using customdata or key parameters.")
      return(id_val)
    }
  }
  
  return(NULL)
}

#' Smart Coordinate-Based ID Lookup
#'
#' Enables automatic linking by matching clicked coordinates to data rows.
#' This function is used during ID extraction when customdata/key parameters
#' are not available, providing zero-configuration automatic linking.
#'
#' @param data Data frame with the original data
#' @param clicked_x X coordinate from plotly event
#' @param clicked_y Y coordinate from plotly event  
#' @param id_column Name of the ID column
#' @return The matched ID or NULL
#' @keywords internal
smart_coordinate_lookup <- function(data, clicked_x, clicked_y, id_column) {
  x_is_numeric <- is.numeric(clicked_x)
  y_is_numeric <- is.numeric(clicked_y)
  
  # Handle mixed categorical/numeric coordinates
  if (!x_is_numeric && y_is_numeric) {
    factor_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    for (x_col in factor_cols) {
      for (y_col in numeric_cols) {
        if (x_col != id_column && y_col != id_column) {
          matches <- which(data[[x_col]] == clicked_x & 
                          abs(data[[y_col]] - clicked_y) < 1e-6)
          
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
        }
      }
    }
  }
  
  # Both coordinates are numeric
  if (x_is_numeric && y_is_numeric) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    for (x_col in numeric_cols) {
      for (y_col in numeric_cols) {
        if (x_col != y_col && x_col != id_column && y_col != id_column) {
          # Try exact match first
          matches <- which(abs(data[[x_col]] - clicked_x) < 1e-10 & 
                          abs(data[[y_col]] - clicked_y) < 1e-10)
          
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
          
          # Try with broader tolerance for floating point issues
          matches <- which(abs(data[[x_col]] - clicked_x) < 1e-6 & 
                          abs(data[[y_col]] - clicked_y) < 1e-6)
          
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
        }
      }
    }
  }
  
  # Both coordinates are categorical
  if (!x_is_numeric && !y_is_numeric) {
    factor_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
    
    for (x_col in factor_cols) {
      for (y_col in factor_cols) {
        if (x_col != y_col && x_col != id_column && y_col != id_column) {
          matches <- which(data[[x_col]] == clicked_x & data[[y_col]] == clicked_y)
          
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
        }
      }
    }
  }
  
  # Fallback - try to match any single unique coordinate
  if (x_is_numeric || y_is_numeric) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    
    for (col in numeric_cols) {
      if (col != id_column) {
        # Try matching x coordinate
        if (x_is_numeric) {
          matches <- which(abs(data[[col]] - clicked_x) < 1e-6)
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
        }
        
        # Try matching y coordinate  
        if (y_is_numeric) {
          matches <- which(abs(data[[col]] - clicked_y) < 1e-6)
          if (length(matches) == 1) {
            return(data[[id_column]][matches[1]])
          }
        }
      }
    }
  }
  
  # Last resort - combination matching with multiple tolerance levels
  if (x_is_numeric && y_is_numeric) {
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    tolerances <- c(1e-8, 1e-4, 1e-2, 0.1)
    
    for (tol in tolerances) {
      for (x_col in numeric_cols) {
        for (y_col in numeric_cols) {
          if (x_col != y_col && x_col != id_column && y_col != id_column) {
            matches <- which(abs(data[[x_col]] - clicked_x) < tol & 
                            abs(data[[y_col]] - clicked_y) < tol)
            
            if (length(matches) == 1) {
              warning("Using loose coordinate tolerance (", tol, ") for plotly linking. Consider adding customdata parameter for more reliable linking.")
              return(data[[id_column]][matches[1]])
            }
          }
        }
      }
    }
  }
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
#' \dontrun{ TODO (update example to actually work)
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
#' The default behavior uses plotly's built-in point selection highlighting, which
#' is simple and works reliably across all plot types.
#'
#' @param session Shiny session object
#' @param registry A link registry created by [create_link_registry()]
#' @param plotly_output_id Character string: the outputId of your plotlyOutput
#' @param data_reactive Reactive expression returning the data frame for the plot
#' @param shared_id_column Character string: name of the ID column
#' @param event_types Character vector: plotly event types to listen for
#' @param source Character string: plotly source identifier for event tracking
#' @param click_handler Optional function: custom selection update handler.
#'   Function signature: function(plot_proxy, selected_data, session)
#'   where selected_data is the row from data_reactive() or NULL to clear selection.
#' @returns NULL (invisible). This function is called for its side effects.
#' @examples TODO
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
  
  # More user-friendly guidance
  message("✓ plotly component '", plotly_output_id, "' registered for linking")
  message("ℹ linkeR will automatically handle linking for most plot configurations")
  message("ℹ If clicking doesn't work, add: customdata = ~", shared_id_column, " to your plot_ly() call")

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
      selected_id <- extract_plotly_id(event_data, component_info)

      if (!is.null(selected_id)) {
        # Update the shared state first
        if (!is.null(registry) && !is.null(registry$set_selection)) {
          registry$set_selection(selected_id, component_id)
        } else {
          shared_state$selected_id <- selected_id
          shared_state$selection_source <- component_id
        }
        
        # Update this plot's own visual state immediately
        current_data <- component_info$data_reactive()
        if (component_info$shared_id_column %in% names(current_data)) {
          selected_row <- current_data[current_data[[component_info$shared_id_column]] == selected_id, ][1, ]
          if (nrow(selected_row) > 0) {
            # Use custom handler if available, otherwise default
            if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
              plot_proxy <- plotly::plotlyProxy(component_id, session = session)
              component_info$config$click_handler(plot_proxy, selected_row, session)
            } else {
              apply_default_plotly_behavior(plotly::plotlyProxy(component_id, session = session), selected_row, session, component_id)
            }
          }
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
      
      # Clear this plot's own visual state immediately
      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        plot_proxy <- plotly::plotlyProxy(component_id, session = session)
        component_info$config$click_handler(plot_proxy, NULL, session)
      } else {
        apply_default_plotly_behavior(plotly::plotlyProxy(component_id, session = session), NULL, session, component_id)
      }
    }
  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE
  )

  # Observer for responding to selections from other components (VISUAL UPDATES ONLY)
  observer2 <- shiny::observeEvent(shared_state$selected_id,
    {
      if (!is.null(shared_state$selection_source) &&
        shared_state$selection_source != component_id) {
        selected_id <- shared_state$selected_id

        # Set session-level flag to prevent recursive calls during visual update
        session$userData[[flag_name]] <- TRUE

        update_plotly_selection(component_id, selected_id, session, NULL)

        later::later(function() {
          session$userData[[flag_name]] <- FALSE
        }, delay = 0.05)
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
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return()
  }

  if (is.null(components)) {
    components <- session$userData[["linkeR_components"]]
  }
  
  if (is.null(components)) {
    return()
  }
  
  component_info <- components[[component_id]]
  if (is.null(component_info)) {
    return()
  }

  current_data <- component_info$data_reactive()

  if (!component_info$shared_id_column %in% names(current_data)) {
    warning("Shared ID column '", component_info$shared_id_column, "' not found in Plotly data for component: ", component_id)
    return()
  }

  plot_proxy <- plotly::plotlyProxy(component_id, session = session)

  if (!is.null(selected_id)) {
    row_idx <- which(current_data[[component_info$shared_id_column]] == selected_id)

    if (length(row_idx) > 0) {
      selected_data <- current_data[row_idx[1], ]

      if (!is.null(component_info$config$click_handler) && is.function(component_info$config$click_handler)) {
        component_info$config$click_handler(plot_proxy, selected_data, session)
      } else {
        apply_default_plotly_behavior(plot_proxy, selected_data, session, component_id)
      }
    }
  } else {
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
#' @description
#' Implements selection highlighting using plotly's native selectedpoints mechanism.
#' This leverages plotly's built-in selection system which works consistently across
#' all chart types without hardcoded styling values.
#' 
#' @details
#' This function uses plotly's native selection capabilities by:
#' \itemize{
#'   \item Finding points using key or customdata across all traces
#'   \item Applying plotly's built-in selectedpoints styling
#'   \item Using plotly's default selected/unselected appearance
#'   \item Working generically across all plot types and trace structures
#' }
#' 
#' The selection highlighting follows plotly's native behavior and appearance,
#' ensuring consistency with user expectations and plotly's design system.
#'
#' @param plot_proxy plotlyProxy object for the target plot.
#' @param selected_row Data frame row containing the selected data point, or NULL for deselection.  
#' @param session Shiny session object for the current user session.
#' @param component_id Character string. ID of the plotly component for reference.
#' @return NULL (invisible). Function is called for side effects only.
apply_default_plotly_behavior <- function(plot_proxy, selected_row, session, component_id) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    return()
  }
  
  tryCatch({
    if (!is.null(selected_row) && nrow(selected_row) > 0) {
      component_info <- session$userData[["linkeR_components"]][[component_id]]
      if (is.null(component_info)) return()
      selected_id <- selected_row[[component_info$shared_id_column]][1]
      js_code <- sprintf('
        (function() {
          var plotDiv = document.getElementById("%s");
          if (!plotDiv || !plotDiv.data) return;
          var targetId = "%s";
          var selectedpoints = [];
          var found = false;
          for (var i = 0; i < plotDiv.data.length; i++) {
            var trace = plotDiv.data[i];
            var indices = [];
            if (trace.key) {
              for (var j = 0; j < trace.key.length; j++) {
                if (String(trace.key[j]) === String(targetId)) {
                  indices.push(j);
                  found = true;
                }
              }
            } else if (trace.customdata) {
              for (var j = 0; j < trace.customdata.length; j++) {
                if (String(trace.customdata[j]) === String(targetId)) {
                  indices.push(j);
                  found = true;
                }
              }
            }
            selectedpoints.push(indices.length > 0 ? indices : null);
          }
          if (found) {
            Plotly.restyle(plotDiv, {
              selectedpoints: selectedpoints,
              selected: {
                marker: {
                  opacity: 1.0,
                  size: 15,
                  line: { width: 2, color: "darkblue" }
                }
              },
              unselected: {
                marker: { opacity: 0.4 }
              }
            });
          }
        })();
      ', component_id, selected_id)
      session$sendCustomMessage("eval", js_code)
    } else {
      js_code <- sprintf('
        (function() {
          var plotDiv = document.getElementById("%s");
          if (!plotDiv || !plotDiv.data) return;
          Plotly.restyle(plotDiv, {
            selectedpoints: null,
            selected: null,
            unselected: null
          });
        })();
      ', component_id)
      session$sendCustomMessage("eval", js_code)
    }
  }, error = function(e) {
    # Silent error handling for visual updates
  })
}