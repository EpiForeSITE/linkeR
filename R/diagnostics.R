#' Diagnostic function to debug linkeR observer issues
#'
#' Add this to your server function to diagnose why observers might not be firing
#'
#' @param registry The linkeR registry object
#' @param session The Shiny session object
#' @export
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   registry <- create_link_registry(session)
#'   
#'   # Register your components...
#'   
#'   # Add diagnostics
#'   diagnose_registry(registry, session)
#' }
#' }
diagnose_registry <- function(registry, session) {
  cat("\n========== linkeR Registry Diagnostics ==========\n")
  
  # Check registry structure
  if (is.null(registry)) {
    cat("ERROR: Registry is NULL!\n")
    return(invisible(NULL))
  }
  
  if (!is.list(registry)) {
    cat("ERROR: Registry is not a list!\n")
    return(invisible(NULL))
  }
  
  required_methods <- c("register_component", "set_selection", "get_selection", "get_components")
  missing_methods <- setdiff(required_methods, names(registry))
  if (length(missing_methods) > 0) {
    cat("ERROR: Registry missing methods:", paste(missing_methods, collapse = ", "), "\n")
    return(invisible(NULL))
  }
  
  cat("Registry structure is valid\n")
  
  # Get registered components
  components <- tryCatch(
    registry$get_components(),
    error = function(e) {
      cat("ERROR getting components:", e$message, "\n")
      return(list())
    }
  )
  
  cat("\n--- Registered Components ---\n")
  if (length(components) == 0) {
    cat("WARNING: No components registered yet!\n")
    cat("Make sure you call register_leaflet() or register_dt() BEFORE rendering the outputs.\n")
  } else {
    cat("Found", length(components), "registered component(s):\n")
    for (comp_id in names(components)) {
      comp <- components[[comp_id]]
      cat(sprintf("  - %s (type: %s, shared_id: %s)\n", 
                  comp_id, comp$type, comp$shared_id_column))
      
      # Check expected input names
      if (comp$type == "leaflet") {
        expected_input <- paste0(comp_id, "_marker_click")
        cat(sprintf("    Expected input: %s\n", expected_input))
        
        # Try to check if input exists (only works if output has been rendered)
        if (!is.null(session$input[[expected_input]])) {
          cat(sprintf("    Input exists: %s\n", expected_input))
        } else {
          cat(sprintf("    Input doesn't exist yet (or no selection made): %s\n", expected_input))
          cat("      This is normal if the map hasn't been clicked yet.\n")
        }
      } else if (comp$type == "datatable") {
        expected_input <- paste0(comp_id, "_rows_selected")
        cat(sprintf("    Expected input: %s\n", expected_input))
        
        if (!is.null(session$input[[expected_input]])) {
          cat(sprintf("    Input exists: %s\n", expected_input))
        } else {
          cat(sprintf("    Input doesn't exist yet (or no selection made): %s\n", expected_input))
          cat("      This is normal if no row has been selected yet.\n")
        }
      }
    }
  }
  
  # Check shared state
  cat("\n--- Shared State ---\n")
  state <- tryCatch(
    registry$get_shared_state(),
    error = function(e) {
      cat("ERROR getting shared state:", e$message, "\n")
      return(list(selected_id = NULL, selection_source = NULL))
    }
  )
  
  if (is.null(state$selected_id)) {
    cat("No selection currently active\n")
  } else {
    cat(sprintf("Selected ID: %s (source: %s)\n", state$selected_id, state$selection_source))
  }
  
  cat("==================================================\n\n")
  
  invisible(registry)
}
