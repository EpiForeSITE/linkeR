# Test cases for Plotly component registration and behavior

test_that("register_plotly validates input", {
  # Mock session and registry
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  # Test with valid inputs
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), x = c(1, 2, 3), y = c(10, 20, 30))
  })

  expect_no_error({
    register_plotly(session, registry, "test_plot", test_data, "id", "test_source")
  })
  
  # Test missing registry
  expect_error(
    register_plotly(session, NULL, "test_plot", test_data, "id", "test_source"),
    "registry"
  )

  # Test missing plotly_output_id
  expect_error(
    register_plotly(session, registry, NULL, test_data, "id", "test_source"),
    "plotly_output_id must be a string"
  )
  
  # Test non-reactive data
  expect_error(
    register_plotly(session, registry, "test_plot", data.frame(id = 1:3), "id", "test_source"),
    "data_reactive must be a reactive expression"
  )
  
  # Test missing source_id
  expect_error(
    register_plotly(session, registry, "test_plot", test_data, "id", NULL),
    "source_id must be a string"
  )
})

test_that("register_plotly creates proper component registration", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      x_val = c(1, 2, 3),
      y_val = c(100, 200, 300)
    )
  })

  # Register the plotly component
  register_plotly(session, registry, "business_plot", test_data, "business_id", "business_plot_source")
  
  # Check component was registered correctly
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-business_plot" %in% names(components))
  
  # Check component details
  plotly_component <- components[["mock-session-business_plot"]]
  expect_equal(plotly_component$type, "plotly")
  expect_equal(plotly_component$shared_id_column, "business_id")
  expect_true(is.list(plotly_component$config))
  expect_equal(plotly_component$config$source_id, "business_plot_source")
})

test_that("register_plotly requires plotly package", {
  # This test would need more sophisticated mocking to actually test the missing package scenario
  # For now, we'll skip if plotly is available
  if (requireNamespace("plotly", quietly = TRUE)) {
    skip("plotly package is available, cannot test missing package scenario")
  } else {
    session <- shiny::MockShinySession$new()
    registry <- create_link_registry(session)
    
    test_data <- reactive({
      data.frame(id = 1:3, name = c("A", "B", "C"))
    })
    
    expect_error(
      register_plotly(session, registry, "test_plot", test_data, "id", "test_source"),
      "plotly package is required"
    )
  }
})

test_that("setup_plotly_observers creates proper observers", {
  skip_if_not_installed("plotly")
  
  session <- shiny::MockShinySession$new()
  
  # Mock reactive values for shared state
  shared_state <- shiny::reactiveValues(
    selected_id = NULL,
    selected_ids = character(0),
    selection_source = NULL
  )
  
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      x = c(1, 2, 3),
      y = c(10, 20, 30)
    )
  })
  
  components <- list(
    test_plot = list(
      data_reactive = test_data,
      shared_id_column = "id",
      config = list(
        source_id = "test_source",
        click_handler = NULL,
        brush_handler = NULL
      )
    )
  )
  
  # Mock registry
  mock_registry <- list(
    set_selection = function(id, source) {
      shared_state$selected_id <- id
      shared_state$selection_source <- source
    },
    set_multiple_selection = function(ids, source) {
      shared_state$selected_ids <- ids
      shared_state$selection_source <- source
    }
  )
  
  # Create observers (this mainly tests that the function doesn't error)
  expect_no_error({
    observers <- setup_plotly_observers(
      "test_plot", 
      session, 
      components, 
      shared_state, 
      NULL, # on_selection_change
      mock_registry
    )
  })
  
  # Should return a list of observers
  expect_true(is.list(observers))
  expect_length(observers, 4) # click_observer, brush_observer, response_observer, multiple_response_observer
})

test_that("plotly integration with registry selection works", {
  skip_if_not_installed("plotly")
  
  session <- shiny::MockShinySession$new()
  
  test_data <- reactive({
    data.frame(
      id = c("A", "B", "C"),
      name = c("Alice", "Bob", "Charlie"),
      x = c(1, 2, 3),
      y = c(25, 30, 35)
    )
  })
  
  # Track selection change calls
  selection_calls <- list()
  
  test_callback <- function(selected_id, selected_data, source_id, session) {
    selection_calls <<- append(selection_calls, list(list(
      selected_id = selected_id,
      selected_data = selected_data,
      source_id = source_id
    )))
  }
  
  registry <- create_link_registry(session, on_selection_change = test_callback)
  register_plotly(session, registry, "test_plot", test_data, "id", "test_source")
  
  # Test programmatic selection
  isolate(registry$set_selection("B", "test_source"))
  
  # Check callback was called
  expect_length(selection_calls, 1)
  expect_equal(selection_calls[[1]]$selected_id, "B")
  expect_equal(selection_calls[[1]]$source_id, "test_source")
  expect_equal(selection_calls[[1]]$selected_data$name, "Bob")
  
  # Test multiple selection
  isolate(registry$set_multiple_selection(c("A", "C"), "test_source"))
  expect_length(selection_calls, 2)
  # Multiple selection callback gets the first item for backward compatibility
  expect_equal(selection_calls[[2]]$selected_id, "A")
  expect_equal(selection_calls[[2]]$source_id, "test_source")
  expect_equal(selection_calls[[2]]$selected_data$name, "Alice")
  
  # Test clearing selection
  isolate(registry$set_multiple_selection(character(0), "clear_source"))
  expect_length(selection_calls, 3)
  expect_null(selection_calls[[3]]$selected_id)
  expect_equal(selection_calls[[3]]$source_id, "clear_source")
})

test_that("plotly handles different data types in shared column", {
  skip_if_not_installed("plotly")
  
  session <- shiny::MockShinySession$new()
  
  registry <- create_link_registry(session)
  
  # Test with character IDs
  char_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      x = c(1, 2, 3),
      y = c(100, 200, 300),
      stringsAsFactors = FALSE
    )
  })
  
  expect_no_error({
    register_plotly(session, registry, "char_plot", char_data, "business_id", "char_source")
  })
  
  # Test with numeric IDs
  numeric_data <- reactive({
    data.frame(
      item_id = c(1, 2, 3),
      name = c("Item A", "Item B", "Item C"),
      x = c(1, 2, 3),
      y = c(10, 20, 30)
    )
  })
  
  expect_no_error({
    register_plotly(session, registry, "numeric_plot", numeric_data, "item_id", "numeric_source")
  })
  
  # Check all were registered
  components <- registry$get_components()
  expect_length(components, 2)
  expect_true("mock-session-char_plot" %in% names(components))
  expect_true("mock-session-numeric_plot" %in% names(components))
})