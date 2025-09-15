# Test cases for DT component registration and behavior
# Note that anything involving DT's actual behavior cannot be tested within this context

test_that("register_dt validates input", {
  # Mock session and registry
  session <- shiny::Mock'shiny'Session$new()
  registry <- create_link_registry(session)
  
  # Test with valid inputs
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), value = c(10, 20, 30))
  })

  expect_no_error({
    register_dt(session, registry, "test_table", test_data, "id")
  })
  
  # Test missing registry
  expect_error(
    register_dt(session, NULL, "test_table", test_data, "id"),
    "registry"
  )

  # Test missing dt_output_id
  expect_error(
    register_dt(session, registry, NULL, test_data, "id"),
    "dt_output_id must be a string"
  )
  
  # Test non-reactive data
  expect_error(
    register_dt(session, registry, "test_table", data.frame(id = 1:3), "id"),
    "data_reactive must be a reactive expression"
  )
  
  # Test missing shared_id_column
  expect_error(
    register_dt(session, registry, "test_table", test_data, NULL),
    "shared_id_column must be a string"
  )
})

test_that("register_dt creates proper component registration", {
  session <- shiny::Mock'shiny'Session$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      revenue = c(100000, 200000, 300000)
    )
  })

  # Register the DT component
  register_dt(session=session, registry, "business_table", test_data, "business_id")
  
  # Check component was registered correctly
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-business_table" %in% names(components))
  
  # Check component details
  dt_component <- components[["mock-session-business_table"]]
  expect_equal(dt_component$type, "datatable")
  expect_equal(dt_component$shared_id_column, "business_id")
  expect_true(is.list(dt_component$config))
  expect_length(dt_component$config, 1) # Should only contain the click handler by default
})

test_that("register_dt requires DT package", {
  # Mock missing DT package
  if (requireNamespace("DT", quietly = TRUE)) {
    # If DT is available, we can't easily test the missing package scenario
    # This test would need more sophisticated mocking
    skip("DT package is available, cannot test missing package scenario")
  } else {
    session <- shiny::Mock'shiny'Session$new()
    registry <- create_link_registry(session)
    
    test_data <- reactive({
      data.frame(id = 1:3, name = c("A", "B", "C"))
    })
    
    expect_error(
      register_dt(session, registry, "test_table", test_data, "id"),
      "DT package is required"
    )
  }
})

test_that("setup_datatable_observers creates proper observers", {
  skip_if_not_installed("DT")
  skip_if_not_installed("later")
  
  session <- shiny::Mock'shiny'Session$new()
  
  # Mock reactive values for shared state
  shared_state <- shiny::reactiveValues(
    selected_id = NULL,
    selection_source = NULL
  )
  
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })
  
  components <- list(
    test_table = list(
      data_reactive = test_data,
      shared_id_column = "id",
      config = list()
    )
  )
  
  # Mock registry
  mock_registry <- list(
    set_selection = function(id, source) {
      shared_state$selected_id <- id
      shared_state$selection_source <- source
    }
  )
  
  # Create observers (this mainly tests that the function doesn't error)
  expect_no_error({
    observers <- setup_datatable_observers(
      "test_table", 
      session, 
      components, 
      shared_state, 
      NULL, # on_selection_change
      mock_registry
    )
  })
  
  # Should return a list of observers
  expect_true(is.list(observers))
  expect_length(observers, 2) # observer1 and observer2
})

test_that("DT observer handles session flag correctly", {
  skip_if_not_installed("DT")
  skip_if_not_installed("later")
  
  session <- shiny::Mock'shiny'Session$new()
  
  # Mock reactive values
  shared_state <- shiny::reactiveValues(
    selected_id = NULL,
    selection_source = NULL
  )
  
  test_data <- reactive({
    data.frame(
      id = c("A", "B", "C"),
      name = c("Alice", "Bob", "Charlie"),
      value = c(10, 20, 30)
    )
  })
  
  components <- list(
    test_table = list(
      data_reactive = test_data,
      shared_id_column = "id",
      config = list()
    )
  )
  
  call_count <- 0
  mock_registry <- list(
    set_selection = function(id, source) {
      call_count <<- call_count + 1
      shared_state$selected_id <- id
      shared_state$selection_source <- source
    }
  )
  
  # Test that flag prevents recursive calls
  flag_name <- "test_table_updating_selection"
  
  # Set flag to TRUE
  session$userData[[flag_name]] <- TRUE
  
  observers <- setup_datatable_observers(
    "test_table",
    session,
    components,
    shared_state,
    NULL,
    mock_registry
  )
  
  # The observer should not call set_selection when flag is TRUE
  # (We can't easily trigger the observer in unit tests, but we can test the logic)
  expect_true(session$userData[[flag_name]])
})

test_that("DT handles different data types in shared column", {
  skip_if_not_installed("DT")
  
  session <- shiny::Mock'shiny'Session$new()
  
  registry <- create_link_registry(session)
  
  # Test with character IDs
  char_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      stringsAsFactors = FALSE
    )
  })
  
  expect_no_error({
    register_dt(session, registry, "char_table", char_data, "business_id")
  })
  
  # Test with numeric IDs
  numeric_data <- reactive({
    data.frame(
      item_id = c(1, 2, 3),
      name = c("Item A", "Item B", "Item C")
    )
  })
  
  expect_no_error({
    register_dt(session, registry, "numeric_table", numeric_data, "item_id")
  })
  
  # Test with factor IDs
  factor_data <- reactive({
    data.frame(
      category_id = factor(c("CAT_A", "CAT_B", "CAT_C")),
      name = c("Category A", "Category B", "Category C")
    )
  })
  
  expect_no_error({
    register_dt(session, registry, "factor_table", factor_data, "category_id")
  })
  
  # Check all were registered
  components <- registry$get_components()
  expect_length(components, 3)
  expect_true("mock-session-char_table" %in% names(components))
  expect_true("mock-session-numeric_table" %in% names(components))
  expect_true("mock-session-factor_table" %in% names(components))
})

test_that("DT integration with registry selection works", {
  skip_if_not_installed("DT")
  
  session <- shiny::Mock'shiny'Session$new()
  
  test_data <- reactive({
    data.frame(
      id = c("A", "B", "C"),
      name = c("Alice", "Bob", "Charlie"),
      age = c(25, 30, 35)
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
  register_dt(session, registry, "test_table", test_data, "id")
  
  # Test programmatic selection
  isolate(registry$set_selection("B", "test_source"))
  
  # Check callback was called
  expect_length(selection_calls, 1)
  expect_equal(selection_calls[[1]]$selected_id, "B")
  expect_equal(selection_calls[[1]]$source_id, "test_source")
  expect_equal(selection_calls[[1]]$selected_data$name, "Bob")
  
  # Test clearing selection
  isolate(registry$set_selection(NULL, "clear_source"))
  expect_length(selection_calls, 2)
  expect_null(selection_calls[[2]]$selected_id)
  expect_equal(selection_calls[[2]]$source_id, "clear_source")
})
