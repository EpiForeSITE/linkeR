# Test cases for leaflet component registration and behavior
# Note that anything involving leaflet's actual behavior cannot be tested within this context

test_that("register_leaflet validates inputs", {
  # Mock session and registry
  session <- list(
    input = list(),
    onSessionEnded = function(callback) callback
  )
  registry <- create_link_registry(session)
  
  # Test with valid inputs
  test_data <- reactive({
    data.frame(
      id = 1:3, 
      name = c("A", "B", "C"), 
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })

  expect_no_error({
    register_leaflet(registry, "test_map", test_data, "id")
  })
  
  # Test missing registry
  expect_error(
    register_leaflet(NULL, "test_map", test_data, "id"),
    "registry"
  )

  # Test missing leaflet_output_id
  expect_error(
    register_leaflet(registry, NULL, test_data, "id"),
    "leaflet_output_id must be a string"
  )
  
  # Test non-reactive data
  expect_error(
    register_leaflet(registry, "test_map", data.frame(id = 1:3), "id"),
    "data_reactive must be a reactive expression"
  )
  
  # Test missing shared_id_column
  expect_error(
    register_leaflet(registry, "test_map", test_data, NULL),
    "shared_id_column must be a string"
  )
})

test_that("register_leaflet creates proper component registration", {
  session <- list(
    input = list(),
    onSessionEnded = function(callback) callback
  )
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      latitude = c(40.7128, 41.8781, 42.3601),
      longitude = c(-74.0060, -87.6298, -71.0589)
    )
  })

  # Register the leaflet component with custom settings
  register_leaflet(
    registry, 
    "business_map", 
    test_data, 
    "business_id",
    lng_col = "longitude",
    lat_col = "latitude", 
    highlight_zoom = 15
  )
  
  # Check component was registered correctly
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("business_map" %in% names(components))
  
  # Check component details
  leaflet_component <- components[["business_map"]]
  expect_equal(leaflet_component$type, "leaflet")
  expect_equal(leaflet_component$shared_id_column, "business_id")
  expect_true(is.list(leaflet_component$config))
  
  # Check leaflet-specific config
  expect_equal(leaflet_component$config$lng_col, "longitude")
  expect_equal(leaflet_component$config$lat_col, "latitude")
  expect_equal(leaflet_component$config$highlight_zoom, 15)
  expect_true(!is.null(leaflet_component$config$highlight_icon))
})

test_that("register_leaflet uses default column names", {
  skip_if_not_installed("leaflet")
  
  session <- list(
    input = list(),
    onSessionEnded = function(callback) callback
  )
  registry <- create_link_registry(session)
  
  # Data with default column names
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })

  # Register without specifying column names (should use defaults)
  register_leaflet(registry, "test_map", test_data, "id")
  
  components <- registry$get_components()
  leaflet_component <- components[["test_map"]]
  
  # Should use default column names
  expect_equal(leaflet_component$config$lng_col, "longitude")
  expect_equal(leaflet_component$config$lat_col, "latitude")
  expect_equal(leaflet_component$config$highlight_zoom, 12) # default zoom
})

test_that("register_leaflet requires leaflet package", {
  # Mock missing leaflet package scenario would require sophisticated mocking
  # This test documents the expected behavior
  if (requireNamespace("leaflet", quietly = TRUE)) {
    skip("leaflet package is available, cannot test missing package scenario")
  } else {
    session <- list(
      input = list(),
      onSessionEnded = function(callback) callback
    )
    registry <- create_link_registry(session)
    
    test_data <- reactive({
      data.frame(
        id = 1:3, 
        latitude = c(40, 41, 42),
        longitude = c(-100, -101, -102)
      )
    })
    
    expect_error(
      register_leaflet(registry, "test_map", test_data, "id"),
      "leaflet package is required"
    )
  }
})

test_that("setup_leaflet_observers creates proper observers", {
  skip_if_not_installed("leaflet")
  
  session <- list(
    input = list(
      test_map_marker_click = NULL
    ),
    userData = list(),
    onSessionEnded = function(callback) callback
  )
  
  # Mock reactive values for shared state
  shared_state <- shiny::reactiveValues(
    selected_id = NULL,
    selection_source = NULL
  )
  
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })
  
  components <- list(
    test_map = list(
      data_reactive = test_data,
      shared_id_column = "id",
      config = list(
        lng_col = "longitude",
        lat_col = "latitude",
        highlight_zoom = 12
      )
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
    observers <- setup_leaflet_observers(
      "test_map", 
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

test_that("apply_default_leaflet_behavior handles selection and deselection", {
  skip_if_not_installed("leaflet")
  
  # Mock leaflet proxy
  mock_proxy <- structure(list(), class = "leaflet")
  
  # Mock component info
  component_info <- list(
    shared_id_column = "id",
    config = list(
      lng_col = "longitude",
      lat_col = "latitude",
      highlight_zoom = 12
    )
  )
  
  # Test with selection
  selected_data <- data.frame(
    id = "test_id",
    name = "Test Location",
    latitude = 40.7128,
    longitude = -74.0060
  )
  
  # Function should not error (can't test full leaflet functionality in unit tests)
  expect_no_error({
    apply_default_leaflet_behavior(mock_proxy, selected_data, component_info)
  })
  
  # Test with deselection (NULL data)
  expect_no_error({
    apply_default_leaflet_behavior(mock_proxy, NULL, component_info)
  })
})

test_that("leaflet handles different data types in shared column", {
  skip_if_not_installed("leaflet")
  
  session <- list(
    input = list(),
    userData = list(),
    onSessionEnded = function(callback) callback
  )
  
  registry <- create_link_registry(session)
  
  # Test with character IDs
  char_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102),
      stringsAsFactors = FALSE
    )
  })
  
  expect_no_error({
    register_leaflet(registry, "char_map", char_data, "business_id")
  })
  
  # Test with numeric IDs
  numeric_data <- reactive({
    data.frame(
      item_id = c(1, 2, 3),
      name = c("Location A", "Location B", "Location C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })
  
  expect_no_error({
    register_leaflet(registry, "numeric_map", numeric_data, "item_id")
  })
  
  # Test with factor IDs
  factor_data <- reactive({
    data.frame(
      category_id = factor(c("CAT_A", "CAT_B", "CAT_C")),
      name = c("Category A", "Category B", "Category C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })
  
  expect_no_error({
    register_leaflet(registry, "factor_map", factor_data, "category_id")
  })
  
  # Check all were registered
  components <- registry$get_components()
  expect_length(components, 3)
  expect_true("char_map" %in% names(components))
  expect_true("numeric_map" %in% names(components))
  expect_true("factor_map" %in% names(components))
})

test_that("leaflet integration with registry selection works", {
  skip_if_not_installed("leaflet")
  
  session <- list(
    input = list(),
    userData = list(),
    onSessionEnded = function(callback) callback
  )
  
  test_data <- reactive({
    data.frame(
      id = c("A", "B", "C"),
      name = c("Alice", "Bob", "Charlie"),
      latitude = c(40.7128, 41.8781, 42.3601),
      longitude = c(-74.0060, -87.6298, -71.0589)
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
  register_leaflet(registry, "test_map", test_data, "id")
  
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

test_that("leaflet handles coordinate validation", {
  skip_if_not_installed("leaflet")
  
  session <- list(
    input = list(),
    userData = list(),
    onSessionEnded = function(callback) callback
  )
  
  # Test with invalid coordinates (should still register but may cause issues)
  invalid_coords_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(NA, 200, -200),  # Invalid latitudes
      longitude = c(-500, NA, 500)  # Invalid longitudes
    )
  })
  
  registry <- create_link_registry(session)
  
  # Should still register (validation happens at runtime)
  expect_no_error({
    register_leaflet(registry, "invalid_map", invalid_coords_data, "id")
  })
  
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("invalid_map" %in% names(components))
})
