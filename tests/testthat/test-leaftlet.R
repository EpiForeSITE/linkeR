# Test cases for leaflet component registration and behavior
# Note that anything involving leaflet's actual behavior cannot be tested within this context

test_that("register_leaflet validates inputs", {
  # Mock session and registry
  session <- shiny::Mock'shiny'Session$new()
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
    register_leaflet(session, registry, "test_map", test_data, "id")
  })
  
  # Test missing registry
  expect_error(
    register_leaflet(session, NULL, "test_map", test_data, "id"),
    "registry"
  )

  # Test missing leaflet_output_id
  expect_error(
    register_leaflet(session, registry, NULL, test_data, "id"),
    "leaflet_output_id must be a string"
  )
  
  # Test non-reactive data
  expect_error(
    register_leaflet(session, registry, "test_map", data.frame(id = 1:3), "id"),
    "data_reactive must be a reactive expression"
  )
  
  # Test missing shared_id_column
  expect_error(
    register_leaflet(session, registry, "test_map", test_data, NULL),
    "shared_id_column must be a string"
  )
})

test_that("register_leaflet creates proper component registration", {
  session <- shiny::Mock'shiny'Session$new()

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
    session,
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
  expect_true("mock-session-business_map" %in% names(components))
  
  # Check component details
  leaflet_component <- components[["mock-session-business_map"]]
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
  
  session <- shiny::Mock'shiny'Session$new()
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
  register_leaflet(session, registry, "test_map", test_data, "id")
  
  components <- registry$get_components()
  leaflet_component <- components[["mock-session-test_map"]]
  
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
    session <- shiny::Mock'shiny'Session$new()
    registry <- create_link_registry(session)
    
    test_data <- reactive({
      data.frame(
        id = 1:3, 
        latitude = c(40, 41, 42),
        longitude = c(-100, -101, -102)
      )
    })
    
    expect_error(
      register_leaflet(session, registry, "test_map", test_data, "id"),
      "leaflet package is required"
    )
  }
})

test_that("setup_leaflet_observers creates proper observers", {
  skip_if_not_installed("leaflet")
  
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
  
  session <- shiny::Mock'shiny'Session$new()
  
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
    register_leaflet(session, registry, "char_map", char_data, "business_id")
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
    register_leaflet(session, registry, "numeric_map", numeric_data, "item_id")
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
    register_leaflet(session, registry, "factor_map", factor_data, "category_id")
  })
  
  # Check all were registered
  components <- registry$get_components()
  expect_length(components, 3)
  expect_true("mock-session-char_map" %in% names(components))
  expect_true("mock-session-numeric_map" %in% names(components))
  expect_true("mock-session-factor_map" %in% names(components))
})

test_that("leaflet integration with registry selection works", {
  skip_if_not_installed("leaflet")
  
  session <- shiny::Mock'shiny'Session$new()
  
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
  register_leaflet(session, registry, "test_map", test_data, "id")
  
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
  
  session <- shiny::Mock'shiny'Session$new()
  
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
    register_leaflet(session, registry, "invalid_map", invalid_coords_data, "id")
  })
  
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-invalid_map" %in% names(components))
})

test_that("leaflet handles sf objects correctly", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")
  
  session <- shiny::Mock'shiny'Session$new()
  
  # Create mock sf object
  mock_sf_data <- reactive({
    # Create simple point geometries
    points <- sf::st_sfc(
      sf::st_point(c(-74.0060, 40.7128)),
      sf::st_point(c(-87.6298, 41.8781)),
      sf::st_point(c(-71.0589, 42.3601))
    )
    
    sf::st_sf(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      geometry = points
    )
  })
  
  registry <- create_link_registry(session)
  
  # Should handle sf objects without error
  expect_no_error({
    register_leaflet(session, registry, "sf_map", mock_sf_data, "business_id")
  })
  
  # Check that component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-sf_map" %in% names(components))
  
  # Test the processed data by calling the reactive directly
  # (not through the component registry which might have additional wrapping)
  test_data <- isolate(mock_sf_data())
  processed_data <- process_sf_data(test_data, "longitude", "latitude")
  
  # The processed data should now have longitude/latitude columns
  expect_true("longitude" %in% names(processed_data))
  expect_true("latitude" %in% names(processed_data))
  expect_true("business_id" %in% names(processed_data))
  expect_true("name" %in% names(processed_data))

  expect_true("geometry" %in% names(processed_data))
  
  # Check coordinate values are reasonable
  expect_true(all(processed_data$longitude >= -180 & processed_data$longitude <= 180))
  expect_true(all(processed_data$latitude >= -90 & processed_data$latitude <= 90))
  
  # Verify the specific coordinate values match what we expect
  expect_equal(processed_data$longitude, c(-74.0060, -87.6298, -71.0589))
  expect_equal(processed_data$latitude, c(40.7128, 41.8781, 42.3601))
})

test_that("process_sf_data handles different input types", {
  skip_if_not_installed("sf")
  
  # Test with sf object
  points <- sf::st_sfc(
    sf::st_point(c(-74, 40)),
    sf::st_point(c(-75, 41))
  )
  sf_obj <- sf::st_sf(id = 1:2, geometry = points)
  
  result <- process_sf_data(sf_obj, "lon", "lat")
  expect_true("lon" %in% names(result))
  expect_true("lat" %in% names(result))
  expect_true("geometry" %in% names(result))
  expect_equal(result$lon, c(-74, -75))
  expect_equal(result$lat, c(40, 41))
  
  # Test with regular data frame (should pass through)
  regular_df <- data.frame(
    id = 1:2,
    longitude = c(-74, -75),
    latitude = c(40, 41)
  )
  
  result2 <- process_sf_data(regular_df, "longitude", "latitude")
  expect_identical(result2, regular_df)
  
  # Test with missing columns (should warn but return original)
  bad_df <- data.frame(id = 1:2, value = c(10, 20))
  expect_warning(
    result3 <- process_sf_data(bad_df, "longitude", "latitude"),
    "missing required coordinate columns"
  )
  expect_identical(result3, bad_df)
})

test_that("sf integration works with link_plots", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")
  skip_if_not_installed("DT")
  
  session <- shiny::Mock'shiny'Session$new()
  
  # Create sf data
  points <- sf::st_sfc(
    sf::st_point(c(-74, 40)),
    sf::st_point(c(-75, 41))
  )
  sf_data <- reactive({
    sf::st_sf(
      business_id = c("BIZ_001", "BIZ_002"),
      name = c("Company A", "Company B"),
      geometry = points
    )
  })
  
  # Create regular table data with same IDs
  table_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002"),
      name = c("Company A", "Company B"),
      revenue = c(100000, 200000)
    )
  })
  
  # Should link sf map with regular table
  expect_no_error({
    registry <- link_plots(
      session,
      business_map = sf_data,
      business_table = table_data,
      shared_id_column = "business_id"
    )
  })
  
  # Both components should be registered
  components <- registry$get_components()
  expect_length(components, 2)
  expect_true("mock-session-business_map" %in% names(components))
  expect_true("mock-session-business_table" %in% names(components))
  
  # Test that the sf data was processed correctly by testing the original data
  # rather than trying to access the wrapped reactive in the component
  original_sf <- isolate(sf_data())
  processed_sf <- process_sf_data(original_sf, "longitude", "latitude")
  
  expect_true("longitude" %in% names(processed_sf))
  expect_true("latitude" %in% names(processed_sf))
  expect_true("geometry" %in% names(processed_sf))
})

test_that("process_sf_data preserves geometry and adds coordinates", {
  skip_if_not_installed("sf")
  
  # Create a mock sf object with POINT geometries
  points <- sf::st_sfc(
    sf::st_point(c(-74, 40)),
    sf::st_point(c(-75, 41))
  )
  sf_obj <- sf::st_sf(id = 1:2, geometry = points)
  
  # Process the sf object
  result <- process_sf_data(sf_obj, "lon", "lat")
  
  # Check that the geometry is preserved and coordinates are added
  expect_true("geometry" %in% names(result))
  expect_true("lon" %in% names(result))
  expect_true("lat" %in% names(result))
  
  # Check coordinate values
  expect_equal(result$lon, c(-74, -75))
  expect_equal(result$lat, c(40, 41))
})

test_that("process_sf_data works for non-point data", {
  skip_if_not_installed("sf")
  
  # Create a mock sf object with POLYGON geometries
  polygons <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(-74, 40), c(-75, 40), c(-75, 41), c(-74, 41), c(-74, 40)))),
    sf::st_polygon(list(rbind(c(-76, 42), c(-77, 42), c(-77, 43), c(-76, 43), c(-76, 42))))
  )
  
  sf_obj <- sf::st_sf(id = 1:2, geometry = polygons)
  
  # Process the sf object
  result <- process_sf_data(sf_obj, "lon", "lat")
  
  # Check that the geometry is preserved and coordinates are added
  expect_true("geometry" %in% names(result))
  expect_true("lon" %in% names(result))
  expect_true("lat" %in% names(result))
  
  # Check coordinate values (should be centroids)
  expect_equal(result$lon, c(mean(c(-74, -75)), mean(c(-76, -77))))
  expect_equal(result$lat, c(mean(c(40, 41)), mean(c(42, 43))))
})
