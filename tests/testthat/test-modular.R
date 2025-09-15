test_that("registry handles multiple sessions correctly", {
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  
  # Create separate sessions (simulating different modules)
  main_session <- shiny::Mock'shiny'Session$new()
  map_session <- main_session$makeScope("map-module")
  table_session <- main_session$makeScope("table-module")
  
  # Create shared registry (in real apps, this would be passed between modules)
  registry <- create_link_registry(main_session)
  
  # Create test data
  locations_data <- reactive({
    data.frame(
      facility_id = c("FAC_001", "FAC_002", "FAC_003"),
      name = c("Facility A", "Facility B", "Facility C"),
      latitude = c(40.7128, 41.8781, 42.3601),
      longitude = c(-74.0060, -87.6298, -71.0589)
    )
  })
  
  facilities_data <- reactive({
    data.frame(
      facility_id = c("FAC_001", "FAC_002", "FAC_003"),
      name = c("Facility A", "Facility B", "Facility C"),
      status = c("Active", "Maintenance", "Active"),
      capacity = c(1000, 1500, 800)
    )
  })
  
  # Register components from different sessions (simulating modules)
  expect_no_error({
    register_leaflet(
      session = map_session,
      registry = registry,
      leaflet_output_id = "facility_map",
      data_reactive = locations_data,
      shared_id_column = "facility_id"
    )
  })
  
  expect_no_error({
    register_dt(
      session = table_session,
      registry = registry,
      dt_output_id = "facility_table", 
      data_reactive = facilities_data,
      shared_id_column = "facility_id"
    )
  })
  
  # Verify both components are registered
  components <- registry$get_components()
  expect_length(components, 2)
  
  # Check that component names are properly stored
  component_names <- names(components)
  expect_true(any(grepl(map_session$ns("facility_map"), component_names)))
  expect_true(any(grepl(table_session$ns("facility_table"), component_names)))
})

test_that("cross-session selection works correctly", {
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  
  # Setup separate sessions
  main_session <- shiny::Mock'shiny'Session$new()
  map_session <- main_session$makeScope("map-module")
  table_session <- main_session$makeScope("table-module")
  
  # Track selection changes
  selection_history <- list()
  callback <- function(selected_id, selected_data, source_id, session) {
    selection_history <<- append(selection_history, list(list(
      selected_id = selected_id,
      source_id = source_id
    )))
  }
  
  registry <- create_link_registry(main_session, on_selection_change = callback)
  
  # Test data
  test_data <- reactive({
    data.frame(
      id = c("A", "B", "C"),
      name = c("Item A", "Item B", "Item C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })
  
  # Register components (simulating different modules)
  register_leaflet(
    session = map_session,
    registry = registry,
    leaflet_output_id = "map",
    data_reactive = test_data,
    shared_id_column = "id"
  )
  
  register_dt(
    session = table_session,
    registry = registry,
    dt_output_id = "table",
    data_reactive = test_data,
    shared_id_column = "id"
  )
  
  # Simulate selection from map component
  isolate({
    registry$set_selection("B", "map")
  })
  
  # Check selection was propagated
  expect_length(selection_history, 1)
  expect_equal(selection_history[[1]]$selected_id, "B")
  expect_equal(selection_history[[1]]$source_id, "map")
  
  # Simulate selection from table component
  isolate({
    registry$set_selection("C", "table")
  })
  
  expect_length(selection_history, 2)
  expect_equal(selection_history[[2]]$selected_id, "C")
  expect_equal(selection_history[[2]]$source_id, "table")
})

test_that("separate sessions maintain component isolation", {
  skip_if_not_installed("DT")
  
  # Create separate sessions
  session1 <- shiny::Mock'shiny'Session$new()
  session2 <- shiny::Mock'shiny'Session$new()
  
  # Create separate registries (simulating different app instances)
  registry1 <- create_link_registry(session1)
  registry2 <- create_link_registry(session2)
  
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })
  
  # Register same component name in different registries
  register_dt(session1, registry1, "data_table", test_data, "id")
  register_dt(session2, registry2, "data_table", test_data, "id")
  
  # Each registry should only have one component
  components1 <- registry1$get_components()
  components2 <- registry2$get_components()
  
  expect_length(components1, 1)
  expect_length(components2, 1)
  
  # Selections should be isolated
  isolate({
    registry1$set_selection("1", "data_table")
    registry2$set_selection("2", "data_table")
  })
  
  selection1 <- isolate(registry1$get_selection())
  selection2 <- isolate(registry2$get_selection())
  
  expect_equal(selection1$selected_id, "1")
  expect_equal(selection2$selected_id, "2")
})

test_that("error handling in registration", {
  skip_if_not_installed("DT")
  
  main_session <- shiny::Mock'shiny'Session$new()
  
  # Test with invalid registry
  expect_error({
    register_dt(
      registry = NULL,
      dt_output_id = "test_table",
      data_reactive = reactive(data.frame(id = 1:2)),
      shared_id_column = "id"
    )
  }, "registry")
  
  # Test with invalid reactive
  registry <- create_link_registry(main_session)
  
  expect_error({
    register_dt(
      registry = registry,
      dt_output_id = "test_table", 
      data_reactive = data.frame(id = 1:2),  # Not reactive!
      shared_id_column = "id"
    )
  }, "reactive")
})

test_that("custom handlers work in multi-component setup", {
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  
  main_session <- shiny::Mock'shiny'Session$new()
  map_session <- main_session$makeScope("map-module")
  table_session <- main_session$makeScope("table-module")

  registry <- create_link_registry(main_session)
  
  # Track custom handler calls
  handler_calls <- list()
  
  custom_leaflet_handler <- function(map_proxy, selected_data, session) {
    handler_calls <<- append(handler_calls, list(list(
      type = "leaflet",
      data = selected_data
    )))
  }
  
  custom_dt_handler <- function(dt_proxy, selected_data, session) {
    handler_calls <<- append(handler_calls, list(list(
      type = "dt", 
      data = selected_data
    )))
  }
  
  test_data <- reactive({
    data.frame(
      id = c("ITEM_1", "ITEM_2"),
      name = c("Item One", "Item Two"),
      latitude = c(40.7, 40.8),
      longitude = c(-74.0, -74.1)
    )
  })
  
  # Register with custom handlers
  register_leaflet(
    session = map_session,
    registry = registry,
    leaflet_output_id = "overview_map",
    data_reactive = test_data,
    shared_id_column = "id",
    click_handler = custom_leaflet_handler
  )
  
  register_dt(
    session = table_session,
    registry = registry,
    dt_output_id = "detail_table",
    data_reactive = test_data,
    shared_id_column = "id",
    click_handler = custom_dt_handler
  )
  
  components <- registry$get_components()
  expect_length(components, 2)
  
  # Verify custom handlers are stored
  for (component in components) {
    expect_true(is.function(component$config$click_handler))
  }
})

test_that("registry state management across components", {
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  
  main_session <- shiny::Mock'shiny'Session$new()
  map_session <- main_session$makeScope("map-module")
  table_session <- main_session$makeScope("table-module")
  table_session2 <- main_session$makeScope("table-module-2")
  
  # Track state changes
  state_changes <- list()
  callback <- function(selected_id, selected_data, source_id, session) {
    state_changes <<- append(state_changes, list(list(
      selected_id = selected_id,
      source = source_id,
      timestamp = Sys.time()
    )))
  }
  
  registry <- create_link_registry(main_session, on_selection_change = callback)
  
  test_data <- reactive({
    data.frame(
      id = c("STATE_1", "STATE_2", "STATE_3"),
      name = c("State One", "State Two", "State Three"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })
  
  # Register multiple components
  register_leaflet(map_session, registry, "state_map", test_data, "id")
  register_dt(table_session, registry, "state_table", test_data, "id")
  register_dt(table_session2, registry, "state_details", test_data, "id")
  
  # Test sequence of selections
  isolate({
    registry$set_selection("STATE_1", "state_map")
    registry$set_selection("STATE_2", "state_table")
    registry$set_selection("STATE_3", "state_details")
  })
  
  # Check state progression
  expect_length(state_changes, 3)
  expect_equal(state_changes[[1]]$selected_id, "STATE_1")
  expect_equal(state_changes[[2]]$selected_id, "STATE_2")
  expect_equal(state_changes[[3]]$selected_id, "STATE_3")
  
  # Final state should be STATE_3
  final_selection <- isolate(registry$get_selection())
  expect_equal(final_selection$selected_id, "STATE_3")
  expect_equal(final_selection$source, "state_details")
})