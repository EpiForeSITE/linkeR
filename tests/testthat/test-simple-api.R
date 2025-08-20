test_that("link_plots validates inputs", {
  session <- shiny::MockShinySession$new()

  # Test missing session
  expect_error(
    link_plots(shared_id_column = "id"),
    "session argument is required"
  )

  # Test missing shared_id_column
  expect_error(
    link_plots(session),
    "shared_id_column argument is required"
  )

  # Test insufficient components
  expect_error(
    link_plots(session, comp1 = reactive({
      data.frame(id = 1)
    }), shared_id_column = "id"),
    "At least two components must be specified"
  )
})

test_that("component type detection works", {
  # Test map detection
  expect_equal(detect_component_type("myMap", NULL), "leaflet")
  expect_equal(detect_component_type("leaflet_output", NULL), "leaflet")

  # Test table detection
  expect_equal(detect_component_type("myTable", NULL), "datatable")
  expect_equal(detect_component_type("dt_output", NULL), "datatable")

  # Test unknown type
  expect_warning(
    result <- detect_component_type("unknown_component", NULL),
    "Could not auto-detect type"
  )
  expect_equal(result, "datatable")
})

test_that("link_plots creates registry with components", {
  session <- shiny::MockShinySession$new()

  # Create test data
  map_data <- reactive({
    data.frame(
      id = 1:3,
      longitude = c(-100, -101, -102),
      latitude = c(40, 41, 42),
      name = c("A", "B", "C")
    )
  })

  table_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      description = c("Place A", "Place B", "Place C")
    )
  })

  # Test linking (will produce messages about setup)
  expect_message(
    registry <- link_plots(
      session,
      testMap = map_data,
      testTable = table_data,
      shared_id_column = "id"
    ),
    "Linked 2 components"
  )

  # Check registry was created
  expect_s3_class(registry, "link_registry")

  # Check components were registered
  components <- registry$get_components()
  expect_length(components, 2)
  expect_true("mock-session-testMap" %in% names(components))
  expect_true("mock-session-testTable" %in% names(components))
})

test_that("link_plots works with 3 components", {
  session <- shiny::MockShinySession$new()

  # Create mock reactive data for three components
  data1 <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"))
  })

  data2 <- reactive({
    data.frame(id = 1:3, value = c(10, 20, 30))
  })

  data3 <- reactive({
    data.frame(id = 1:3, description = c("Desc A", "Desc B", "Desc C"))
  })

  # Link three components
  expect_message(
    registry <- link_plots(
      session,
      table1 = data1,
      table2 = data2,
      table3 = data3,
      shared_id_column = "id"
    ),
    "Linked 3 components"
  )

  # Check registry was created
  expect_s3_class(registry, "link_registry")
  components <- registry$get_components()
  expect_length(components, 3)
  expect_true("mock-session-table1" %in% names(components))
  expect_true("mock-session-table2" %in% names(components))
  expect_true("mock-session-table3" %in% names(components))

  # Check shared_id_column is consistent
  expect_equal(
    registry$get_components()[["table1"]]$shared_id_column,
    registry$get_components()[["table2"]]$shared_id_column
  )
  expect_equal(
    registry$get_components()[["table1"]]$shared_id_column,
    registry$get_components()[["table3"]]$shared_id_column
  )
  expect_equal(
    registry$get_components()[["table2"]]$shared_id_column,
    registry$get_components()[["table3"]]$shared_id_column
  )
})

test_that("link_plots handles edge cases correctly", {
  session <- shiny::MockShinySession$new()

  # Test with single component (should not work)
  single_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"))
  })

  registry <- create_link_registry(session)

  expect_error(
    link_plots(
      session,
      table1 = single_data,
      shared_id_column = "id"
    ),
    "At least two components must be specified for linking"
  )

  components <- registry$get_components()
  expect_length(components, 0)

  # Test with no components (should not work)
  expect_error(
    link_plots(session, shared_id_column = "id"),
    "At least two components must be specified for linking"
  )
  expect_length(registry$get_components(), 0)
})

test_that("link_plots validates shared_id_column across all datasets", {
  session <- shiny::MockShinySession$new()

  # Data with shared ID column
  good_data <- reactive({
    data.frame(id = 1:3, value = c("A", "B", "C"))
  })

  # Data missing shared ID column
  bad_data <- reactive({
    data.frame(value = c("X", "Y", "Z"))
  })

  # Should fail due to missing shared_id_column
  expect_error(
    link_plots(
      session,
      table1 = good_data,
      table2 = bad_data,
      shared_id_column = "id"
    ),
    "Component 'table2' data must contain the shared_id_column: id"
  )
})

test_that("link_plots handles different data types in shared column", {
  session <- shiny::MockShinySession$new()
  # Test with character IDs
  char_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("A", "B", "C")
    )
  })

  char_data2 <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_004"),
      description = c("Place A", "Place B", "Place D")
    )
  })

  expect_no_error({
    registry <- link_plots(
      session,
      table1 = char_data,
      table2 = char_data2,
      shared_id_column = "business_id"
    )
  })

  # Test with numeric IDs
  numeric_data <- reactive({
    data.frame(
      item_id = c(1, 2, 3),
      name = c("A", "B", "C")
    )
  })

  numeric_data2 <- reactive({
    data.frame(
      item_id = c(1, 2, 4),
      description = c("Item A", "Item B", "Item D")
    )
  })

  expect_no_error({
    registry <- link_plots(
      session,
      table1 = numeric_data,
      table2 = numeric_data2,
      shared_id_column = "item_id"
    )
  })
})

test_that("link_plots custom handlers are properly stored", {
  session <- shiny::MockShinySession$new()

  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })

  # Custom handler function
  custom_leaflet_handler <- function(map_proxy, selected_data, session) {
    # Custom behavior
    return("custom_result_map")
  }

  custom_dt_handler <- function(dt_proxy, selected_data, session) {
    # Custom behavior
    return("custom_result_table")
  }

  registry <- link_plots(
    session,
    test_map = test_data,
    test_table = test_data,
    shared_id_column = "id",
    leaflet_click_handler = custom_leaflet_handler,
    dt_click_handler = custom_dt_handler
  )

  components <- registry$get_components()

  # Check leaflet handler
  map_component <- components[["mock-session-test_map"]]
  expect_true(is.function(map_component$config$click_handler))

  # Check DT handler
  table_component <- components[["mock-session-test_table"]]
  expect_true(is.function(table_component$config$click_handler))

  # Check the click behavior
  expect_equal(
    map_component$config$click_handler(NULL, test_data()[1, ], session),
    "custom_result_map"
  )

  expect_equal(
    table_component$config$click_handler(NULL, test_data()[1, ], session),
    "custom_result_table"
  )
})

test_that("link_plots on_selection_change callback works", {
  session <- shiny::MockShinySession$new()

  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"))
  })

  test_data2 <- reactive({
    data.frame(id = 1:3, description = c("Desc A", "Desc B", "Desc C"))
  })

  # Track callback calls
  callback_calls <- list()

  test_callback <- function(selected_id, selected_data, source_id, session) {
    callback_calls <<- append(callback_calls, list(list(
      selected_id = selected_id,
      selected_data = selected_data,
      source_id = source_id
    )))
  }

  registry <- link_plots(
    session,
    test_table = test_data,
    test_table2 = test_data2,
    shared_id_column = "id",
    on_selection_change = test_callback
  )

  # The callback should be stored
  expect_true(is.function(registry$get_on_selection_change()))

  # Simulate a selection change
  test_data_val <- isolate(test_data())
  isolate(registry$set_selection(1, "test_table"))
  expect_length(callback_calls, 1)
  expect_equal(callback_calls[[1]]$selected_id, 1)
  expect_equal(callback_calls[[1]]$selected_data, test_data_val[1, ])
  expect_equal(callback_calls[[1]]$source_id, "test_table")
})
