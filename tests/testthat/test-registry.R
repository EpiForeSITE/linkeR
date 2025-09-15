test_that("create_link_registry works", {
  # Mock shiny session
  session <- shiny::MockShinySession$new()

  # Create registry
  registry <- create_link_registry(session)

  # Test registry structure
  expect_s3_class(registry, "link_registry")
  expect_true(is.function(registry$register_component))
  expect_true(is.function(registry$get_components))
})

test_that("register_component validates inputs", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)

  # Test missing component_id
  expect_error(
    registry$register_component(
      session = session,
      type = "test",
      data_reactive = reactive({
        data.frame(id = 1:3)
      }),
      shared_id_column = "id",
      component_id = NULL
    ),
    "component_id must be a string"
  )

  # Test non-reactive data
  expect_error(
    registry$register_component(
      session = session,
      component_id = "test",
      type = "test",
      data_reactive = data.frame(id = 1:3), # Not reactive
      shared_id_column = "id"
    ),
    "data_reactive must be a reactive expression"
  )
})

test_that("component registration works", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)

  # Create mock reactive data
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })

  # Register component (will produce message about unsupported type)
  expect_error(
    registry$register_component(
      session = session,
      component_id = "test_component",
      type = "test_type",
      data_reactive = test_data,
      shared_id_column = "id"
    ),
    "Unsupported component type: test_type"
  )

  # Check component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_equal(names(components), "mock-session-test_component")
  expect_equal(components[["mock-session-test_component"]]$type, "test_type")
  expect_equal(components[["mock-session-test_component"]]$shared_id_column, "id")
})

test_that("component registration works with supported types", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)

  # Create mock reactive data for DT (supported type)
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })

  # Register DT component (supported type)
  expect_no_error({
    registry$register_component(
      session = session,
      component_id = "test_table",
      type = "datatable",
      data_reactive = test_data,
      shared_id_column = "id"
    )
  })

  # Check component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_equal(names(components), "mock-session-test_table")
  expect_equal(components[["mock-session-test_table"]]$type, "datatable")
  expect_equal(components[["mock-session-test_table"]]$shared_id_column, "id")
})

test_that("leaflet component registration works", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)

  # Create mock reactive data for leaflet
  map_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })

  # Register leaflet component (supported type)
  expect_no_error({
    registry$register_component(
      session = session,
      component_id = "test_map",
      type = "leaflet",
      data_reactive = map_data,
      shared_id_column = "id",
      config = list(
        lng_col = "longitude",
        lat_col = "latitude",
        highlight_zoom = 13
      )
    )
  })

  # Check component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_equal(names(components), "mock-session-test_map")
  expect_equal(components[["mock-session-test_map"]]$type, "leaflet")
  expect_equal(components[["mock-session-test_map"]]$shared_id_column, "id")
  expect_equal(components[["mock-session-test_map"]]$config$lng_col, "longitude")
})

test_that("DT component registration works", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)

  # Create mock reactive data for DT
  table_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })

  # Register DT component (supported type)
  expect_no_error({
    registry$register_component(
      session = session,
      component_id = "test_table",
      type = "datatable",
      data_reactive = table_data,
      shared_id_column = "id"
    )
  })

  # Check component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_equal(names(components), "mock-session-test_table")
  expect_equal(components[["mock-session-test_table"]]$type, "datatable")
  expect_equal(components[["mock-session-test_table"]]$shared_id_column, "id")
})

test_that("registry manages multiple components correctly", {
  session <- shiny::MockShinySession$new()

  registry <- create_link_registry(session)

  # Create test data
  data1 <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      latitude = c(40, 41, 42),
      longitude = c(-100, -101, -102)
    )
  })

  data2 <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      value = c(10, 20, 30)
    )
  })

  # Register multiple components using the helper functions
  register_leaflet(session, registry, "map1", data1, "id")
  register_dt(session, registry, "table1", data2, "id")

  components <- registry$get_components()
  expect_length(components, 2)
  expect_true("mock-session-map1" %in% names(components))
  expect_true("mock-session-table1" %in% names(components))
})

test_that("selection state management works", {
  session <- shiny::MockShinySession$new()

  registry <- create_link_registry(session)

  # Test initial state
  selection <- isolate(registry$get_selection())  # Should be isolated to avoid triggering observers
  expect_null(selection$selected_id)
  expect_null(selection$source)

  # Test setting selection
  isolate(registry$set_selection("test_id", "test_source"))
  selection <- isolate(registry$get_selection())
  expect_equal(selection$selected_id, "test_id")
  expect_equal(selection$source, "test_source")

  # Test clearing selection
  isolate(registry$set_selection(NULL, "test_source"))
  selection <- isolate(registry$get_selection())
  expect_null(selection$selected_id)
  expect_equal(selection$source, "test_source")  # Source should remain
})

test_that("registry cleanup works", {
  session <- shiny::MockShinySession$new()

  registry <- create_link_registry(session)

  # Add some components
  data1 <- reactive({ data.frame(id = 1:3, name = c("A", "B", "C")) })
  register_dt(session, registry, "table1", data1, "id")

  # Set selection
  isolate(registry$set_selection("test_id", "test_source"))

  # Clear all
  registry$clear_all()

  # Should be empty
  components <- registry$get_components()
  expect_length(components, 0)

  selection <- isolate(registry$get_selection())
  expect_null(selection$selected_id)
})

test_that("registry handles duplicate component names", {
  session <- shiny::MockShinySession$new()
  
  registry <- create_link_registry(session)
  data1 <- reactive({ data.frame(id = 1:3, name = c("A", "B", "C")) })
  
  # Register same component twice
  register_dt(session, registry, "table1", data1, "id")
  register_dt(session, registry, "table1", data1, "id")  # Should overwrite
  
  components <- registry$get_components()
  expect_length(components, 1)  # Should only have one
})
