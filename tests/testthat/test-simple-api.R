test_that("link_plots validates inputs", {
  session <- list(input = list())

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
  session <- list(input = list())

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
  expect_true("testMap" %in% names(components))
  expect_true("testTable" %in% names(components))
})
