test_that("create_link_registry works", {
  # Mock shiny session
  session <- list(input = list())

  # Create registry
  registry <- create_link_registry(session)

  # Test registry structure
  expect_s3_class(registry, "link_registry")
  expect_true(is.function(registry$register_component))
  expect_true(is.function(registry$get_components))
})

test_that("register_component validates inputs", {
  session <- list(input = list())
  registry <- create_link_registry(session)

  # Test missing component_id
  expect_error(
    registry$register_component(
      type = "test",
      data_reactive = reactive({
        data.frame(id = 1:3)
      }),
      shared_id_column = "id"
    ),
    "component_id must be a single character string"
  )

  # Test non-reactive data
  expect_error(
    registry$register_component(
      component_id = "test",
      type = "test",
      data_reactive = data.frame(id = 1:3), # Not reactive
      shared_id_column = "id"
    ),
    "data_reactive must be a reactive expression"
  )
})

test_that("component registration works", {
  session <- list(input = list())
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
  expect_message(
    registry$register_component(
      component_id = "test_component",
      type = "test_type",
      data_reactive = test_data,
      shared_id_column = "id"
    ),
    "Unsupported component type"
  )

  # Check component was registered
  components <- registry$get_components()
  expect_length(components, 1)
  expect_equal(names(components), "test_component")
  expect_equal(components$test_component$type, "test_type")
  expect_equal(components$test_component$shared_id_column, "id")
})
