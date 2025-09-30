# Test cases for Plotly component registration and behavior
# Note that anything involving Plotly's actual behavior cannot be tested within this context

test_that("register_plotly validates input", {
  # Mock session and registry
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  # Test with valid inputs
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), x = c(1, 2, 3), y = c(4, 5, 6))
  })

  expect_no_error({
    register_plotly(session, registry, "test_plot", test_data, "id")
  })
  
  # Test missing registry
  expect_error(
    register_plotly(session, NULL, "test_plot", test_data, "id"),
    "registry"
  )

  # Test missing plotly_output_id
  expect_error(
    register_plotly(session, registry, NULL, test_data, "id"),
    "plotly_output_id must be a string"
  )
  
  # Test non-reactive data
  expect_error(
    register_plotly(session, registry, "test_plot", data.frame(id = 1:3), "id"),
    "data_reactive must be a reactive expression"
  )
})

test_that("register_plotly creates proper component registration", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(
      business_id = c("BIZ_001", "BIZ_002", "BIZ_003"),
      name = c("Company A", "Company B", "Company C"),
      x = c(1, 2, 3),
      y = c(4, 5, 6),
      value = c(100, 200, 300)
    )
  })

  register_plotly(session, registry, "test_plot", test_data, "business_id")

  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-test_plot" %in% names(components))
  
  plotly_component <- components[["mock-session-test_plot"]]
  expect_equal(plotly_component$type, "plotly")
  expect_equal(plotly_component$shared_id_column, "business_id")
  expect_true(is.list(plotly_component$config))
})

test_that("register_plotly handles custom configuration", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(id = 1:3, x = c(1, 2, 3), y = c(4, 5, 6))
  })

  custom_handler <- function(plot_proxy, selected_data, session) {
    # Custom behavior
  }

  register_plotly(
    session, registry, "test_plot", test_data, "id",
    event_types = c("plotly_click", "plotly_selected"),
    source = "custom_source",
    click_handler = custom_handler
  )

  components <- registry$get_components()
  plotly_component <- components[["mock-session-test_plot"]]
  
  expect_equal(plotly_component$config$event_types, c("plotly_click", "plotly_selected"))
  expect_equal(plotly_component$config$source, "custom_source")
  expect_equal(plotly_component$config$click_handler, custom_handler)
})

test_that("register_plotly uses default values correctly", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(id = 1:3, x = c(1, 2, 3), y = c(4, 5, 6))
  })

  register_plotly(session, registry, "test_plot", test_data, "id")

  components <- registry$get_components()
  plotly_component <- components[["mock-session-test_plot"]]
  
  # Default event types
  expect_equal(plotly_component$config$event_types, c("plotly_click"))
  # Default source should be the plotly_output_id
  expect_equal(plotly_component$config$source, "test_plot")
  # Default click handler should be NULL
  expect_null(plotly_component$config$click_handler)
})

test_that("update_plotly_selection validates inputs", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(
      id = 1:3,
      name = c("A", "B", "C"),
      x = c(1, 2, 3),
      y = c(4, 5, 6)
    )
  })

  register_plotly(session, registry, "test_plot", test_data, "id")
  components <- registry$get_components()

  # Should not error with valid inputs
  expect_no_error({
    update_plotly_selection("test_plot", 2, session, components)
  })

  # Should handle NULL selected_id (deselection)
  expect_no_error({
    update_plotly_selection("test_plot", NULL, session, components)
  })

  # Should handle non-existent component gracefully
  expect_no_error({
    update_plotly_selection("non_existent", 1, session, components)
  })
})

test_that("setup_plotly_observers creates observers", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  shared_state <- reactiveValues()
  
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), x = c(1, 2, 3), y = c(4, 5, 6))
  })

  register_plotly(session, registry, "test_plot", test_data, "id")
  components <- registry$get_components()

  on_selection_change <- function(selected_id) {
    # Mock callback
  }

  observers <- setup_plotly_observers(
    "mock-session-test_plot", session, components, shared_state, on_selection_change, registry
  )

  # Should return a list of observers
  expect_true(is.list(observers))
  expect_equal(length(observers), 2)
  
  # Each observer should be an observer object
  expect_true(all(sapply(observers, function(obs) inherits(obs, "Observer"))))
})

test_that("setup_plotly_observers handles missing component", {
  session <- shiny::MockShinySession$new()
  shared_state <- reactiveValues()
  components <- list()  # Empty components list

  on_selection_change <- function(selected_id) {
    # Mock callback
  }

  # Should handle missing component gracefully and issue warning
  expect_warning({
    observers <- setup_plotly_observers(
      "non_existent", session, components, shared_state, on_selection_change
    )
  }, "Component info not found")
})

test_that("apply_default_plotly_behavior handles selection and deselection", {
  # This test is limited since we can't actually test plotly interactions
  # But we can test that the function runs without error
  
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), x = c(1, 2, 3), y = c(4, 5, 6))
  })

  register_plotly(session, registry, "test_plot", test_data, "id")
  components <- registry$get_components()
  
  # Store component info in session userData for the function to access
  session$userData[["linkeR_components"]] <- components

  # Mock plotly proxy
  plot_proxy <- list(session = session, outputId = "test_plot")
  class(plot_proxy) <- "plotlyProxy"
  
  selected_data <- data.frame(id = 2, name = "B", x = 2, y = 5)

  # Should not error when applying behavior
  expect_no_error({
    apply_default_plotly_behavior(plot_proxy, selected_data, session, "test_plot")
  })

  # Should not error when clearing selection
  expect_no_error({
    apply_default_plotly_behavior(plot_proxy, NULL, session, "test_plot")
  })
})

test_that("plotly functions handle missing component gracefully", {
  session <- shiny::MockShinySession$new()
  components <- list()  # Empty components list
  
  # Should return early and not error
  expect_no_error({
    result <- update_plotly_selection("test_plot", 1, session, components)
  })
})

test_that("plotly registration works with valid column", {
  session <- shiny::MockShinySession$new()
  registry <- create_link_registry(session)
  
  test_data <- reactive({
    data.frame(id = 1:3, name = c("A", "B", "C"), x = c(1, 2, 3), y = c(4, 5, 6))
  })

  # Should work with valid column
  expect_no_error({
    register_plotly(session, registry, "test_plot1", test_data, "id")
  })
  
  # Verify registration worked
  components <- registry$get_components()
  expect_length(components, 1)
  expect_true("mock-session-test_plot1" %in% names(components))
})