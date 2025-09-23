library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(linkeR)
library(bslib)

# Generate sample business data for different linking scenarios
generate_business_data <- function() {
  set.seed(42) # For reproducible data

  n_businesses <- 50

  # Business categories
  categories <- c("Restaurant", "Retail", "Healthcare", "Technology", "Manufacturing")

  # Generate realistic business data
  business_data <- data.frame(
    business_id = paste0("BIZ_", sprintf("%03d", 1:n_businesses)),
    name = paste("Business", 1:n_businesses),
    category = sample(categories, n_businesses, replace = TRUE),

    # Location data (scattered across a fictional city)
    latitude = runif(n_businesses, 40.7000, 40.8000),
    longitude = runif(n_businesses, -111.9500, -111.8500),

    # Financial metrics
    annual_revenue = round(runif(n_businesses, 50000, 5000000), -3),
    employees = sample(1:500, n_businesses, replace = TRUE),

    # Performance metrics
    customer_rating = round(runif(n_businesses, 2.5, 5.0), 1),
    years_in_business = sample(1:25, n_businesses, replace = TRUE),

    # Risk/compliance data
    compliance_score = round(runif(n_businesses, 60, 100)),
    risk_level = sample(c("Low", "Medium", "High"), n_businesses,
      replace = TRUE, prob = c(0.5, 0.3, 0.2)
    ),

    # Time series data (monthly revenue for past 12 months)
    stringsAsFactors = FALSE
  )

  # Add monthly revenue data for time series
  for (i in 1:12) {
    month_col <- paste0("month_", sprintf("%02d", i))
    business_data[[month_col]] <- round(business_data$annual_revenue / 12 *
      runif(n_businesses, 0.7, 1.3), -2)
  }

  return(business_data)
}

# UI with multiple tabs demonstrating different complexity levels
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Business Analytics Dashboard - Multiple Linking Scenarios"),
  tabsetPanel(
    id = "main_tabs",

    # Tab 1: Simple 2-Way Linking
    tabPanel(
      "Simple Linking",
      value = "simple",
      br(),
      div(
        class = "alert alert-info",
        h4("Scenario 1: Simple 2-Way Linking"),
        p("Basic map ↔ table linking with default behaviors. Click markers or table rows.")
      ),
      fluidRow(
        column(
          6,
          h4("Business Locations"),
          leafletOutput("simple_map", height = "400px")
        ),
        column(
          6,
          h4("Business Locations 2"),
          leafletOutput("simple_map2", height = "400px")
        ),
        column(
          6,
          h4("Business Directory"),
          DTOutput("simple_table")
        )
      ),
      fluidRow(
        column(
          12,
          br(),
          h4("Selection Details"),
          verbatimTextOutput("simple_selection")
        )
      )
    ),

    # Tab 2: Custom Behavior Linking
    tabPanel(
      "Custom Behaviors",
      value = "custom",
      br(),
      div(
        class = "alert alert-warning",
        h4("Scenario 2: Custom Click Behaviors"),
        p("Map shows detailed popups with custom styling. Table selections trigger business analysis.")
      ),
      fluidRow(
        column(
          6,
          h4("Enhanced Business Map"),
          leafletOutput("custom_map", height = "400px")
        ),
        column(
          6,
          h4("Business Analysis Table"),
          DTOutput("custom_table")
        )
      ),
      fluidRow(
        column(
          12,
          br(),
          uiOutput("custom_business_details")
        )
      )
    ),

    # Tab 3: Multi-Component Linking
    tabPanel(
      "Multi-Component",
      value = "multi",
      br(),
      div(
        class = "alert alert-success",
        h4("Scenario 3: Multiple Component Linking with Multiple Selection Support"),
        p("Map ↔ Table ↔ Chart ↔ Summary. All components linked bidirectionally."),
        p("New: Supports multiple selections! Use Ctrl+click in table, brush selection in chart.")
      ),
      fluidRow(
        column(
          4,
          h5("Geographic View"),
          leafletOutput("multi_map", height = "300px"),
          br(),
          h5("Performance Metrics"),
          plotlyOutput("multi_chart", height = "250px")
        ),
        column(
          4,
          h5("Business Data"),
          DTOutput("multi_table"),
        ),
        column(
          4,
          h5("Time Series Analysis"),
          plotlyOutput("multi_timeseries", height = "300px"),
          br(),
          h5("Selection Info"),
          verbatimTextOutput("multi_selection"),
          br(),
          actionButton("clear_multi_selection", "Clear Selection", class = "btn-secondary")
        )
      )
    ),
  )
)

server <- function(input, output, session) {
  # Generate shared business data
  business_data <- reactive({
    generate_business_data()
  })

  # Store registries for each scenario
  registries <- reactiveValues(
    simple = NULL,
    custom = NULL,
    multi = NULL,
    advanced = NULL
  )

  # =============================================================================
  # SCENARIO 1: SIMPLE 2-WAY LINKING
  # =============================================================================

  # Simple map
  output$simple_map <- renderLeaflet({
    data <- business_data()

    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~business_id,
      ) %>%
      setView(lng = -111.9000, lat = 40.7500, zoom = 11)
  })

  output$simple_map2 <- renderLeaflet({
    data <- business_data()

    leaflet(data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~business_id,
      ) %>%
      setView(lng = -111.9000, lat = 40.7500, zoom = 11)
  })

  # Simple table
  output$simple_table <- renderDT({
    data <- business_data()

    simple_data <- data[, c("name", "category", "annual_revenue", "employees")]

    datatable(
      simple_data,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    ) %>%
      formatCurrency("annual_revenue", currency = "$", digits = 0)
  })

  # Initialize simple linking
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "simple" && is.null(registries$simple)) {
      registries$simple <- linkeR::link_plots(
        session,
        simple_map = business_data,
        simple_map2 = business_data,
        simple_table = business_data,
        shared_id_column = "business_id",
        on_selection_change = function(selected_id, selected_data, source_id, session) {
          # Simple notification
          if (!is.null(selected_data)) {
            showNotification(
              paste("Selected:", selected_data$name),
              type = "default",
              duration = 2
            )
          }
        }
      )
    }
  })

  # Simple selection display
  output$simple_selection <- renderText({
    if (!is.null(registries$simple)) {
      selection <- registries$simple$get_selection()
      if (!is.null(selection$selected_id)) {
        data <- business_data()
        selected <- data[data$business_id == selection$selected_id, ]
        if (nrow(selected) > 0) {
          return(paste0(
            "Selected Business: ", selected$name, "\n",
            "Category: ", selected$category, "\n",
            "Revenue: $", format(selected$annual_revenue, big.mark = ","), "\n",
            "Employees: ", selected$employees, "\n",
            "Rating: ", selected$customer_rating, "/5.0"
          ))
        }
      }
    }
    return("No business selected")
  })

  # =============================================================================
  # SCENARIO 2: CUSTOM CLICK BEHAVIORS
  # =============================================================================

  # Custom styled map
  output$custom_map <- renderLeaflet({
    data <- business_data()

    # Color by category - compute colors beforehand
    color_palette <- c(
      "Restaurant" = "red", "Retail" = "blue", "Healthcare" = "green",
      "Technology" = "purple", "Manufacturing" = "orange"
    )

    # Add color column to the data
    data$marker_color <- color_palette[data$category]

    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~business_id,
        radius = ~ sqrt(annual_revenue / 100000) + 3,
        color = ~marker_color, # Use the pre-computed color column
        fillColor = ~marker_color, # Also set fillColor for better visibility
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        opacity = 1 # Make sure the stroke is visible
      ) %>%
      setView(lng = -111.9000, lat = 40.7500, zoom = 11)
  })

  # Custom table
  output$custom_table <- renderDT({
    data <- business_data()

    custom_data <- data[, c("name", "category", "customer_rating", "compliance_score", "risk_level")]

    datatable(
      custom_data,
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    ) %>%
      formatStyle("risk_level",
        backgroundColor = styleEqual(
          c("Low", "Medium", "High"),
          c("lightgreen", "yellow", "lightcoral")
        )
      )
  })

  # Initialize custom linking
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "custom" && is.null(registries$custom)) {
      registries$custom <- linkeR::link_plots(
        session,
        custom_map = business_data,
        custom_table = business_data,
        shared_id_column = "business_id",

        # Custom leaflet click handler
        leaflet_click_handler = function(map_proxy, selected_data, session) {
          if (!is.null(selected_data)) {
            # Rich popup with business details
            popup_content <- paste0(
              "<div style='min-width: 200px;'>",
              "<h6 style='margin-bottom: 10px; color: #2c3e50;'>", selected_data$name, "</h4>",
              "<hr style='margin: 5px 0;'>",
              "<span><strong>Category:</strong> ", selected_data$category, "</span><br />",
              "<span><strong>Revenue:</strong> $", format(selected_data$annual_revenue, big.mark = ","), "</span><br />",
              "<span><strong>Employees:</strong> ", selected_data$employees, "</span><br />",
              "<span><strong>Rating:</strong> ", selected_data$customer_rating, "/5.0 ⭐</span><br />",
              "<span><strong>Risk Level:</strong> <span style='color: ",
              switch(selected_data$risk_level,
                "Low" = "green",
                "Medium" = "orange",
                "High" = "red"
              ), ";'>",
              selected_data$risk_level, "</span></span>",
              "</div>"
            )

            map_proxy %>%
              leaflet::setView(lng = selected_data$longitude, lat = selected_data$latitude, zoom = 14) %>%
              leaflet::clearPopups() %>%
              leaflet::addPopups(
                lng = selected_data$longitude,
                lat = selected_data$latitude,
                popup = popup_content
              )
          }
        }
      )
    }
  })

  # Custom business details panel
  output$custom_business_details <- renderUI({
    if (!is.null(registries$custom)) {
      selection <- registries$custom$get_selection()
      if (!is.null(selection$selected_id)) {
        data <- business_data()
        business <- data[data$business_id == selection$selected_id, ]

        if (nrow(business) > 0) {
          business <- business[1, ]

          # Create analysis cards
          fluidRow(
            column(
              3,
              div(
                class = "card",
                div(class = "card-header", h5("Financial Health")),
                div(
                  class = "card-body",
                  p("Annual Revenue: $", format(business$annual_revenue, big.mark = ",")),
                  p("Revenue per Employee: $", format(round(business$annual_revenue / business$employees), big.mark = ",")),
                  p("Business Age: ", business$years_in_business, " years")
                )
              )
            ),
            column(
              3,
              div(
                class = "card",
                div(class = "card-header", h5("Performance")),
                div(
                  class = "card-body",
                  p("Customer Rating: ", business$customer_rating, "/5.0"),
                  p("Compliance Score: ", business$compliance_score, "%"),
                  p("Risk Assessment: ", business$risk_level)
                )
              )
            ),
            column(
              3,
              div(
                class = "card",
                div(class = "card-header", h5("Operations")),
                div(
                  class = "card-body",
                  p("Employee Count: ", business$employees),
                  p("Category: ", business$category),
                  p("Location: ", round(business$latitude, 4), ", ", round(business$longitude, 4))
                )
              )
            ),
          )
        }
      }
    }
  })

  # =============================================================================
  # SCENARIO 3: MULTI-COMPONENT LINKING
  # =============================================================================

  # Multi map
  output$multi_map <- renderLeaflet({
    data <- business_data()

    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~business_id,
        radius = 5,
        fillOpacity = 0.8
      ) %>%
      setView(lng = -111.9000, lat = 40.7500, zoom = 11)
  })

  # Multi table
  output$multi_table <- renderDT({
    data <- business_data()

    multi_data <- data[, c("name", "category", "annual_revenue")]

    datatable(
      multi_data,
      selection = "multiple", # Enable multiple selection
      rownames = FALSE,
      options = list(pageLength = 6, scrollX = TRUE)
    ) %>%
      formatCurrency("annual_revenue", currency = "$", digits = 0)
  })

  # Multi chart (scatter plot)
  output$multi_chart <- renderPlotly({
    data <- business_data()

    p <- plot_ly(
      data = data,
      x = ~employees,
      y = ~annual_revenue,
      color = ~category,
      key = ~business_id, # This is crucial for linking!
      text = ~ paste("Name:", name, "<br>Category:", category, "<br>Employees:", employees, "<br>Revenue: $", format(annual_revenue, big.mark = ",")),
      type = "scatter", # Explicitly specify the trace type
      mode = "markers", # Explicitly specify the mode
      source = "multi_chart", # Source ID for plotly linking
      hovertemplate = "%{text}<extra></extra>" # Custom hover template
    ) %>%
      layout(
        title = list(text = "Revenue vs Employees", font = list(size = 14)),
        xaxis = list(title = "Number of Employees"),
        yaxis = list(title = "Annual Revenue ($)"),
        showlegend = TRUE,
        legend = list(title = list(text = "Business Category")),
        dragmode = "select" # Enable brush selection for multiple selection support
      ) %>%
      config(displayModeBar = TRUE, modeBarButtonsToRemove = c("lasso2d")) # Show toolbar for brush selection

    return(p)
  })

  output$multi_timeseries <- renderPlotly({
    if (!is.null(registries$multi)) {
      selection <- registries$multi$get_selection()
      if (!is.null(selection$selected_id)) {
        data <- business_data()
        selected_business <- data[data$business_id == selection$selected_id, ]

        if (nrow(selected_business) > 0) {
          # Extract monthly data
          monthly_cols <- paste0("month_", sprintf("%02d", 1:12))
          monthly_values <- as.numeric(selected_business[1, monthly_cols])

          time_data <- data.frame(
            month = 1:12,
            revenue = monthly_values,
            month_name = month.abb[1:12] # Add month names for better display
          )

          p <- plot_ly(
            data = time_data,
            x = ~month,
            y = ~revenue,
            type = "scatter", # Explicitly specify type
            mode = "lines+markers", # Explicitly specify mode
            line = list(color = "blue", width = 3),
            marker = list(size = 8, color = "blue"),
            text = ~ paste("Month:", month_name, "<br>Revenue: $", format(revenue, big.mark = ",")),
            hovertemplate = "%{text}<extra></extra>"
          ) %>%
            layout(
              title = list(text = paste("Monthly Revenue:", selected_business$name), font = list(size = 14)),
              xaxis = list(
                title = "Month",
                tickmode = "array",
                tickvals = 1:12,
                ticktext = month.abb[1:12]
              ),
              yaxis = list(title = "Revenue ($)"),
              showlegend = FALSE
            ) %>%
            config(displayModeBar = FALSE)

          return(p)
        }
      }
    }

    # Default empty chart
    plot_ly() %>%
      add_annotations(
        text = "Select a business to view time series",
        x = 0.5,
        y = 0.5,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, color = "gray")
      ) %>%
      layout(
        title = "Monthly Revenue Trend",
        xaxis = list(title = "Month", showgrid = FALSE, showticklabels = FALSE),
        yaxis = list(title = "Revenue", showgrid = FALSE, showticklabels = FALSE)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # Initialize multi-component linking
  observeEvent(input$main_tabs, {
    if (input$main_tabs == "multi" && is.null(registries$multi)) {
      registries$multi <- linkeR::link_plots(
        session,
        multi_map = business_data,
        multi_table = business_data,
        shared_id_column = "business_id"
      )

      # Register plotly component for better integration and multiple selection support
      # This replaces the manual event handling with automatic registry integration
      linkeR::register_plotly(
        session = session,
        registry = registries$multi,
        plotly_output_id = "multi_chart",
        data_reactive = business_data,
        shared_id_column = "business_id",
        source_id = "multi_chart"
      )
      
      # Note: The manual observeEvent for plotly_click is no longer needed
      # The register_plotly function handles both click and brush selections automatically
    }
  })

  # Multi selection display
  output$multi_selection <- renderText({
    if (!is.null(registries$multi)) {
      selection <- registries$multi$get_selection()
      if (length(selection$selected_ids) > 0) {
        data <- business_data()
        selected <- data[data$business_id %in% selection$selected_ids, ]
        if (nrow(selected) > 0) {
          if (nrow(selected) == 1) {
            # Single selection
            return(paste0(
              "Selected: ", selected$name, "\n",
              "Source: ", selection$source, "\n",
              "Category: ", selected$category
            ))
          } else {
            # Multiple selections
            return(paste0(
              "Selected: ", nrow(selected), " businesses\n",
              "Source: ", selection$source, "\n",
              "Categories: ", paste(unique(selected$category), collapse = ", "), "\n",
              "Total Revenue: $", format(sum(selected$annual_revenue), big.mark = ",")
            ))
          }
        }
      }
    }
    return("No selection")
  })

  # Clear multi selection
  observeEvent(input$clear_multi_selection, {
    if (!is.null(registries$multi)) {
      registries$multi$set_multiple_selection(character(0), "manual_clear")
    }
  })

  # Clean up all registries on session end
  session$onSessionEnded(function() {
    for (registry_name in names(registries)) {
      registry <- registries[[registry_name]]
      if (!is.null(registry)) {
        registry$clear_all()
      }
    }
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
