library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(linkeR)
library(bslib)

# Generate sample data for multiple selection demo
generate_sample_data <- function() {
  set.seed(123) # For reproducible data
  
  n_items <- 20
  
  # Create sample data with geographic and performance metrics
  sample_data <- data.frame(
    item_id = paste0("ITEM_", sprintf("%03d", 1:n_items)),
    name = paste("Item", 1:n_items),
    category = sample(c("Type A", "Type B", "Type C"), n_items, replace = TRUE),
    
    # Location data (clustered around different regions)
    latitude = c(
      runif(7, 40.70, 40.75),   # Cluster 1
      runif(7, 40.76, 40.81),   # Cluster 2
      runif(6, 40.82, 40.87)    # Cluster 3
    ),
    longitude = c(
      runif(7, -111.95, -111.90), # Cluster 1
      runif(7, -111.89, -111.84), # Cluster 2
      runif(6, -111.83, -111.78)  # Cluster 3
    ),
    
    # Performance metrics
    score = round(runif(n_items, 50, 100), 1),
    value = round(runif(n_items, 1000, 10000), -2),
    priority = sample(c("Low", "Medium", "High"), n_items, replace = TRUE),
    
    stringsAsFactors = FALSE
  )
  
  return(sample_data)
}

# UI
ui <- page_fillable(
  title = "Multiple Selection Demo - linkeR Package",
  
  # Custom CSS for better styling
  tags$head(
    tags$style(HTML("
      .selection-info {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 0.375rem;
        padding: 1rem;
        margin-bottom: 1rem;
      }
      .selection-count {
        font-weight: bold;
        color: #0d6efd;
      }
      .alert-info {
        background-color: #e7f3ff;
        border-color: #b8daff;
        color: #004085;
      }
    "))
  ),
  
  # Header
  div(
    class = "alert alert-info",
    h3("Multiple Selection Demo"),
    p("This demo showcases multiple selection support across DT tables, Leaflet maps, and Plotly charts. Try:"),
    tags$ul(
      tags$li("Select multiple rows in the table (using Ctrl+click or Shift+click)"),
      tags$li("Use brush selection in the plotly chart (drag to select multiple points)"),
      tags$li("Click individual markers on the map (leaflet supports single selection, but responds to multiple selections from other components)")
    )
  ),
  
  # Main content
  fluidRow(
    # Left column - Table and Controls
    column(4,
      h4("Data Table"),
      p("Supports multiple row selection"),
      DTOutput("data_table"),
      br(),
      h5("Selection Controls"),
      div(
        class = "selection-info",
        textOutput("selection_info"),
        br(),
        actionButton("clear_selection", "Clear All Selections", class = "btn-warning btn-sm"),
        br(), br(),
        actionButton("select_high_score", "Select High Scores (>80)", class = "btn-info btn-sm"),
        br(), br(),
        actionButton("select_by_category", "Select Type A Items", class = "btn-info btn-sm")
      )
    ),
    
    # Middle column - Map
    column(4,
      h4("Geographic View"),
      p("Responds to multiple selections from other components"),
      leafletOutput("data_map", height = "400px"),
      br(),
      h5("Map Info"),
      div(
        class = "selection-info",
        verbatimTextOutput("map_info")
      )
    ),
    
    # Right column - Chart
    column(4,
      h4("Performance Chart"),
      p("Supports brush selection (drag to select multiple points)"),
      plotlyOutput("data_chart", height = "400px"),
      br(),
      h5("Chart Info"),
      div(
        class = "selection-info",
        verbatimTextOutput("chart_info")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Generate reactive data
  sample_data <- reactive({
    generate_sample_data()
  })
  
  # Create registry for linking components
  registry <- linkeR::create_link_registry(
    session = session,
    on_selection_change = function(selected_id, selected_data, source_id, session) {
      # This callback is called whenever selection changes
      # You can add custom logic here for additional feedback
    }
  )
  
  # Register components with multiple selection support
  observeEvent(sample_data(), {
    # Register DT table
    linkeR::register_dt(
      session = session,
      registry = registry,
      dt_output_id = "data_table",
      data_reactive = sample_data,
      shared_id_column = "item_id"
    )
    
    # Register Leaflet map
    linkeR::register_leaflet(
      session = session,
      registry = registry,
      leaflet_output_id = "data_map",
      data_reactive = sample_data,
      shared_id_column = "item_id"
    )
    
    # Register Plotly chart
    linkeR::register_plotly(
      session = session,
      registry = registry,
      plotly_output_id = "data_chart",
      data_reactive = sample_data,
      shared_id_column = "item_id",
      source_id = "demo_chart"
    )
  }, once = TRUE)
  
  # Render DT table with multiple selection enabled
  output$data_table <- renderDT({
    data <- sample_data()
    
    datatable(
      data[, c("name", "category", "score", "value", "priority")],
      selection = "multiple",  # Enable multiple selection
      rownames = FALSE,
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        dom = 'tp' # Remove search box for cleaner look
      )
    ) %>%
      formatCurrency("value", currency = "$", digits = 0) %>%
      formatStyle("priority",
        backgroundColor = styleEqual(c("Low", "Medium", "High"), 
                                   c("#d4edda", "#fff3cd", "#f8d7da"))
      ) %>%
      formatStyle("score",
        backgroundColor = styleInterval(c(70, 85), c("#f8d7da", "#fff3cd", "#d4edda"))
      )
  })
  
  # Render Leaflet map
  output$data_map <- renderLeaflet({
    data <- sample_data()
    
    # Color palette for categories
    category_colors <- c("Type A" = "blue", "Type B" = "green", "Type C" = "red")
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~item_id, # Critical for linking!
        color = ~category_colors[category],
        radius = ~sqrt(score/10) + 3,
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste0(
          "<strong>", name, "</strong><br>",
          "Category: ", category, "<br>",
          "Score: ", score, "<br>",
          "Value: $", format(value, big.mark = ",")
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = category_colors,
        labels = names(category_colors),
        title = "Category"
      )
  })
  
  # Render Plotly chart with brush selection
  output$data_chart <- renderPlotly({
    data <- sample_data()
    
    p <- plot_ly(
      data = data,
      x = ~score,
      y = ~value,
      color = ~category,
      key = ~item_id, # Critical for linking!
      text = ~paste0("Name: ", name, "<br>",
                    "Category: ", category, "<br>",
                    "Score: ", score, "<br>",
                    "Value: $", format(value, big.mark = ",")),
      type = "scatter",
      mode = "markers",
      source = "demo_chart", # Must match the source_id in register_plotly
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Score vs Value", font = list(size = 14)),
        xaxis = list(title = "Score"),
        yaxis = list(title = "Value ($)"),
        showlegend = TRUE,
        dragmode = "select" # Enable brush selection
      ) %>%
      config(displayModeBar = TRUE, modeBarButtonsToRemove = c("lasso2d"))
    
    return(p)
  })
  
  # Display selection information
  output$selection_info <- renderText({
    selection <- registry$get_selection()
    
    if (length(selection$selected_ids) > 0) {
      paste0(
        "Selected: ", length(selection$selected_ids), " item(s)\n",
        "Source: ", selection$source, "\n",
        "IDs: ", paste(selection$selected_ids, collapse = ", ")
      )
    } else {
      "No items selected"
    }
  })
  
  # Map information
  output$map_info <- renderText({
    selection <- registry$get_selection()
    
    if (length(selection$selected_ids) > 0) {
      data <- sample_data()
      selected_items <- data[data$item_id %in% selection$selected_ids, ]
      
      paste0(
        "Showing ", nrow(selected_items), " selected item(s) on map\n",
        "Categories: ", paste(unique(selected_items$category), collapse = ", "), "\n",
        "Score range: ", min(selected_items$score), " - ", max(selected_items$score)
      )
    } else {
      "No items selected - showing all items"
    }
  })
  
  # Chart information
  output$chart_info <- renderText({
    selection <- registry$get_selection()
    
    if (length(selection$selected_ids) > 0) {
      data <- sample_data()
      selected_items <- data[data$item_id %in% selection$selected_ids, ]
      
      paste0(
        "Selected ", nrow(selected_items), " point(s) in chart\n",
        "Avg Score: ", round(mean(selected_items$score), 1), "\n",
        "Total Value: $", format(sum(selected_items$value), big.mark = ",")
      )
    } else {
      "No points selected - use brush tool to select multiple points"
    }
  })
  
  # Control buttons
  observeEvent(input$clear_selection, {
    registry$set_multiple_selection(character(0), "manual_clear")
  })
  
  observeEvent(input$select_high_score, {
    data <- sample_data()
    high_score_ids <- data$item_id[data$score > 80]
    registry$set_multiple_selection(high_score_ids, "manual_filter")
  })
  
  observeEvent(input$select_by_category, {
    data <- sample_data()
    type_a_ids <- data$item_id[data$category == "Type A"]
    registry$set_multiple_selection(type_a_ids, "manual_filter")
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
}