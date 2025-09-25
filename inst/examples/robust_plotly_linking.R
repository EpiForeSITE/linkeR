library(shiny)
library(plotly)
library(DT)
library(linkeR)

# Simple test data
test_data <- data.frame(
  item_id = 1:15,
  name = paste("Item", LETTERS[1:15]),
  x_value = 1:15,
  y_value = runif(15, 1, 10),
  category = rep(c("Type1", "Type2", "Type3"), 5),
  score = round(runif(15, 50, 100), 1)
)

ui <- fluidPage(
  titlePanel("Robust Plotly Linking Demo"),
  
  div(class = "alert alert-info",
      p("This demonstrates robust plotly linking that works with any plot configuration."),
      p("All plots should link correctly regardless of whether they use color grouping or not.")
  ),
  
  fluidRow(
    column(4,
           h4("Plot 1: With Color Grouping"),
           p("Uses color = ~category (multiple traces)"),
           plotlyOutput("plot1", height = "300px")
    ),
    column(4,
           h4("Plot 2: Without Color Grouping"),
           p("Single trace with marker colors"),
           plotlyOutput("plot2", height = "300px")
    ),
    column(4,
           h4("Plot 3: Complex Plot"),
           p("Size + color + grouping"),
           plotlyOutput("plot3", height = "300px")
    )
  ),
  
  fluidRow(
    column(6,
           h4("Data Table"),
           DT::dataTableOutput("data_table", height = "250px")
    ),
    column(6,
           h4("Selection Info"),
           verbatimTextOutput("selection_info"),
           br(),
           div(class = "well",
               p("✅ All plots use automatic linking preparation"),
               p("✅ Robust ID extraction handles any plot structure"),
               p("✅ Clear error messages for debugging")
           )
    )
  )
)

server <- function(input, output, session) {
  # Create registry
  registry <- create_link_registry(session)
  
  reactive_data <- reactive({ test_data })
  
  # Register all components - notice the helpful messages
  register_plotly(session, registry, "plot1", reactive_data, "item_id", source = "plot1_source")
  register_plotly(session, registry, "plot2", reactive_data, "item_id", source = "plot2_source")  
  register_plotly(session, registry, "plot3", reactive_data, "item_id", source = "plot3_source")
  register_dt(session, registry, "data_table", reactive_data, "item_id")
  
  # Plot 1: Traditional problematic approach (color grouping)
  output$plot1 <- renderPlotly({
    p <- plot_ly(
      data = reactive_data(),
      x = ~x_value,
      y = ~y_value,
      color = ~category,  # This creates multiple traces
      text = ~paste("ID:", item_id, "<br>Name:", name),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      source = "plot1_source"
    )
    
    # Use the new utility function to add linking parameters
    prepare_plotly_linking(p, "item_id", "plot1_source")
  })
  
  # Plot 2: Single trace approach
  output$plot2 <- renderPlotly({
    p <- plot_ly(
      data = reactive_data(),
      x = ~x_value,
      y = ~score,
      text = ~paste("ID:", item_id, "<br>Name:", name, "<br>Score:", score),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      marker = list(
        size = 10,
        color = ~score,
        colorscale = "Viridis",
        showscale = TRUE
      ),
      source = "plot2_source"
    )
    
    # This also works with the utility function
    prepare_plotly_linking(p, "item_id", "plot2_source")
  })
  
  # Plot 3: Complex plot with size, color, and grouping
  output$plot3 <- renderPlotly({
    p <- plot_ly(
      data = reactive_data(),
      x = ~score,
      y = ~y_value,
      size = ~x_value,
      color = ~category,  # Multiple traces again
      text = ~paste("ID:", item_id, "<br>Name:", name),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      source = "plot3_source"
    )
    
    # Robust linking handles this complex case too
    prepare_plotly_linking(p, "item_id", "plot3_source")
  })
  
  # Data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(
      reactive_data(),
      selection = "single",
      options = list(
        pageLength = 6,
        scrollY = "200px"
      )
    )
  })
  
  # Selection information with debugging
  output$selection_info <- renderText({
    selected_id <- registry$get_selected_id()
    source <- registry$get_selection_source()
    
    if (is.null(selected_id)) {
      return("No selection. Click any point in any plot or any table row.\n\nAll plots should link correctly!")
    }
    
    selected_item <- reactive_data()[reactive_data()$item_id == selected_id, ]
    
    paste(
      paste("Selected ID:", selected_id),
      paste("Source:", source),
      paste("Name:", selected_item$name),
      paste("Category:", selected_item$category),
      paste("Score:", selected_item$score),
      "",
      "✅ Robust linking working!",
      "Check console for debug info.",
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)