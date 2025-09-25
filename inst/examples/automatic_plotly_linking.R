library(shiny)
library(plotly)
library(DT)
library(linkeR)

# Test data
auto_data <- data.frame(
  auto_id = 1:12,
  name = paste("Item", LETTERS[1:12]),
  x_numeric = 1:12,
  y_numeric = runif(12, 1, 10),
  category = rep(c("Red", "Blue", "Green"), 4),
  score = round(runif(12, 50, 100), 1)
)

ui <- fluidPage(
  titlePanel("Automatic Plotly Linking (No Manual Setup Required)"),
  
  div(class = "alert alert-success",
      p("🎯 These plots require NO manual customdata/key setup!"),
      p("✨ linkeR automatically handles linking for any plot configuration.")
  ),
  
  fluidRow(
    column(4,
           h4("Plot 1: Color Grouping"),
           p("Traditional problematic case"),
           plotlyOutput("auto_plot1", height = "300px")
    ),
    column(4,
           h4("Plot 2: Categorical X-axis"),
           p("Categories vs numeric"),
           plotlyOutput("auto_plot2", height = "300px")
    ),
    column(4,
           h4("Plot 3: Complex Plot"),
           p("Size + color mapping"),
           plotlyOutput("auto_plot3", height = "300px")
    )
  ),
  
  fluidRow(
    column(6,
           h4("Data Table"),
           DT::dataTableOutput("auto_table", height = "250px")
    ),
    column(6,
           h4("Automatic Linking Status"),
           verbatimTextOutput("auto_info"),
           br(),
           div(class = "well",
               h5("Zero Configuration Required:"),
               tags$ul(
                 tags$li("No customdata parameter needed"),
                 tags$li("No key parameter needed"),
                 tags$li("No manual plot preparation"),
                 tags$li("Works with any plot structure")
               )
           )
    )
  )
)

server <- function(input, output, session) {
  registry <- create_link_registry(session)
  reactive_data <- reactive({ auto_data })
  
  # Simple registration - no plot modification needed!
  register_plotly(session, registry, "auto_plot1", reactive_data, "auto_id", source = "auto1")
  register_plotly(session, registry, "auto_plot2", reactive_data, "auto_id", source = "auto2")  
  register_plotly(session, registry, "auto_plot3", reactive_data, "auto_id", source = "auto3")
  register_dt(session, registry, "auto_table", reactive_data, "auto_id")
  
  # Plot 1: Standard scatter with color grouping (previously problematic)
  output$auto_plot1 <- renderPlotly({
    plot_ly(
      data = reactive_data(),
      x = ~x_numeric,
      y = ~y_numeric,
      color = ~category,  # Creates multiple traces - should work automatically!
      text = ~paste("ID:", auto_id, "<br>Name:", name),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      source = "auto1"
      # Notice: NO customdata or key needed!
    )
  })
  
  # Plot 2: Categorical x-axis (tests smart coordinate lookup)
  output$auto_plot2 <- renderPlotly({
    plot_ly(
      data = reactive_data(),
      x = ~category,  # Categorical x-axis
      y = ~score,
      text = ~paste("ID:", auto_id, "<br>Name:", name, "<br>Score:", score),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      marker = list(size = 12),
      source = "auto2"
      # Notice: NO special parameters needed!
    )
  })
  
  # Plot 3: Complex plot with size mapping
  output$auto_plot3 <- renderPlotly({
    plot_ly(
      data = reactive_data(),
      x = ~score,
      y = ~y_numeric,
      size = ~x_numeric,
      color = ~category,
      text = ~paste("ID:", auto_id, "<br>Name:", name),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      source = "auto3"
      # Notice: Even complex plots work automatically!
    )
  })
  
  # Standard data table
  output$auto_table <- DT::renderDataTable({
    DT::datatable(
      reactive_data(),
      selection = "single",
      options = list(pageLength = 6)
    )
  })
  
  # Show automatic linking status
  output$auto_info <- renderText({
    selected_id <- registry$get_selected_id()
    source <- registry$get_selection_source()
    
    if (is.null(selected_id)) {
      return("✨ Ready for automatic linking!\n\nClick any point or table row.\n\n🔍 Check console for debug info about how linkeR automatically detects the clicked item.")
    }
    
    selected_item <- reactive_data()[reactive_data()$auto_id == selected_id, ]
    
    paste(
      "🎯 AUTOMATIC LINKING SUCCESS!",
      "",
      paste("Selected ID:", selected_id),
      paste("Triggered from:", source),
      paste("Item:", selected_item$name),
      paste("Category:", selected_item$category),
      paste("Score:", selected_item$score),
      "",
      "✅ No manual setup required!",
      "✅ Works with any plot structure!",
      "✅ Smart coordinate detection!",
      "",
      "Check browser console for debug details.",
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)