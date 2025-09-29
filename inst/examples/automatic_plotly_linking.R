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
  titlePanel("Plotly Linking: Simple Default vs Custom Behavior"),
  
  div(class = "alert alert-info",
      p("🎯 Compare simple default behavior vs advanced custom callbacks!"),
      p("✨ Default behavior uses plotly's built-in selectedpoints highlighting."),
      p("🚀 Custom behavior allows full control over visual updates.")
  ),
  
  fluidRow(
    column(6,
           h4("Default Behavior: Simple & Reliable"),
           p("Uses plotly's built-in selectedpoints highlighting"),
           plotlyOutput("default_plot", height = "300px"),
           div(class = "well well-sm",
               p(strong("Benefits:"), "Simple, works everywhere, no custom code needed"))
    ),
    column(6,
           h4("Custom Behavior: Advanced Control"),
           p("Custom callback with enhanced visual feedback"),
           plotlyOutput("custom_plot", height = "300px"),
           div(class = "well well-sm",
               p(strong("Benefits:"), "Full control, custom styling, advanced features"))
    )
  ),
  
  fluidRow(
    column(4,
           h4("Data Table"),
           DT::dataTableOutput("auto_table", height = "250px")
    ),
    column(4,
           h4("Linking Status"),
           verbatimTextOutput("auto_info")
    ),
    column(4,
           h4("Implementation Comparison"),
           div(class = "well",
               h5("Default (Simple):"),
               tags$ul(
                 tags$li("Just call register_plotly()"),
                 tags$li("Uses selectedpoints highlighting"),
                 tags$li("Works with any plot type")
               ),
               h5("Custom (Advanced):"),
               tags$ul(
                 tags$li("Define plotly_click_handler function"),
                 tags$li("Full control over visual updates"),
                 tags$li("Add animations, colors, etc.")
               )
           )
    )
  )
)

server <- function(input, output, session) {
  registry <- create_link_registry(session)
  reactive_data <- reactive({ auto_data })
  
  # DEFAULT BEHAVIOR: Simple registration with built-in selectedpoints highlighting
  register_plotly(session, registry, "default_plot", reactive_data, "auto_id", source = "default")
  
  # CUSTOM BEHAVIOR: Advanced callback with custom visual feedback
  custom_highlight_handler <- function(plot_proxy, selected_data, session) {
    if (!is.null(selected_data)) {
      # Custom highlighting: Add a pulsing red circle
      selected_id <- selected_data[["auto_id"]]
      current_data <- reactive_data()
      point_idx <- which(current_data$auto_id == selected_id)[1] - 1  # 0-indexed for plotly
      
      if (!is.na(point_idx)) {
        # Clear any existing custom traces
        if (isTRUE(session$userData[["custom_highlight_active"]])) {
          plotly::plotlyProxyInvoke(plot_proxy, "deleteTraces", list(-1))
        }
        
        # Add custom highlight trace with animation
        selected_row <- current_data[point_idx + 1, ]
        plotly::plotlyProxyInvoke(
          plot_proxy,
          "addTraces",
          list(
            x = list(selected_row$x_numeric),
            y = list(selected_row$score),  # Use score, not y_numeric!
            mode = "markers",
            marker = list(
              size = 25,
              color = "rgba(255, 0, 0, 0.6)",
              symbol = "circle-open",
              line = list(width = 5, color = "red")
            ),
            showlegend = FALSE,
            hoverinfo = "skip",
            name = "custom_highlight"
          )
        )
        session$userData[["custom_highlight_active"]] <- TRUE
        
        # Show message
        showNotification(
          paste("Custom highlight for:", selected_row$name), 
          type = "message", 
          duration = 2
        )
      }
    } else {
      # Clear custom highlighting
      if (isTRUE(session$userData[["custom_highlight_active"]])) {
        plotly::plotlyProxyInvoke(plot_proxy, "deleteTraces", list(-1))
        session$userData[["custom_highlight_active"]] <- FALSE
      }
    }
  }
  
  register_plotly(
    session, registry, "custom_plot", reactive_data, "auto_id", 
    source = "custom",
    click_handler = custom_highlight_handler
  )
  
  register_dt(session, registry, "auto_table", reactive_data, "auto_id")
  
  # DEFAULT PLOT: Uses simple built-in selectedpoints highlighting
  output$default_plot <- renderPlotly({
    plot_ly(
      data = reactive_data(),
      x = ~x_numeric,
      y = ~y_numeric,
      color = ~category,
      text = ~paste("ID:", auto_id, "<br>Name:", name, "<br>Category:", category),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      source = "default"
      # No special parameters needed - linkeR handles everything automatically!
    )
  })
  
  # CUSTOM PLOT: Uses advanced custom callback for enhanced visuals
  output$custom_plot <- renderPlotly({
    plot_ly(
      data = reactive_data(),
      x = ~x_numeric,
      y = ~score,
      color = ~category,
      text = ~paste("ID:", auto_id, "<br>Name:", name, "<br>Score:", score),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      marker = list(size = 10),
      source = "custom"
      # Same simple plot - the magic is in the custom callback!
    )
  })
  
  # Data table
  output$auto_table <- DT::renderDataTable({
    DT::datatable(
      reactive_data(),
      selection = "single",
      options = list(pageLength = 6)
    )
  })
  
  # Show linking status and behavior comparison
  output$auto_info <- renderText({
    selection <- registry$get_selection()
    selected_id <- selection$selected_id
    source <- selection$source
    
    if (is.null(selected_id)) {
      return(paste(
        "Click any plot point or table row to see:",
        "",
        "🔹 DEFAULT: Simple selectedpoints highlighting",
        "🔹 CUSTOM: Enhanced visual feedback + notification",
        "",
        "Both require zero plot configuration!",
        sep = "\n"
      ))
    }
    
    selected_item <- reactive_data()[reactive_data()$auto_id == selected_id, ]
    behavior_type <- if(source == "default") "DEFAULT (selectedpoints)" else if(source == "custom") "CUSTOM (callback)" else "TABLE"
    
    paste(
      paste("🎯 Selected:", selected_item$name),
      paste("📍 Source:", behavior_type),
      paste("🏷️ Category:", selected_item$category),
      paste("📊 Score:", selected_item$score),
      "",
      if(source == "default") "Using plotly's built-in selectedpoints" else if(source == "custom") "Using custom callback with red highlight" else "Table selection triggers both plots",
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)