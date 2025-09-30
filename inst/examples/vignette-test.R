library(shiny)
library(plotly)
library(linkeR)
library(DT)
library(dplyr)

# Generate sample data
set.seed(123)
categories <- c("Electronics", "Clothing", "Books")
n <- 30
sample_data <- data.frame(
  business_id = paste0("PROD_", sprintf("%03d", 1:n)),
  name = paste("Product", LETTERS[1:n]),
  price = round(runif(n, 10, 100), 2),
  sales = round(runif(n, 100, 1000), 0),
  category = sample(categories, n, replace = TRUE),
  rating = round(runif(n, 1, 5), 1),
  stringsAsFactors = FALSE
)
# Defensive: Remove any rows with NA in key columns
sample_data <- subset(sample_data, !is.na(business_id) & !is.na(name) & !is.na(category))

ui <- fluidPage(
  titlePanel("Complete Plotly + linkeR Example"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('eval', function(code) {
      try {
        eval(code);
      } catch(e) {
        console.error('JavaScript execution error:', e);
      }
    });
  ")),
  fluidRow(
    column(7,
      h4("Scatter Plot"),
      plotlyOutput("scatter_plot", height = "400px"),
      br(),
      verbatimTextOutput("current_selection")
    ),
    column(5,
      h4("Data Table"),
      DTOutput("data_table")
    )
  )
)

server <- function(input, output, session) {
  data_reactive <- reactive({ sample_data })
  # Use a fresh registry name to avoid conflicts
  scatter_registry <- link_plots(
    session,
    scatter_plot = data_reactive,
    data_table = data_reactive,
    shared_id_column = "business_id"
  )
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    plot_ly(
      data = sample_data,
      x = ~price,
      y = ~sales,
      color = ~category,
      key = ~business_id,
      source = "scatter_plot",
      text = ~paste("Product:", name, "<br>Category:", category, "<br>Rating:", rating),
      hovertemplate = "%{text}<br>Price: $%{x:.2f}<br>Sales: %{y:.0f}<extra></extra>",
      type = "scatter",
      mode = "markers"
    ) %>%
      layout(
        title = "Price vs Sales by Category",
        xaxis = list(title = "Price ($)"),
        yaxis = list(title = "Sales")
      )
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(
      sample_data,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searchHighlight = TRUE
      )
    ) %>%
      formatCurrency("price", currency = "$") %>%
      formatRound(c("sales", "rating"), digits = c(0, 1))
  })
  
  # Show current selection
  output$current_selection <- renderText({
    selection <- scatter_registry$get_selection()
    if (!is.null(selection$selected_id)) {
      selected_item <- sample_data[sample_data$business_id == selection$selected_id, ]
      if (nrow(selected_item) > 0) {
        paste0(
          "Selected: ", selected_item$name, "\n",
          "Category: ", selected_item$category, "\n",
          "Price: $", selected_item$price, "\n",
          "Sales: ", selected_item$sales, "\n",
          "Rating: ", selected_item$rating, "\n",
          "Source: ", selection$source
        )
      } else {
        "No item selected"
      }
    } else {
      "No item selected"
    }
  })
}

# Run the application
if (interactive()) {
  shinyApp(ui = ui, server = server)
}