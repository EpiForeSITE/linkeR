library(shiny)
library(plotly)
library(DT)
library(linkeR)

# Sample business data with multiple dimensions for different plots
set.seed(123)
business_data <- data.frame(
  business_id = 1:25,
  name = paste("Business", LETTERS[1:25]),
  category = sample(c("Retail", "Tech", "Food", "Service", "Manufacturing"), 25, replace = TRUE),
  revenue = round(runif(25, 50000, 500000), 0),
  employees = sample(5:200, 25, replace = TRUE),
  satisfaction_score = round(runif(25, 3.0, 5.0), 1),
  years_operating = sample(1:25, 25, replace = TRUE),
  growth_rate = round(runif(25, -10, 25), 1),
  region = sample(c("North", "South", "East", "West", "Central"), 25, replace = TRUE)
)

ui <- fluidPage(
  titlePanel("Multiple Plotly Charts Linked Example"),
  
  div(class = "alert alert-info",
      p("This example shows three different plotly charts and a data table all linked together. ",
        "Click on any point in any chart or row in the table to see the selection propagate across all components.")
  ),
  
  fluidRow(
    column(6,
           h4("Revenue vs Employees"),
           plotlyOutput("revenue_plot", height = "300px")
    ),
    column(6,
           h4("Years Operating vs Growth Rate"),
           plotlyOutput("growth_plot", height = "300px")
    )
  ),
  
  fluidRow(
    column(6,
           h4("Satisfaction Score by Category"),
           plotlyOutput("satisfaction_plot", height = "300px")
    ),
    column(6,
           h4("Business Data Table"),
           DT::dataTableOutput("business_table", height = "300px")
    )
  ),
  
  fluidRow(
    column(12,
           h4("Selection Information"),
           verbatimTextOutput("selection_info"),
           br(),
           div(class = "well",
               h5("How This Works:"),
               p("All components share the same ", code("business_id"), " column as their linking key."),
               p("Each plotly chart uses ", code("key = ~business_id"), " to pass the ID to click events."),
               p("When you click any point or row, all other components highlight the corresponding item.")
           )
    )
  )
)

server <- function(input, output, session) {
  # Create registry
  registry <- create_link_registry(session)
  
  # Reactive data
  reactive_data <- reactive({
    business_data
  })
  
  # Register all components with the same shared ID
  register_plotly(session, registry, "revenue_plot", reactive_data, "business_id", source = "revenue_source")
  register_plotly(session, registry, "growth_plot", reactive_data, "business_id", source = "growth_source")
  register_plotly(session, registry, "satisfaction_plot", reactive_data, "business_id", source = "satisfaction_source")
  register_dt(session, registry, "business_table", reactive_data, "business_id")
  
  # Revenue vs Employees scatter plot
  output$revenue_plot <- renderPlotly({
    plot_data <- reactive_data()
    
    plot_ly(
      data = plot_data,
      x = ~employees,
      y = ~revenue,
      text = ~paste("Business:", name, "<br>ID:", business_id, "<br>Category:", category, 
                   "<br>Employees:", employees, "<br>Revenue: $", scales::comma(revenue)),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      key = ~business_id,  # Critical for linking
      marker = list(
        size = 10,
        color = ~as.numeric(as.factor(category)),  # Color by category without creating traces
        colorscale = "Set1",
        showscale = FALSE
      ),
      source = "revenue_source"
    ) %>%
      layout(
        title = "Revenue vs Employees (colored by category)",
        xaxis = list(title = "Number of Employees"),
        yaxis = list(title = "Revenue ($)", tickformat = "$,"),
        showlegend = FALSE
      )
  })
  
  # Years Operating vs Growth Rate scatter plot
  output$growth_plot <- renderPlotly({
    plot_data <- reactive_data()
    
    plot_ly(
      data = plot_data,
      x = ~years_operating,
      y = ~growth_rate,
      text = ~paste("Business:", name, "<br>ID:", business_id, "<br>Region:", region,
                   "<br>Years Operating:", years_operating, "<br>Growth Rate:", growth_rate, "%"),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      key = ~business_id,  # Critical for linking
      marker = list(
        size = 10,
        color = ~as.numeric(as.factor(region)),  # Color by region without creating traces
        colorscale = "Set2",
        showscale = FALSE
      ),
      source = "growth_source"
    ) %>%
      layout(
        title = "Business Age vs Growth Rate (colored by region)",
        xaxis = list(title = "Years Operating"),
        yaxis = list(title = "Growth Rate (%)", ticksuffix = "%"),
        showlegend = FALSE
      )
  })
  
  # Satisfaction Score by Category bar/scatter plot
  output$satisfaction_plot <- renderPlotly({
    plot_data <- reactive_data()
    
    plot_ly(
      data = plot_data,
      x = ~category,
      y = ~satisfaction_score,
      text = ~paste("Business:", name, "<br>ID:", business_id, "<br>Category:", category,
                   "<br>Satisfaction:", satisfaction_score, "/5.0"),
      hoverinfo = "text",
      mode = "markers",
      type = "scatter",
      key = ~business_id,  # Critical for linking - use key instead of customdata
      marker = list(
        size = 12,
        color = ~satisfaction_score,
        colorscale = "Viridis",
        showscale = TRUE,
        colorbar = list(title = "Satisfaction")
      ),
      source = "satisfaction_source"
    ) %>%
      layout(
        title = "Customer Satisfaction by Category",
        xaxis = list(title = "Business Category"),
        yaxis = list(title = "Satisfaction Score", range = c(2.5, 5.5)),
        showlegend = FALSE
      )
  })
  
  # Business data table
  output$business_table <- DT::renderDataTable({
    DT::datatable(
      reactive_data() %>%
        dplyr::select(business_id, name, category, revenue, employees, satisfaction_score, growth_rate) %>%
        dplyr::mutate(revenue = scales::dollar(revenue)),
      selection = "single",
      options = list(
        pageLength = 8,
        autoWidth = TRUE,
        scrollY = "250px",
        scrollX = TRUE
      ),
      colnames = c("ID", "Business Name", "Category", "Revenue", "Employees", "Satisfaction", "Growth %")
    )
  })
  
  # Selection information
  output$selection_info <- renderText({
    selected_id <- registry$get_selected_id()
    source <- registry$get_selection_source()
    
    if (is.null(selected_id)) {
      return("No selection made. Click on any chart point or table row to see linking in action.")
    }
    
    # Get the selected business details
    selected_business <- reactive_data()[reactive_data()$business_id == selected_id, ]
    
    paste(
      paste("Selected Business ID:", selected_id),
      paste("Selection Source:", source),
      paste("Business Name:", selected_business$name),
      paste("Category:", selected_business$category),
      paste("Revenue:", scales::dollar(selected_business$revenue)),
      paste("Employees:", selected_business$employees),
      paste("Satisfaction Score:", selected_business$satisfaction_score, "/5.0"),
      paste("Growth Rate:", selected_business$growth_rate, "%"),
      paste("Years Operating:", selected_business$years_operating),
      paste("Region:", selected_business$region),
      "",
      "All linked components should now highlight this business!",
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)