library(shiny)
library(leaflet)
library(DT)
library(linkeR)

# Sample data
sample_data <- data.frame(
  id = 1:10,
  name = paste("Location", 1:10),
  longitude = runif(10, -180, 180),
  latitude = runif(10, -90, 90),
  category = sample(c("Type A", "Type B", "Type C"), 10, replace = TRUE),
  value = round(runif(10, 1, 100)),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("linkeR Example: Linked Map and Table"),
  fluidRow(
    column(
      6,
      h3("Interactive Map"),
      leafletOutput("map", height = "400px")
    ),
    column(
      6,
      h3("Data Table"),
      DT::DTOutput("table")
    )
  ),
  fluidRow(
    column(
      12,
      h3("Instructions"),
      p("• Click on a marker in the map to select the corresponding row in the table"),
      p("• Click on a row in the table to highlight the corresponding marker on the map"),
      p("• The map will automatically zoom to the selected location")
    )
  )
)

server <- function(input, output, session) {
  # Reactive data
  map_data <- reactive({
    sample_data
  })

  table_data <- reactive({
    sample_data[, c("id", "name", "category", "value")]
  })

  # Create the map
  output$map <- renderLeaflet({
    leaflet(sample_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~id, # This is crucial for linking!
        popup = ~ paste0(
          "<b>", name, "</b><br>",
          "Category: ", category, "<br>",
          "Value: ", value
        )
      )
  })

  # Create the table
  output$table <- DT::renderDT({
    DT::datatable(
      table_data(),
      selection = "single", # Important for linking
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # Set up linking using the simple API
  linkeR::link_plots(
    session,
    map = map_data,
    table = table_data,
    shared_id_column = "id"
  )
}

# Run the app
if (interactive()) {
  shinyApp(ui = ui, server = server)
}
