# Table module - mimics user's summary_table_module.R structure
# Tests linkeR observer firing in module context

table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DTOutput(ns("facilityTable")),
    hr(),
    verbatimTextOutput(ns("module_debug"))
  )
}

table_server <- function(id, table_data, registry) {
  moduleServer(id, function(input, output, session) {
    cat("\n[TABLE MODULE] Starting for id:", id, "\n")
    cat("[TABLE MODULE] Session namespace:", session$ns(""), "\n")
    
    # Manual click counter for comparison
    manual_clicks <- reactiveVal(0)
    linker_clicks <- reactiveVal(0)
    
    # Prepare data for DT (remove geometry)
    table_data_df <- reactive({
      data <- table_data
      if (inherits(data, "sf")) {
        data <- st_drop_geometry(data)
      }
      data
    })
    
    # Register DT component with linkeR (BEFORE rendering)
    cat("[TABLE MODULE] Registering DT component 'facilityTable'...\n")
    
    linkeR::register_dt(
      session = session,
      registry = registry,
      dt_output_id = "facilityTable",
      data_reactive = table_data_df,
      shared_id_column = "msd_name"
    )
    
    cat("[TABLE MODULE] DT component registered successfully\n")
    
    # Render the table (AFTER registration)
    output$facilityTable <- renderDT({
      cat("[TABLE MODULE] Rendering DT table...\n")
      
      df <- table_data_df()
      
      datatable(
        df,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    # Manual observer for comparison (mimics user's workaround)
    observeEvent(input$facilityTable_rows_selected, {
      cat("[TABLE MODULE - MANUAL] Manual observer fired!\n")
      manual_clicks(manual_clicks() + 1)
      
      selected_row <- input$facilityTable_rows_selected
      if (!is.null(selected_row) && length(selected_row) > 0) {
        df <- table_data_df()
        if (selected_row <= nrow(df)) {
          clicked_id <- df$msd_name[selected_row]
          cat("[TABLE MODULE - MANUAL] Selected ID:", clicked_id, "\n")
          
          # This demonstrates what the user had to do as a workaround
          # If linkeR is working, this should be redundant
        }
      }
    })
    
    # Debug output
    output$module_debug <- renderPrint({
      cat("Table Module Status:\n")
      cat("  linkeR selections:", linker_clicks(), "\n")
      cat("  Manual selections:", manual_clicks(), "\n")
      cat("  Input name:", session$ns("facilityTable_rows_selected"), "\n")
      cat("  Current selection:", paste(input$facilityTable_rows_selected, collapse = ", "), "\n")
      
      if (linker_clicks() > 0) {
        cat("\nlinkeR observers are WORKING!\n")
      } else if (manual_clicks() > 0) {
        cat("\nManual observers work, but linkeR not firing yet\n")
      } else {
        cat("\n→ Click a table row to test\n")
      }
    })
    
    return(list(
      selected_row = reactive(input$facilityTable_rows_selected)
    ))
  })
}
