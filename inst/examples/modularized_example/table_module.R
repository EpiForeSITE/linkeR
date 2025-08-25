# /path/to/your/app/table_module.R

#' Table Module UI
#'
#' @param id A character string. The namespace ID.
#' @return A UI definition.
tableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("wastewater_table"))
}

#' Table Module Server
#'
#' @param id A character string. The namespace ID.
#' @param data A reactive expression returning the data frame for the table.
#' @param registry A link_registry object for managing component linking.
tableServer <- function(id, data, registry) {
  moduleServer(id, function(input, output, session) {
    # Register this component with the central registry
    register_dt(
      session = session, # <-- pass the module's session
      registry = registry,
      dt_output_id = "wastewater_table", # <-- the local ID
      data_reactive = data,
      shared_id_column = "id",
      click_handler = function(map_proxy, selected_data, session) { # <-- click handler must have map_proxy, selected_data, session, overrides all default behavior
        print("The DT table component was just clicked!")
      }
    )

    output$wastewater_table <- renderDT({
      table_data <- data()[, c("facility_name", "city", "risk_level", 
                               "covid_copies_per_ml", "population_served", 
                               "last_sample_date")]
      
      datatable(
        table_data,
        selection = "single",
        rownames = FALSE,
        colnames = c("Facility", "City", "Risk", "COVID Copies/mL", 
                     "Population", "Last Sample"),
        options = list(
          pageLength = 8,
          scrollX = TRUE,
          order = list(list(3, 'desc'))
        )
      ) %>%
        formatCurrency(c("covid_copies_per_ml", "population_served"), currency = "", digits = 0) %>%
        formatStyle("risk_level",
                    backgroundColor = styleEqual(c("Low", "Medium", "High"), 
                                                 c("lightgreen", "orange", "lightcoral"))
        )
    })
  })
}
