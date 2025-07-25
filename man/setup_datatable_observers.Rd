% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dt.R
\name{setup_datatable_observers}
\alias{setup_datatable_observers}
\title{Setup DataTable Observers}
\usage{
setup_datatable_observers(
  component_id,
  session,
  components,
  shared_state,
  on_selection_change,
  registry = NULL
)
}
\arguments{
\item{component_id}{Character string. Unique identifier for the DataTable component.}

\item{session}{Shiny session object. The current Shiny session for reactive context.}

\item{components}{List. Collection of UI components in the application.}

\item{shared_state}{Reactive values object. Shared state container for cross-component communication.}

\item{on_selection_change}{Function. Callback function to execute when table selection changes.}

\item{registry}{List or NULL. Optional registry for component management. Defaults to NULL.}
}
\value{
NULL. This function is called for its side effects of setting up observers.
}
\description{
Sets up reactive observers for a DataTable component to handle user interactions
and state changes. This function establishes the necessary event handlers for
selection changes and synchronizes the component with the shared application state.
}
\details{
This function creates reactive observers that monitor DataTable interactions and
update the shared state accordingly. It handles selection events and ensures
proper synchronization between the DataTable component and other application components.
}
\examples{
\dontrun{
setup_datatable_observers(
  component_id = "my_table",
  session = session,
  components = ui_components,
  shared_state = app_state,
  on_selection_change = function(selected_rows) {
    # Handle selection change
  }
)
}
}
