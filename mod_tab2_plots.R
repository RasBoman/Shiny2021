## Module for the second tab of Velmu-app, plots
library(janitor)

tab2_UI_plots <- function(id) {
  ns <- NS(id)
  
  fluidRow(
   # column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(12, plotOutput(ns("plot_hist")))
  )
}


tab2_Server <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactive df from the 
    df_plots <- reactive({ogdf() %>%
        dplyr::filter(kohteen.taso == 63) %>%
        dplyr::select(any_of(var_boxplotit)) %>%
        janitor::remove_empty("cols")
      })
  })
}
