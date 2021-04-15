## Module for the second tab of Velmu-app, plots
library(janitor)

var_boxplotit <- c("veden.lampotila",	"lampotilan.mittaussyvyys",	"secchi.syvyys", "tuulen.voimakkuus", 
                   "videon.syvyyden.korjaus", "videon.alkusyvyys", "videon.loppusyvyys",
                   "sukelluslinjan.syvyyden.korjaus", "arviointiruudun.syvyys", "arviointiruudun.etaisyys", 
                   "pohjanlaadut.yhteensa", "lajin.peittavyys", "lajin.lukumaara", "lajin.korkeus")

tab2_UI_plots <- function(id) {
  ns <- NS(id)
  
  fluidRow(
   # column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    uiOutput(ns("graphs_ui")),
    textOutput(ns("teksti")),
    plotOutput(ns("testplot"))
    #column(12, plotOutput(ns("plot_hist")))
  )
}


tab2_Server <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    
    output$teksti <- renderText({Xnames()})
    
    # Create reactive df from the 
    df_plots <- reactive({df_to_use() %>%
        dplyr::filter(kohteen.taso == 63) %>%
        dplyr::select(any_of(var_boxplotit)) %>%
        janitor::remove_empty("cols")
      })
    
    Xnames <- reactive({colnames(df_plots())})
    
    create_hist <- function(variable) {
      df_plots() %>%
        ggplot(aes(x = variable)) +
        geom_bar()
    }
    
    graphs <- reactive({map(Xnames(), create_hist)})
    output$testplot <- renderPlot(graphs()[4])
    
    observeEvent(df_to_use(), {
      req(graphs())
      
      iwalk(graphs(), ~{
        output_name <- paste0("plot_", .y)
        output[[output_name]] <- renderPlot(.x)
      })
    })
    
    #### renderUI ####
    output$graphs_ui <- renderUI({
      req(graphs())
      
      plots_list <- imap(graphs(), ~{
        tagList(
          plotOutput(
            outputId = paste0("plot_", .y)
          )
        )
      })
      
      tagList(plots_list)
      print(tagList(plots_list))
    })
    
  })
}
