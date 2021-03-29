## Source code for the first tab of Velmu-app

source("fct_tab1_import_excel.R")
source("fct_menetelmakohtainen_select.R")
library(leaflet)

# Consider whether this check is necessary or whether it should be included on the next page technical checks
tab1_ui <- function(id) {
  tagList(selectInput(NS(id, "taulukkomuoto"), "Valitse taulukon kartoitusmenetelmä", choices = c("Video", "Linja", "Kartoituspiste")))
}

tab1_ui_main <- function(id){
  ns <- NS(id)
  tagList(leafletOutput(ns("overallMap"), height = "600px"),
          tags$hr(),
          h2("Yleiskatsaus kartoituksiin"),
          dataTableOutput(ns("overallDatatable")))
}


tab1MapServer <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    
    # Create a reactive, selected dataframe to be used in map and datatable
    df_uniq <- reactive({df_to_use() %>%
        filter_df_linjat_ruudut() %>%
        dplyr::mutate(kohteen.nimi = kohteen.nimi,
                      ruudun.koordinaatti.E = round(as.numeric(ruudun.koordinaatti.E), digits = 5),
                      ruudun.koordinaatti.N = round(as.numeric(ruudun.koordinaatti.N), digits = 5)) %>%
        dplyr::select(kohteen.nro, kohteen.nimi, kartoituspvm, kartoitusmenetelma, 
                      ruudun.koordinaatti.E, ruudun.koordinaatti.N) %>%
        unique()
    })
    
    # Leaflet output, continue this if needed 
    # Add a selection tool to grasp certain points and show the datatable?
    output$overallMap <- renderLeaflet({
      req(df_uniq())
      leaflet::leaflet(df_uniq()) %>%
        addTiles() %>%
        addMarkers(~as.numeric(ruudun.koordinaatti.E),
                   ~as.numeric(ruudun.koordinaatti.N))
    })
    
    output$overallDatatable <- renderDataTable({data.table(df_uniq())})
    
  })
}
