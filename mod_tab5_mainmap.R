#### 
#' Code for 5 tab
#' Rasmus Boman
#' 23.3.2021

source("fct_tab5_mainmap.R")

tab5_ui_main <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   h3("Kartoittaja"),
                   selectInput(ns("filt_kart"), label = "Suodata kartoittajan perusteella:", choices = character())), # Lisää ohjeita jos tarpeen
      mainPanel(width = 10,
                h2("Kartoitukset kartalla"),
                leafletOutput(ns("mainmap"), height = "800px"))))
                #dataTableOutput(ns("summ_lajit")))))
}

tab5LeafletServer <- function(id, df_to_use) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Separate wrangling function from drawing of leaflet:
      data_for_map <- reactive({df_to_use() %>% mutate_for_map() })
      
      #### OUTPUTS ####
      output$mainmap <- renderLeaflet({leaflet_depth_map(data_for_map())})  
      #output$summ_lajit <- renderDataTable({data_for_map()})
    })
}
