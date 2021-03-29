## JATKA KESKIVIIKKONA 23.3.2021 ##

# Module for tab nr 6
# Created by: Rasmus Boman 
# Date: 23.3.2021

source("fct_tab6_drawplots.R")

tab6_ui_main <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 2,
                   h3("Linja"),
                   selectInput(ns("linjanro"), label = "Suodata linjan perusteella:", choices = character()),
                   checkboxInput(ns("naytapohjeliot"), label = "Näytä pohjaeläimet"),
                   numericInput(ns("lajimaara"), label = "Näytettävä lajimäärä", value = 6)), # Lisää ohjeita jos tarpeen
      mainPanel(width = 10,
                h2("Linjakuvaaja"),
                plotOutput(ns("lajiplot")))))
}

tab6LinegraphServer <- function(id, df_to_use) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      linjat_list <- reactive(df_to_use() %>% prepare_kohde_linjat()) 
      
      # Separate wrangling function from drawing of leaflet:
      observeEvent(df_to_use(), {updateSelectInput(session, "linjanro", choices = unique(linjat_list()$kohde))})
      
      # data_for_plot <- reactive({df_to_use() %>% MUTATE_FOR_PLOT() })
      
      #### OUTPUTS ####
      output$lajiplot <- renderPlot({lajimaaraplot})  
      #output$summ_lajit <- renderDataTable({data_for_map()})
    })
}
