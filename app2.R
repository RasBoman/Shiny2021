#### INFO ####

# 
# ShinyApp to read and examine data in VELMU-format
# Author : Rasmus Boman
# Path to wdir = C:\Users\Rasmusbo\OneDrive - Metsahallitus\R\Shiny2021
# Date created : 8.3.2021
# Last updated : 17.3.2021

############## LISÄÄ JOHONKIN NÄISSÄ SARAKKEISSA EI PITÄISI OLLA TIETOA ############

#' App written in modules:
#' 
#' "mod_tabX_YY.R" contains the code for UI and SERVER for the tab described
#' fct_tabX_YY.R" contains the functions for the same tab module
#' 
# getwd()

#### Libraries an

source("mod_tab1_ui_import_excel.R")
#source("mod_tab2_plots.R") NOT READY
source("mod_tab3_errors.R")
source("fct_tab1_import_excel.R")
source("texts_for_app.R")
source("mod_tab4_lajidata.R")
source("mod_tab5_mainmap.R")
#source("mod_tab6_linegraph.R")
options(shiny.maxRequestSize = 30*1024^2, encoding = "UTF-8")

# source("utils.R")
#### Constant variables and dataframes ####

# Path to this project = 

lajinimet <- read_rds("data/lajit.rds")

#### UI ####
ui <- fluidPage(
  navbarPage("VELMU-tarkastus",
             
             #### 1. tabPanel: Start from here ####
             # 10.3.2021 Ok for now
             tabPanel("Lataa kartoitustaulukko",
                      # Layout: erillinen paneeli vasemmassa reunassa
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     tab1_ui("tab1"), # mod_tab1_ui_import_excel.R
                                     fileInput("file1", "Aloita lataamalla excel-taulukko:", multiple = FALSE, accept = c(".xls", ".xlsx", ".xlsm")),
                                     p(tab1_import_ohje)), # texts_for_app.R
                        
                        mainPanel(width = 9,
                                  h2("Kartta"),
                                  tab1_ui_main("map1")))), # mod_tab1_ui_import_excel.R

             #### 2. tabPanel: Kuvaajat ####
             # Rakenna tähän boxplotteja map-funktiolla? KESKEN
             tabPanel("Kuvaajat",
                      sidebarLayout(
                        # sivupaneelissa ohjeistus ja filtteröintimahdollisuuksia?
                        sidebarPanel(width = 3),
                        mainPanel(width = 9,
                                  h2("Boxplotit"),
                                  uiOutput("ruudunSyvyysBoxPlot")))),
             
             #### 3. tabPanel: Teknisiä tarkastuksia erityisesti duplikaattien suhteen ####
             tabPanel("Tekninen tarkastus",
                      sidebarLayout(
                        sidebarPanel(width = 2,
                                     h3("Ohjeistus"),
                                     tab3_sidepanel_txt), # Lisää ohjeita jos tarpeen
                        mainPanel(width = 10,
                                  h2("Tekniset tarkastukset & typot"),
                                  tab3_ui_main("pakolliset_tiedot")))),
             #### 4. tabPanel: Lajidata ####
             tabPanel("Lajitiedot",
                     tab4_ui_main("lajiaineisto")),
             #### 5. tabPanel: Pääkartta Add Brush? ####
             tabPanel("Kartta",
                      tab5_ui_main("paakartta"))#,
             #### 6. tabPanel: Linjakuvaajat ####
             #tabPanel("Linjakuvaaja",
              #        tab6_ui_main("linjagraafit"))
             )
  )
#### END UI ####
server <- function(input, output, session) {
  
  #### REACTIVE Uploaded dataframe ####
  # This dataframe is uploaded by the user. Use this in further server functions
  # Until user uploads own data, testilinjat is shown as example.
  ogdf <- reactive({

    if (!is.null(input$file1)){
      aineisto <- return_filtered_df(input$file1$datapath)
    } else {
      aineisto <- return_filtered_df("data/testilinjat.xlsx")
    }
    return(aineisto)
  })

  #### Calls To tab MODULES ####
  
  # Tab 1 the benigging
  tab1MapServer("map1", df_to_use = ogdf) # mod_tab1_ui_import_excel.R
  # Tab 2 graphs
  # TAB2SERVER HERE
  # Tab 3 Errors and typos
  tab3TypoServer("pakolliset_tiedot", df_to_use = ogdf)
  # Tab 4 Species data
  tab4LajiDataServer("lajiaineisto", df_to_use = ogdf)
  # Tab 5 Leaflet map
  tab5LeafletServer("paakartta", df_to_use = ogdf)
  # Tab 6 Graphs of dive lines
  #tab6LinegraphServer("linjagraafit", df_to_use = ogdf)
  
}

#### END SERVER ####
shinyApp(ui = ui, server = server)
