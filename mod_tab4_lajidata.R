
#' Modules for UI and Server of Tab4
#' Author : Rasmus Boman
#' Date : 19.3.2021
#' Vaikuttaisi olevan ok 15.4.2021 -Rasmus

source("fct_tab4_lajidata.R")

#### TAB4 UI #### 

tab4_ui_main <- function(id){
  ns <- NS(id)
  tagList(
  fluidRow(
    #### Taulukot lajeista ####
    column(4,
           # Lajihavainnot
           h4("Lajihavainnot ja havaintojen määrä taulukossa"),
           p("Tarkista ainakin harvinaisimmat havainnot, näissä on suurin typojen mahdollisuus."),
           wellPanel(dataTableOutput(ns("lajit_df"))),
           # Lajinimien vertailu
           h4("Lajisto & typot"),
           p("'Lajien oikeinkirjoitus'-taulukon tulisi olla tyhjä. Näissä lajeissa on: "), 
           p("a) Jokin kirjoitusvirhe esim. isoissa ja pienissä kirjaimissa"),
           p("b) Lajinimi on oikein kirjoitettu, mutta sitä ei ole päivitetty Excelin käyttäjälomakkeelle."),
           p(paste("Taulukkoa ei tulisi täyttää ilman käyttäjälomaketta eli b-tapauksissa lajin TULISI OLLA merkitty 'Lajia ei listassa' ja lajinimen kommenteissa.",
           "Nämä uudet havainnot löytyvät alemmasta taulukosta ja nämä on helppo tarkistaa Metsähallituksen toimesta jälkikäteen.",
           "Summa summarum: Alemmassa taulukossa saa olla rivejä ja lajit kommenteissa. 'Lajien oikeinkirjoitus'-taulukon tulee olla tyhjä.")),
           h4("Lajien oikeinkirjoitus"),
           wellPanel(dataTableOutput(ns("lajinimi_vertailu"))),
           h4("Lajit, joita ei ole käyttäjälomakkeen lajilistassa"),
           wellPanel(dataTableOutput(ns("lajia_ei_listassa")))),
    #### Leaflet-kartta lajistosta ####
    column(8,
           wellPanel(h4("Lajistokartta"),
                     p("Klikkaamalla karttamerkkiä saat ruudusta lisätietoa."),
                     leafletOutput(ns("laji_leaflet"), height = "500px")),
           wellPanel(selectInput(ns("filt_laji"), label = "Suodata kartalla näytettävät lajit:", choices = character())),
           #### Outlierit taulukossa ####
           h4("Outlierit"),
           wellPanel(dataTableOutput(ns("outlierit"))),
           wellPanel(selectInput(ns("filt_outlier"), label = "Tarkista outlier-sijainnit kohteen numeron perusteella", choices = character())),
           wellPanel(h4("Outlier-kartta"),
                     p("Klikkaamalla karttamerkkiä saat ruudusta lisätietoa."),
                     leafletOutput(ns("outlier_leaflet"), height = "500px"))
           )
    )
  )
}
#### END UI ####

#### TAB4 Server ####

tab4LajiDataServer <- function(id, df_to_use) {
  moduleServer(
    id, 
    function(input, output, session) {
    
      #### Reactive functions ####
      # Show the amount of species in the table
      lajimaarat <- reactive(df_to_use() %>% return_lajilista()) 
      
      # Compare the species names to Hertta
      laji_comparison <- reactive(df_to_use() %>% compare_species_to_hertta()) 
      
      # Show 'lajia ei listassa' species
      uudet_lajit <- reactive(df_to_use() %>% return_new_species())
      
      # Observe when new df is loaded and update species list for leaflet
      observeEvent(df_to_use(), {updateSelectInput(session, "filt_laji", choices = lajimaarat()$lajihavainto)})
      
      # Filter species on map based on user input -> Create new df 
      lajisto_kartalle <- reactive({
        df_to_use() %>%
          dplyr::filter(lajihavainto %in% input$filt_laji) %>%
          dplyr::mutate(ruudun.koordinaatti.E = as.numeric(ruudun.koordinaatti.E), 
                 ruudun.koordinaatti.N = as.numeric(ruudun.koordinaatti.N))
      })
      
      # Create the leaflet map
      leaflet_map <- reactive(
        lajisto_kartalle() %>%  # Defined above, user input filtered df with just on species 
          edit_kartoittaja_syvyys() %>% # Mutate depth and surveyor to one column on both videos and dive points (fct_tab4_lajidata)
          create_leaflet_species # Create the leaflet (fct_tab4_lajidata)
      )

      outlier_species <- reactive(df_to_use() %>% filter_based_on_growth_limits())
      
      # Observe when new df is loaded and update species list for leaflet
      observeEvent(df_to_use(), {updateSelectInput(session, "filt_outlier", choices = outlier_species()$kohteen.nro)})
      
      # Filter species on map based on user input -> Create new df 
      outlierit_kartalle <- reactive({
        df_to_use() %>%
          dplyr::filter(kohteen.nro %in% input$filt_outlier) %>%
          dplyr::mutate(ruudun.koordinaatti.E = as.numeric(ruudun.koordinaatti.E), 
                        ruudun.koordinaatti.N = as.numeric(ruudun.koordinaatti.N))
      })
      
      # Create the leaflet map
      outlier_map <- reactive(
        outlierit_kartalle() %>%  # Defined above, user input filtered df with just on species 
          edit_kartoittaja_syvyys() %>% # Mutate depth and surveyor to one column on both videos and dive points (fct_tab4_lajidata)
          create_leaflet_species # Create the leaflet (fct_tab4_lajidata)
      )

      

      #### Outputs ####
      output$laji_leaflet <- renderLeaflet({leaflet_map()})
      output$lajit_df <- renderDataTable({data.table(lajimaarat())})
      output$lajinimi_vertailu <- renderDataTable({data.table(laji_comparison())})
      output$lajia_ei_listassa <- renderDataTable({data.table(uudet_lajit())})
      output$outlierit <- renderDataTable({data.table(outlier_species())})
      output$outlier_leaflet <- renderLeaflet({outlier_map()})
    })
}

#### END SERVER ####