# To whomever may continue this:
# Sorry, it's a mess between this script, fct_tab6b_wrangling.R and fct_tab6_drawplots.R
## But the time is running out :)

# Module for tab nr 6
# Created by: Rasmus Boman 
# Date: 23.3.2021

source("fct_tab6_drawplots.R")
source("fct_menetelmakohtainen_select.R")

tab6_ui_main <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   h3("Linja"),
                   selectInput(ns("linjanro"), label = "Suodata linjan perusteella:", choices = character()),
                   textOutput(ns("kokoomavertailu"))),
                   # checkboxInput(ns("naytapohjeliot"), label = "Näytä pohjaeläimet"),
                   #numericInput(ns("lajimaara"), label = "Näytettävä lajimäärä", value = 6)), # Lisää ohjeita jos tarpeen
      mainPanel(width = 9,
                h2("Linjakuvaaja"),
                plotOutput(ns("main_plot"), height = "600px"),
                plotOutput(ns("substr_plot"), height = "300px"))))
}

tab6LinegraphServer <- function(id, df_to_use) {
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Observes the diveline user chooses
      observeEvent(df_to_use(), {updateSelectInput(session, "linjanro", choices = unique(list_of_lines()$kohde))})
      
      # Mutates and selects a list of lines for user interface to select from
      list_of_lines <- reactive(df_to_use() %>% prepare_kohde_linjat())
      # Uses only the selected line in further functions
      selected_line_df <- reactive(df_to_use() %>% prepare_kohde_linjat() %>% filter_one_diveline(input$linjanro))
      # Nests species so they are accessible for plotting
      species_df <- reactive(selected_line_df() %>% linja_nest_species()) 
      # Wrangles substrates so they are accessible for plotting
      substrates_df <- reactive(selected_line_df() %>% linja_nest_substrates())
      # Factorizes the substrates
      factor_subst_df <- reactive(selected_line_df() %>% factorize_substrates())
      
      # Creates 5 sections for surveysquares on the dive line..  
      separated_distances <- reactive(selected_line_df() %>% lines_sep_distances())
      # And categorizes species into each
      categorised_species <- reactive(luok_lajit(raw_data =  selected_line_df(), lines_sep = separated_distances()))
      # Chooses top 6 species from each interval for plotting
      top_lajit_df <- reactive(categorised_species() %>% top_cover_lajit())
      
      #### Create color palettes etc. Move to fct-script if time ####
      
      #nb.cols <- reactive(length(levels(factor_subst_df()$pohjanlaatu))) # Luodaan int, pohjanlaatujen määrä = värien määrä
      #colcodes_for_pohjat <- reactiveVal(1 %>% c(levels(factor_subst_df()$pohjanlaatu)))
      #isola
      #names(colcodes_for_pohjat) <<- reactive(c(levels(factor_subst_df()$pohjanlaatu))) # Nimetään värit pohjanlaatujen mukaan
      #pohjalaadut_scale <<- scale_fill_manual(name = "Pohjanlaatu", values = colcodes_for_pohjat()) # Manuaalinen täyttöscale pohjanlaaduille
      
      #### ####
      # The main plot with topography and species
      syvyys_plot <- reactive(syv_plot(linja_nest_species =  species_df(), 
                                       luok_lajit = categorised_species(),
                                       top_cover_lajit_df = top_lajit_df()))
      

      
      #output$kokoomavertailu <- renderText(paste("Testi", "testi2"))
      # data_for_plot <- reactive({df_to_use() %>% MUTATE_FOR_PLOT() })
      
      #### OUTPUTS ####
      output$substr_plot <- renderPlot(subs_plot(factor_subst_df()))
      output$main_plot <- renderPlot(syvyys_plot())
      # output$lajiplot <- renderPlot({lajimaaraplot})  
      # output$summ_lajit <- renderDataTable({data_for_map()})
    })
}
