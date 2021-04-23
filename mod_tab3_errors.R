source("fct_menetelmakohtainen_select.R")
source("fct_tab1_import_excel.R")
source("fct_tab3_typos_errors.R")

library(kableExtra)

# Tämä vaikuttaisi olevan ok for now - Rasmus 15.4.2021

#### TAB3 UI ####

tab3_ui_main <- function(id){
  ns <- NS(id)
  tagList(
    h3("1/6 Puuttuvat, pakolliset tiedot"),
    wellPanel(
      p("Mikäli tietoja puuttuu, sarake ja puuttuvien arvojen määrä on listattu alle. Mikäli tämä harmaa laatikko on tyhjä, kaikki on ok."),
      dataTableOutput(ns("pakolliset_tiedot"))),
    tags$hr(),
    h3("2/6 Kohteen numero, nimi ja koordinaatit"),
    wellPanel(
      p("Joitain poikkeuksia lukuun ottamatta taulukossa jokaisella koordinaatilla tulisi olla uniikki nimi ja numero."),
      textOutput(ns("lkm_nimi")),
      p("Näiden lukujen tulisi täsmätä. Mikäli näin ei ole, alla olevista taulukoista löytyy rivit, joissa vika."),
      p("Jos määrät täsmäävät, tässä laatikossa ei pitäisi olla taulukoita."),
      tags$hr(),
      h4("Useammat koordinaatit yhdellä kohteen numerolla"),
      dataTableOutput(ns("dupl_nr_koord")),
      tags$hr(),
      h4("Useammat koordinaatit yhdellä kohteen nimellä"),
      dataTableOutput(ns("dupl_name_koord")),
      tags$hr(),
      h4("Koordinaatit säilyy samana, kohteen nimi tai numero vaihtuu"),
      dataTableOutput(ns("dupl_koordinaatit"))),
    h3("3/6 Vaihtuvat tiedot samoilla kohteen numeroilla."),
    wellPanel(
      p("Tämän laatikon tulee olla tyhjä. Mikäli laatikossa on tietoja, tiedot muuttuvat rivien välillä, vaikka kohteen numero säilyy samana. 
        Mikäli taulukkoa on täytetty käyttäjälomakkeen avulla, lajitietoja lukuunottamatta tietojen tulisi olla identtisiä kullakin pisteellä."),
      dataTableOutput(ns("testiDT"))),
    tags$hr(),
    h3("4/6 Duplikaattilajit"),
    wellPanel(
      p("Tämän laatikon tulee olla tyhjä. Alla olevilla pisteillä sama laji esiintyy ruudulla useampaan kertaan (laji, epifyyttisyys ja havainnon laatu identtisiä)."),
      p("Kommenttisarakkeen mahdolliset lisätiedot eivät riitä erottelemaan lajistoa, mikäli lajihavainto ei muutu."),
      p("Poista duplikaatit Excelistä. Yhdistä tarvittaessa lajitietoja siten, että laji esiintyy ruudulla vain kerran."),
      dataTableOutput(ns("duplos_lajit"))),
    tags$hr(),
    h3("5/6 Kategoriset muuttujat"),
    wellPanel(
      p("Excelistä on poimittu sarakkeet, joissa tulee olla vain muutama eri arvo."),
      p("Käy nämä läpi, ettei joukossa ole omituisia arvoja tai väärässä sarakkeessa olevia tietoja."),
      verbatimTextOutput(ns("cat_vars"))),
    tags$hr(),
    h3("6/6 Muut sarakkeet"),
    wellPanel(
      p("Tämä ruutu on useimmiten tyhjä. Tähän on haettu tiedot sarakkeista, jotka eivät täyty Excelin käyttäjälomakkeen kautta."),
      p("Nämä tiedot on siis manuaalisesti täytetty Exceliin ja todennäköisesti ok. Mikäli taulukkoa on muokattu 
        tai tietoja kopioitu taulukosta toiseen, nämä kannattaa tarkistaa."),
      dataTableOutput(ns("nonessential_vars"))))
}

#### TAB3 SERVER ####
  
tab3TypoServer <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    
    #### REACTIVE FUNCTIONS ####
    kohdemaara <- reactive({df_to_use() %>% 
        filter_df_linjat_ruudut() %>%
        select(kohteen.nimi) %>% 
        n_distinct()
      })
    
    kohteen_numerot <- reactive({df_to_use() %>%
        filter_df_linjat_ruudut() %>%
        select(kohteen.nro) %>%
        n_distinct()
    })
    
    lkm_koord <- reactive({df_to_use() %>% 
        filter_df_linjat_ruudut() %>%
        transmute(koordinaatitYHT = paste(ruudun.koordinaatti.N, ruudun.koordinaatti.E, sep = "")) %>% 
        n_distinct()
    })
    

    dupl_nrs_df <- reactive({df_to_use() %>% dupl_nr_koords()})
    dupl_names_df <- reactive({df_to_use() %>% dupl_name_koords()})
    dupl_koords_df <- reactive({df_to_use() %>% dupl_koords()})
    # Function to create a df with summing NAs together and keeping only them
    pakolliset_sarakkeet <- reactive(df_to_use() %>% testaa_pakolliset_sarakkeet())
    
    duplicate_vars <- reactive(df_to_use() %>% find_duplicated_data()) # fct_tab3_typos_errors

    # Select unique kohteen nrot from duplicate table
    dupl_kohdenrot <- reactive(duplicate_vars() %>%
                                 dplyr::select(kohteen.nro) %>%
                                 map(unique))
    
    # TÄMÄ EI TOIMI 17.3.2021 !! Yrittää poimia tekstimuotoon poikkeavuudet rivien välillä.
    #list_of_dupl_values <- reactive(for(i in 1:length(dupl_kohdenrot())){
    #  DetectTyposInCol2(duplicate_vars(), dupl_kohdenrot()[[i]])
    #  })
    
    laji_duplos <- reactive(df_to_use() %>% filter_dupl_lajit())
    
    nonessentials <- reactive(df_to_use() %>% sum_na_nonessential_vars())
    
    #### OUTPUTS ####
    # Wellpanel 1
    output$pakolliset_tiedot <- renderDataTable({data.table(show_if_not_empty_fun(pakolliset_sarakkeet()))})
    # WP 2
    output$lkm_nimi <- renderText({paste0("Uniikkeja kohteen nimiä taulukossa: ", kohdemaara(), 
                                         ". Kohteen numeroita: ", kohteen_numerot(), ". Koordinaatteja: ", lkm_koord())})
    output$dupl_nr_koord <- renderDataTable({data.table(show_if_not_empty_fun(dupl_nrs_df()))})
    output$dupl_name_koord <- renderDataTable({data.table(show_if_not_empty_fun(dupl_names_df()))})
    output$dupl_koordinaatit <- renderDataTable({data.table(show_if_not_empty_fun(dupl_koords_df()))})
    # WP 3 Kohteen numero verrattuna muihin arvoihin 
    output$testiDT <- renderDataTable({data.table(show_if_not_empty_fun(duplicate_vars()))})
    # WP 4 Duplikaattilajit
    output$duplos_lajit <- renderDataTable({data.table(show_if_not_empty_fun(laji_duplos()))})
    # WP 5 Kategoriset muuttuja
    output$cat_vars <- renderPrint({unique_cat_vars(df_to_use())})
    # WP 6 Epäolennaiset sarakkeet
    output$nonessential_vars <- renderDataTable({data.table(show_if_not_empty_fun(nonessentials()))})

    #### END OF MODULE SERVER ####
    })
}
