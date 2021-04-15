source("fct_menetelmakohtainen_select.R")
source("fct_tab1_import_excel.R")
source("fct_tab3_typos_errors.R")

# Tämä vaikuttaisi olevan ok for now - Rasmus 15.4.2021

#### TAB3 UI ####

tab3_ui_main <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h3("1/5 Puuttuvat, pakolliset tiedot"),
      p("Mikäli tietoja puuttuu, sarake ja määrä on listattu alle. Mikäli tämä harmaa laatikko on tyhjä, kaikki on ok."),
      dataTableOutput(ns("pakolliset_tiedot"))),
    tags$hr(),
    wellPanel(
      h3("2/5 Vaihtuvat tiedot samoilla kohteen numeroilla."),
      p("Tämän taulukon tulee olla tyhjä. Mikäli laatikossa on tietoja, tiedot muuttuvat rivien välillä, vaikka kohteen nro säilyy samana. 
        Mikäli taulukkoa on täytetty käyttäjälomakkeen avulla, alla olevien tietojen tulisi olla identtisiä rivien välillä"),
      dataTableOutput(ns("testiDT"))),
    tags$hr(),
    #wellPanel(
    #  h3("3/XX TEsTIPANeELI"),
    #  verbatimTextOutput(ns("duplos_loop"))),
    #tags$hr(),
    wellPanel(
      h3("3/5 Duplikaattilajit"),
      p("Tämän taulukon tulee olla tyhjä. Alla olevilla pisteillä sama laji esiintyy ruudulla useampaan kertaan (laji, epifyyttisyys ja havainnon laatu identtisiä)."),
      p("Poista duplikaatit Excelistä. Yhdistä tarvittaessa lajitietoja siten, että laji esiintyy ruudulla vain kerran."),
      dataTableOutput(ns("duplos_lajit"))),
    tags$hr(),
    wellPanel(
      h3("4/5 Kategoriset muuttujat"),
      p("Excelistä on poimittu sarakkeet, joissa tulee olla vain muutama eri arvo."),
      p("Käy nämä läpi, ettei joukossa ole omituisia arvoja tai väärässä sarakkeessa olevia tietoja."),
      verbatimTextOutput(ns("cat_vars"))),
    tags$hr(),
    wellPanel(
      h3("5/5 Muut sarakkeet"),
      p("Alla oleva taulukko on useimmiten tyhjä. Siihen on haettu tiedot sarakkeista, jotka eivät täyty Excelin käyttäjälomakkeen kautta."),
      p("Nämä tiedot on siis manuaalisesti täytetty Exceliin ja todennäköisesti ok."),
      dataTableOutput(ns("nonessential_vars"))))
}

#### TAB3 SERVER ####
  
tab3TypoServer <- function(id, df_to_use) {
  moduleServer(id, function(input, output, session) {
    
    #### REACTIVE FUNCTIONS ####
    # Function to create a df with summing NAs together and keeping only them
    pakolliset_sarakkeet <- reactive(df_to_use() %>% 
                                       testaa_pakolliset_sarakkeet())
    
    duplicate_vars <- reactive(df_to_use() %>%
                                 find_duplicated_data()) # fct_tab3_typos_errors

    # Select unique kohteen nrot from duplicate table
    dupl_kohdenrot <- reactive(duplicate_vars() %>%
                                 dplyr::select(kohteen.nro) %>%
                                 map(unique))
    
    # TÄMÄ EI TOIMI 17.3.2021 !!
    #list_of_dupl_values <- reactive(for(i in 1:length(dupl_kohdenrot())){
    #  DetectTyposInCol2(duplicate_vars(), dupl_kohdenrot()[[i]])
    #  })
    
    laji_duplos <- reactive(df_to_use() %>% filter_dupl_lajit())
    
    nonessentials <- reactive(df_to_use() %>% sum_na_nonessential_vars())
    
    #### OUTPUTS ####
    
    output$pakolliset_tiedot <- renderDataTable({data.table(show_if_not_empty_fun(pakolliset_sarakkeet()))})
    output$testiDT <- renderDataTable({data.table(show_if_not_empty_fun(duplicate_vars()))}) ##JATKA TÄMÄN REAKTIIVISUUDESTA MA 15.3.2021½!!!!
    
    #### Jatka tiistaina miksei DetectTyposInCol2 luo listaa vaan vain yhden arvon?
    #output$duplos_loop <- renderPrint({list_of_dupl_values()})
    output$duplos_lajit <- renderDataTable({data.table(show_if_not_empty_fun(laji_duplos()))})
    output$cat_vars <- renderPrint({unique_cat_vars(df_to_use())})
    output$nonessential_vars <- renderDataTable({data.table(nonessentials())})

    #### END OF MODULE SERVER ####
    })
}
