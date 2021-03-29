library(tidyverse)

lajinimet <- read_rds("data/lajit.rds")

# KÄYTÄ NÄITÄ TESTIVAIHEESSA, POISTA JÄLKIKÄTEEN
#tiedostolle_nimi <- as.character(inputATlinjanimi)
inputATlinjanimi <- "33.Kuulinen"
inputATlajimaara <- 6
inputATnaytapohjeliot <- "Pohjaeläimet"

# 1. Separate kohteen nro and select relevant variables
# The data 
prepare_kohde_linjat <- function(data_in){
  data_in %>%
    filter_df_linjat_ruudut() %>%
    mutate(arviointiruudun.syvyys = arviointiruudun.syvyys * -1,
           secchi.syvyys = secchi.syvyys * -1) %>%
    mutate(kohde_ylanro = gsub("\\..*$","", kohteen.nro), #Dot followed by any number of any chars
           ylanimi = gsub("\\_.*$", "", kohteen.nimi)) %>% # Underscore followed by any number of any chars
    mutate(kohde = paste0(kohde_ylanro, ".", ylanimi), .before = kohteen.nro) %>%
    select(kohde, 
           kohteen.nro, kohteen.nimi, secchi.syvyys,
           ruudun.koordinaatti.N, ruudun.koordinaatti.E,
           arviointiruudun.etaisyys, arviointiruudun.syvyys,
           kallio:puun.rungot,
           lajihavainto, lajin.peittavyys, sedimentin.maara)
}

# 2. Join species data with higher level taxa information
join_lajitarkennus <- function(data_in){
  data_in %>%
    left_join(lajinimet, by = "lajihavainto") %>% # Left_join lajidata in order to plot also the higher taxa
    mutate(Tarkennus = replace_na(Tarkennus, levels(lajinimet$Tarkennus)[11])) %>% # All the species don't have a higher taxa -> to avoid NA rename to "Ei m??ritelty"
    mutate(Tarkennus = as_factor(Tarkennus)) %>% # Factorise to ease plotting
    filter(Tarkennus != "Kalat")
}

# 3. Filter based on user input and NEST the species data ####
nest_species_and_kohde <- function(data_in){
  data_in %>%
    filter(kohde == inputATlinjanimi) %>%
    select(kohde, arviointiruudun.etaisyys, arviointiruudun.syvyys, secchi.syvyys, sedimentin.maara, kallio:lajin.peittavyys, Tarkennus) %>%
    nest(data = c(lajihavainto, Tarkennus, lajin.peittavyys)) %>% # Save species data into own dataframe within the line
    remove_empty("cols")
}

# 4. Pivot the substrate data into long form
pivot_longer_nestdata <- function(nested_data){
  nested_data %>%  
    pivot_longer(cols = -c(kohde, arviointiruudun.etaisyys, arviointiruudun.syvyys, secchi.syvyys, sedimentin.maara, data), 
                 names_to = "pohjanlaatu",
                 values_to = "pohj_peitt",
                 values_drop_na = F) %>%
    mutate(pohj_peitt = replace_na(pohj_peitt, 0)) %>%
    dplyr::select(-data)
}

# The whole process in a pipe, use this for species:
linja_nest_species <- function(data_in){ 
  data_in %>%
    prepare_kohde_linjat() %>%
    join_lajitarkennus() %>%
    nest_species_and_kohde() 
}

linja_nest_substrates <- function(data_in){
  data_in %>%
    pivot_longer_nestdata() 
}
