library(tidyverse)

lajinimet <- read_rds("data/lajit.rds")
source("fct_tab1_import_excel.R")
source("fct_menetelmakohtainen_select.R")

# KÄYTÄ NÄITÄ TESTIVAIHEESSA, POISTA JÄLKIKÄTEEN
#tiedostolle_nimi <- as.character(inputATlinjanimi)
test_df33 <- return_filtered_df("C:/Users/Rasmusbo/OneDrive - Metsahallitus/Data2020/SYKE/Velmu2020_sukelluslinjat_yhdistetty.xlsx")
inputATlinjanimi <- "27.Tallholmen"
#inputATlajimaara <- 6
# inputATnaytapohjeliot <- "Pohjaeläimet"

# 1. Separate kohteen nro and select relevant variables
# The data 
prepare_kohde_linjat <- function(data_in){
  data_in %>%
    filter(kohteen.taso == 63,
           lajihavainto != "Ei lajihavaintoa") %>%
    mutate(arviointiruudun.syvyys = arviointiruudun.syvyys * -1,
           secchi.syvyys = secchi.syvyys * -1) %>%
    mutate(kohde_ylanro = gsub("\\..*$","", kohteen.nro), #Dot followed by any number of any chars
           ylanimi = gsub("\\_.*$", "", kohteen.nimi)) %>% # Underscore followed by any number of any chars
    mutate(kohde = paste0(kohde_ylanro, ".", ylanimi), .before = kohteen.nro) %>%
    select(kohde, kohteen.nro, kohteen.nimi,
           ruudun.koordinaatti.N, ruudun.koordinaatti.E,
           arviointiruudun.etaisyys, arviointiruudun.syvyys,
           kallio:puun.rungot, lajihavainto, lajin.peittavyys)
}

filter_one_diveline <- function(data_in, line_input_from_app){
  data_in %>% 
    filter(kohde == line_input_from_app)
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
    select(kohde, arviointiruudun.etaisyys, arviointiruudun.syvyys, kallio:lajin.peittavyys, Tarkennus) %>%
    nest(data = c(lajihavainto, Tarkennus, lajin.peittavyys)) %>% # Save species data into own dataframe within the line
    remove_empty("cols")
}

# 4. Pivot the substrate data into long form
pivot_longer_nestdata <- function(nested_data){
  nested_data %>%  
    pivot_longer(cols = -c(kohde, arviointiruudun.etaisyys, arviointiruudun.syvyys, data), 
                 names_to = "pohjanlaatu",
                 values_to = "pohj_peitt",
                 values_drop_na = F) %>%
    mutate(pohj_peitt = replace_na(pohj_peitt, 0)) %>%
    dplyr::select(-data)
}

# The whole process in a pipe, use this for species:
linja_nest_species <- function(data_in){ 
  data_in %>%
   # prepare_kohde_linjat() %>%
    join_lajitarkennus() %>%
    nest_species_and_kohde() 
}

# And this for substrate plot
linja_nest_substrates <- function(data_in){
  data_in %>%
    #prepare_kohde_linjat() %>%
    join_lajitarkennus() %>%
    nest_species_and_kohde() %>%
    pivot_longer_nestdata() 
}

#Test
#linja_nest_species(test_df)
#linja_nest_substrates(test_df)

# Factorizing the substrates for visualisation
factorize_substrates <- function(raw_data){
  raw_data %>%
    linja_nest_substrates() %>%
    mutate(pohjanlaatu = factor(pohjanlaatu, 
                                levels = c("kallio", "lohkareab", "lohkarebb", "lohkarecb", "glasiaalisavi", "kivias", 
                                           "kivibs", "sora", "hiekka", "siltti", "savi", "muta", "konkreetiot", 
                                           "hiekkakivi", "keinotekoinen.alusta", "turve", "puun.rungot"),
                                labels = c("Kallio", "Lohkare > 3m", "Lohkare 1.2-3 m", "Lohkare 0.6-1.2 m", "Glasiaalisavi", "Kivi 10-60 cm", 
                                           "Kivi 6-10 cm", "Sora", "Hiekka", "Siltti", "Savi", "Muta", "Konkreetiot", 
                                           "Hiekkakivi", "Keinotekoinen alusta", "Turve", "Puun rungot")))
}

#factorize_substrates(test_df)

colcodes_for_pohjat <- c("#525252", "#00564C", "#0D726A", "#2E9088", "#969696", #Kallio, lohkare, glasiaalisavi
                         "#2171B5", "#4292C6", "#CFA155", "#E1C685", "#F0DEB1", # Kivet -> Siltti
                         "#774408", "#543005", "#F0F0F0", "#F5ECD5", "#252525", 
                         "#E5F5E0", "#A1D99B") # Puunrungot, turva

# Create color palettes etc. Siirretty mod-puolelle

#nb.cols <- length(levels(subst_df$pohjanlaatu)) # Luodaan int, pohjanlaatujen määrä = värien määrä
#names(colcodes_for_pohjat) <- c(levels(subst_df$pohjanlaatu)) # Nimetään värit pohjanlaatujen mukaan
#pohjalaadut_scale <- scale_fill_manual(name = "Pohjanlaatu", values = colcodes_for_pohjat) # Manuaalinen täyttöscale pohjanlaaduille

#tarkennus.cols <- length(levels(lajinimet$Tarkennus)) # Create a int of how many colors = how many substrates
#tarkennus.colors <- c("#D7301F", "#016C59", "#8c510A", "#EC7014", "#C7E9C0", "#238B45", "#FEB24C", "#DFC27D" ,"#807DBA", "#807DBA" ,"#969696") #74A9CF, "#35978F"
#names(tarkennus.colors) <- c(levels(lajinimet$Tarkennus))
#lajitarkennus_scale <- (scale_color_manual(name = "lajitarkennus", values = tarkennus.colors))

# Lajitellaan linja viiteen segmenttiin etäisyyden perusteella
lines_sep_distances <-  function(data_in){
  seq(from = min(data_in$arviointiruudun.etaisyys),
      to = max(data_in$arviointiruudun.etaisyys),
      by = (max(data_in$arviointiruudun.etaisyys) / 5))
}

#### Erotellaan lajidata ####

# Lajitellaan lajit etäisyysluokkiin
luok_lajit <- function(raw_data, lines_sep) {
  linja_nest_species(raw_data) %>% 
    unnest(cols = c("data")) %>%
    select(arviointiruudun.etaisyys, arviointiruudun.syvyys, lajihavainto, Tarkennus, lajin.peittavyys) %>%
    mutate(etais_luokka = case_when(
      arviointiruudun.etaisyys <= lines_sep[2] ~ (lines_sep[1] + lines_sep[2] / 2),
      arviointiruudun.etaisyys > lines_sep[2] & arviointiruudun.etaisyys <= lines_sep[3] ~ (lines_sep[2] + lines_sep[2] / 2),
      arviointiruudun.etaisyys > lines_sep[3] & arviointiruudun.etaisyys <= lines_sep[4] ~ (lines_sep[3] + lines_sep[2] / 2),
      arviointiruudun.etaisyys > lines_sep[4] & arviointiruudun.etaisyys <= lines_sep[5] ~ (lines_sep[4] + lines_sep[2] / 2),
      arviointiruudun.etaisyys > lines_sep[5] ~ (lines_sep[5] + lines_sep[2] / 2),
      TRUE ~ as.numeric(NA)
    ))
}