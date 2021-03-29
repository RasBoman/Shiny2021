
#### INFO ####

# Created along Shiny2021 project / app
# This script includes essential functions for the app to work. 
# Date created = 19.2.2021
# Date updated = 19.2.2021

#### LIBRARIES ####

library(tidyverse)
library(shiny)
library(readxl)
library(janitor)

#### Functions to mutate dataframes ####

read_velmu_xl <- function(path_to_file) {read_xlsx(path_to_file,
                                                   sheet = 1,
                                                   skip = 5,
                                                   .name_repair = "universal",
                                                   guess_max = 10000) %>%
    select(1:148)
} # Read in data with VELMU-specifications

mutate_velmu_xl <- function(raw_velmu_df) {
  
  aineisto <- raw_velmu_df %>%
    select(kohteen.nro = 1,
           kohteen.taso = 2,
           kartoituksen.tarkoitus = 3, 
           kohteen.nimi = 4, 
           alkukoordinaatti.N = 5, # 5-8 vain kokoomalinjoilla (62)
           alkukoordinaatti.E = 6,
           loppukoordinaatti.N = 7,
           loppukoordinaatti.E = 8,
           ruudun.koordinaatti.N = as.numeric(9),
           ruudun.koordinaatti.E = as.numeric(10),
           EI_TIETOA = 11, #
           EI_TIETOA2 =12, #
           pisteen.id = 13,
           kartoitusmenetelma = 14, 
           kartoitusmenetelman.tarkennus = 15, 
           runsausarvioinnin.menetelma = 16,
           avoimuusindeksi = 17,
           kohteen.huomautukset = 18, 
           otantamenetelma = 19, 
           peittavyyden.arviointi = 20,
           kart.tarkistustarve = 21,
           SYKEID = 22, #
           kartoituskerta = 23,
           kartoituspvm = 24,
           aloitusaika = 25,
           kenttahenkilot = 26, 
           vene = 27, 
           veden.lampotila = 28,
           lampotilan.mittaussyvyys = 29,
           secchi.syvyys = 30, 
           levakukinta = 31, 
           tuulen.suunta = 32, 
           tuulen.voimakkuus = 33,
           sedimentin.koodisto = 34,
           sedimentin.maara = 35, 
           saliniteetti = 36, #
           
           TALLENTAJAN.NIMI = 37,
           TALLENTAJAN.ORG = 38,
           
           ## secchi.levyn.koko = 39, tama uudessa versiossa ilmeisesti.. ##
           kartoituskerran.huomautukset = 39, 
           YMPARISTOTYYPPI = 40, #
           REHEV.HERKKA = 41, #
           JOKIP.TYYPPI = 42, #
           RANNAN.KALT = 43, #
           VESIALUE.KMUOTO = 44, #
           RANNAN.KASV = 45, #
           R.KASV.TARK = 46, #
           videon.tallennuslaite = 47,
           videon.ID = 48, 
           videon.kesto = 49,
           videon.syvyyden.korjaus = 50,
           videon.alkusyvyys = 51,
           videon.loppusyvyys = 52,
           videon.analysointipvm = 53,
           videon.analysoija = 54,
           videon.laatu = 55,
           VIDEON.VARI = 56, #
           
           sukelluslinjan.kartoittaja = 57,
           sukelluslinjan.pituus = 58,
           sukelluslinjan.kompassisuunta = 59,
           sukelluslinjan.etaisyys.rannasta = 60,
           sukelluslinjan.alkusyvyys = 61,
           sukelluslinjan.loppusyvyys = 62,
           sukelluslinjan.syvyyden.korjaus = 63, 
           pohjan.kaltevuus = 64, 
           arviointiruudun.pinta.ala = 65, 
           arviointiruudun.syvyys = 66,
           SYVYYDEN.TARKENNE = 67, #
           arviointiruudun.etaisyys = 68,
           ETAIS.MITMEN = 69, #
           
           kasv.alarajan.lajit = 70,
           kasv.alarajan.syvyys = 71,
           kasv.alarajan.etaisyys = 72,
           SYVIN.LAJI = 73, #
           SYVIN.LAJI.SYVYYS = 74, #
           SYVIN.LAJI.ETAISYYS = 75, #
           matalin.fucus.syvyys = 76,
           matalin.fucus.etaisyys = 77,
           syvin.fucus.syvyys = 78,
           syvin.fucus.etaisyys = 79,
           
           vyohykkeen.muodostaja.ylataso = 80,
           vyohykkeen.valtalaji = 81,
           vyohykkeen.alaraja.syvyys = 82,
           vyohykkeen.alaraja.etaisyys = 83,
           vyohykkeen.ylaraja.syvyys = 84,
           vyohykkeen.ylaraja.etaisyys = 85,
           runsain.vyohyke.alaraja.syvyys = 86,
           runsain.vyohyke.alaraja.etaisyys = 87,
           runsain.vyohyke.ylaraja.syvyys = 88,
           runsain.vyohyke.ylaraja.etaisyys = 89,
           
           kallio = 90,
           lohkareab = 91, 
           lohkarebb = 92,
           lohkarecb = 93,
           glasiaalisavi = 94,
           kivias = 95,
           liikkumaton.pohja = 96,
           kivibs = 97,
           sora = 98,
           hiekka = 99,
           siltti = 100,
           savi = 101,
           muta = 102,
           liikkuva.pohja = 103,
           konkreetiot = 104,
           hiekkakivi = 105,
           keinotekoinen.alusta = 106,
           turve = 107,
           puun.rungot = 108,
           pohjanlaadut.yhteensa = 109,
           
           epavarma.pohja = 110,
           VELMU.SORA = 111, #
           VELMU.HIEKKA = 112, #
           VELMU.LIEJU = 113, #
           
           roskat.koodisto = 114,
           roskat.kpl = 115,
           
           havainnon.tarkistustarve = 116,   
           lajihavainto = 117, 
           lajin.peittavyys = 118, 
           lajin.lukumaara = 119,
           lajin.maaran.yksikko = 120,
           lajin.korkeus = 121,
           LAJIN.BIOMASSA = 122, #
           lajihavainnon.laatu = 123,
           laji.huomautukset = 124,
           laji.epifyyttinen = 125,
           
           126:133, # LUTU-TYYPPEJ? #
           
           naytteen.numero = 134,
           naytteen.tyyppi = 135,
           naytteen.keraaja = 136,
           naytteen.maarittaja = 137,
           naytteen.maaritys.pvm = 138,
           naytteen.maaritysteos = 139,
           naytteen.sijainti = 140,
           naytteen.museonumero = 141,
           naytteen.URI = 142,
           naytteen.lisatiedot = 143,
           
           144:147, # Hankkeen tietoja, ei kaytossa
           
           hanke.ID = 148) 
} # Rename the variables

filter_df_all <- function(renamed_df) {
  
  renamed_df %>%
    dplyr::select(1:4, # Perustiedot
           9:16, # Koordinaatit
           14:20, # Menetelmät, huomiot
           23:39, # Kartoituskerran tietoja
           47:55, # Videon tiedot
           57, 63:68, # Linjan tietoja
           90:110, # Pohjanlaadut
           117:126, # Lajitiedot & uhanalaiskuvaus
           148) # Hanke 
} # Selects all the variables that should have some info

# Mutate df to display in leaflet properly and eliminate useless variables
edit_kartoittaja_syvyys <- function(data_in){
  data_in %>%
    dplyr::mutate(kartoittaja = ifelse(is.na(sukelluslinjan.kartoittaja), 
                                       videon.analysoija, sukelluslinjan.kartoittaja), .after = epavarma.pohja,
                  syvyys = ifelse(is.na(arviointiruudun.syvyys),
                                  videon.alkusyvyys, arviointiruudun.syvyys))
}


#### OUTDATED 17.3.2021: Returning the desired variables based on the "menetelma"-variable ####

filter_df_linjat_kokooma <- function(renamed_df) {
  renamed_df %>%
    dplyr::filter(kohteen.taso == 62) %>% # Otetaan pelkät kokoomalinjat
    dplyr::select(1:8, # Nimet, koordinaatit
           14:20, # Menetelmätietoja
           23:35, # Päivämäärä, ympäristötietoja
           57:63, # Sukelluslinjan tietoja
           70:79, # Vyöhyketiedot
           148) # Hanke
  } # Pick out the kokoomalinjat from divelines

filter_df_linjat_ruudut <- function(renamed_df) {
  renamed_df %>%
    filter(kohteen.taso == 63) %>%
    select(1:4, # Perustiedot
           9:10, # Koordinaatit
           14:20, # Menetelmät, huomiot
           23:35, # Kartoituskerran tietoja
           57, 63:68, # Linjan tietoja
           90:110, # Pohjanlaadut
           117:126, # Lajitiedot & uhanalaiskuvaus
           148) # Hanke 
} # Selects the right variables for 

filter_df_videot <- function(renamed_df) {
  
  renamed_df %>%
    select(1:4,
           9:16,
           19:20,
           23:39,
           47:55, # Videon tiedot
           90:110, # Pohjanlaadut
           117:125, 148) # Lajitiedot
} # Selects the right variables for videodata

filter_df_pisteet <- function(renamed_df) {
  
  renamed_df %>%
    select(1:4, # Perustiedot
           9:10, # Koordinaatit
           14:20, # Menetelmät, huomiot
           23:35, # Kartoituskerran tietoja
           57, 63:68, # Linjan tietoja
           90:110, # Pohjanlaadut
           117:126, # Lajitiedot & uhanalaiskuvaus
           148) # Hanke 
} # Selects the right variables for point-shaped data

return_correct_df <- function(placeholder_to_user_input) {
  
  # Mutate the raw df with functions above
  dfv <- read_velmu_xl(placeholder_to_user_input) %>%
    mutate_velmu_xl() 
  
  # Assign the right function based on the method:
  if (dfv$kartoitusmenetelma[1] %in% c(42, 43, 44)) {
    df_videot <<- filter_df_videot(dfv)
    
  } else if (dfv$kartoitusmenetelma[1] %in% 21) {
    df_kokooma <<- filter_df_linjat_kokooma(dfv)
    df_ruudut <<- filter_df_linjat_ruudut(dfv)
    
  } else {
    df_pisteet <<- filter_df_pisteet(dfv) 
    
  }

}
