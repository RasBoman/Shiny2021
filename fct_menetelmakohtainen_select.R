
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

# Rename the variables

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
    dplyr::filter(kohteen.taso == 63) %>%
    dplyr::select(1:4, # Perustiedot
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
