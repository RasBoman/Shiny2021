source("fct_menetelmakohtainen_select.R")
source("fct_tab1_import_excel.R")

# Tämä vaikuttaisi olevan ok 15.4.2021 -Rasmus

#### Duplikaatit koko taulukossa ####

# Kullakin hierarkian pisteellä lajitietoja lukuunottamatta tietojen tulisi olla identtisiä.
# Valmistellaan taulukko leveäksi kääntöä varten
prepare_for_widepivot <- function(dataframe1) {
  dataframe1 %>%
    dplyr::mutate(across(where(is.character), str_trim)) %>%
    dplyr::filter(kohteen.taso == 63) %>% # Ei kokoomarivejä
    dplyr::mutate(kartoittaja = ifelse(is.na(sukelluslinjan.kartoittaja), videon.analysoija, sukelluslinjan.kartoittaja), .after = epavarma.pohja) %>% # Luodaan vain yksi sarake kartoittajille
    dplyr::mutate(peit_tai_lkm = ifelse(is.na(lajin.peittavyys), lajin.lukumaara, lajin.peittavyys)) %>% # Siirretään peittävyydet yhteen sarakkeeseen
    dplyr::select(-c(lajin.peittavyys, lajin.lukumaara, lajin.maaran.yksikko, # Poistetaan lajikohtaiset muuttujat
                     lajin.korkeus, laji.epifyyttinen, lajihavainnon.laatu, laji.huomautukset))  %>%
    dplyr::select(-c(roskat.koodisto, roskat.kpl, havainnon.tarkistustarve)) %>%
    dplyr::select(-c(avoimuusindeksi, kohteen.huomautukset, secchi.syvyys, kartoituskerran.huomautukset)) %>%
    dplyr::select(-(LAJIN.BIOMASSA:hanke.ID))
}

# Pivot data, duplicates will show up
pivot_data_wider <- function(dataframe2){
  dataframe2 %>%
    tidyr::pivot_wider(names_from = lajihavainto, 
                       values_from = peit_tai_lkm,
                       values_fill = 0,
                       values_fn = sum) %>%
    janitor::remove_empty("cols") %>%
    dplyr::select(kohteen.nro:kartoittaja)
}

# Duplicate coordinates, names and numbers

dupl_nr_koords <- function(data_in){
  
  data_in <- data_in %>% filter(kohteen.taso == 63)
  
  unique_values <- data_in %>% 
    select(kohteen.nro, ruudun.koordinaatti.N, ruudun.koordinaatti.E) %>%
    unique() 

  duplicated_nros <- unique_values %>%
    select(kohteen.nro) %>%
    duplicated()
  
  duplicated_nros_rev <- unique_values %>%
    select(kohteen.nro) %>%
    duplicated(fromLast = T)

  bind_rows(unique_values[duplicated_nros, ], unique_values[duplicated_nros_rev, ]) %>%
    arrange(kohteen.nro)

}

dupl_name_koords <- function(data_in){
  
  data_in <- data_in %>% filter(kohteen.taso == 63)
  
  unique_values <- data_in %>% 
    select(kohteen.nimi, ruudun.koordinaatti.N, ruudun.koordinaatti.E) %>%
    unique() 
  
  duplicated_names <- unique_values %>% 
    select(kohteen.nimi) %>%
    duplicated()
  
  duplicated_names_rev <- unique_values %>% 
    select(kohteen.nimi) %>%
    duplicated(fromLast = T)
  
  bind_rows(unique_values[duplicated_names, ], unique_values[duplicated_names_rev, ]) %>%
    arrange(kohteen.nimi)
}

dupl_koords <- function(data_in){
  
  data_in <- data_in %>% filter(kohteen.taso == 63)
  
  unique_values <- data_in %>% 
    select(kohteen.nro, kohteen.nimi, ruudun.koordinaatti.N, ruudun.koordinaatti.E) %>%
    unique() 
  
  duplicated_koords <- unique_values %>% 
    select(ruudun.koordinaatti.N, ruudun.koordinaatti.E) %>%
    duplicated()
  
  duplicated_koords_rev <- unique_values %>% 
    select(ruudun.koordinaatti.N, ruudun.koordinaatti.E) %>%
    duplicated(fromLast = T)
  
  bind_rows(unique_values[duplicated_koords, ], unique_values[duplicated_koords_rev, ]) %>%
    arrange(kohteen.nro)
}

# Anynomous function to find and display duplicated rows
find_duplos <- function(dat)dat[dat$kohteen.nro %in% dat$kohteen.nro[duplicated(dat$kohteen.nro)], ]

# Bring it all together in a pipe and turn into function:
find_duplicated_data <- function(dataframe3){
  dataframe3 %>%
    #filter_df_all() %>% # From fct_import_excel.R
    prepare_for_widepivot() %>%
    pivot_data_wider() %>%
    find_duplos() %>%
    dplyr::arrange(desc(kohteen.nro))
}

# LAJIGIS pakolliset tiedot sarakkeissa 
testaa_pakolliset_sarakkeet <- function(data_to_use){
  data_to_use %>%
    filter_df_all() %>%
    edit_kartoittaja_syvyys() %>% # fct_menetelmakohtainen_select.R
    dplyr::select(kohteen.nro, kohteen.taso, kartoituksen.tarkoitus, kohteen.nimi, kartoituspvm, kartoittaja) %>% # Pakolliset sarakkeet
    purrr::map_df(~sum(is.na(.))) %>% # Summataan yhteen NAt
    dplyr::select(where(~ any(. != 0)))
}

# Display plot if these > 0 rows. dat should be reactive_df()
show_if_not_empty_fun <- function(dat_in){
  if (nrow(dat_in) != 0) {
    dat_in
  } else {
    #shiny::showNotification("No data", type = "error")
    NULL
  }
}

# Duplikaattilajit
filter_dupl_lajit <- function(dat){
  dat %>%
    dplyr::select(kohteen.nro, lajihavainto, lajihavainnon.laatu, lajin.maaran.yksikko, laji.epifyyttinen) %>%
    janitor::get_dupes()
}

#### Tulosta duplikaattien sarakkeiden nimet ####

# TÄMÄ EI TOIMI SHINYSSÄ 15.4.2021, näyttää vääriä arvoja!!
DetectTyposInCol2 <- function(vdata, kohdenro) {
  dat.t <- vdata[vdata$kohteen.nro == kohdenro, ] %>% dplyr::select(-kohteen.nro) # Filtteröidään kohdenron perusteella.
  unique.row.information <- apply(dat.t, 2, function(x) {length(unique(x))}) # LASKEE sarakkeista uniikkien arvojen määrän.
  potential.typo <- names(unique.row.information[unique.row.information > 1]) # jos Nimiä on enemmän kuin yksi, tulostukseen.
  typos <- paste(potential.typo, collapse = ", ")
  print(paste0("Kohteessa ", kohdenro, " solujen tiedot eroavat seuraavissa sarakkeissa: ", typos)) # Tulostetaan info
}



# Note 12.3.2021 use transform to chr -> numeric then test if there are NA values left! 
# transform turns them automatically into NAs.


# And also compare these:
#length(unique(dat$kohteen.nro))
#length(unique(dat$kohteen.nimi))

#### Categorical variables ####

unique_cat_vars <- function(data_to_use){
  cat_var <- data_to_use %>%
    dplyr::filter(kohteen.taso == 63) %>%
    dplyr::select(kohteen.taso,
                  kartoituksen.tarkoitus, 
                  kartoitusmenetelma, 
                  kartoitusmenetelman.tarkennus, 
                  runsausarvioinnin.menetelma,
                  otantamenetelma, 
                  peittavyyden.arviointi,
                  kenttahenkilot, 
                  vene, 
                  levakukinta, 
                  tuulen.suunta, 
                  sedimentin.maara, 
                  sukelluslinjan.kartoittaja,
                  arviointiruudun.pinta.ala, 
                  lajihavainnon.laatu,
                  hanke.ID)
  
  cate_vari <- map(cat_var, unique) 
  print(cate_vari)
}

#### Unimportant variables that shouldn't have data #### 

# Valitaan päinvastaiset sarakkeet kuin muualla

select_unimportant_vars <- function(renamed_df) {
  
  renamed_df %>%
    dplyr::select(-c(1:4, # Perustiedot
                     9:16, # Koordinaatit
                     14:20, # Menetelmät, huomiot
                     23:39, # Kartoituskerran tietoja
                     47:55, # Videon tiedot
                     57, 63:68, # Linjan tietoja
                     90:110, # Pohjanlaadut
                     116:126, # Lajitiedot & uhanalaiskuvaus
                     148)) # Hanke 
}

# Poimitaan täältä jos sarakkeissa on arvoja niin mukaan
sum_na_nonessential_vars <- function(data_in) {
  data_in %>%
    filter(kohteen.taso == 63) %>%
    select_unimportant_vars() %>%
    purrr::map_df(~sum(!is.na(.))) %>% # Summataan yhteen NAt
    dplyr::select(where(~ any(. != 0)))
}

