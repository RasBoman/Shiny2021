source("fct_menetelmakohtainen_select.R")
source("fct_tab1_import_excel.R")

#### Duplikaatit koko taulukossa ####

# Kullakin hierarkian pisteellä lajitietoja lukuunottamatta tietojen tulisi olla identtisiä.
prepare_for_widepivot <- function(dataframe1) {
  dataframe1 %>%
    dplyr::mutate(across(where(is.character), str_trim)) %>%
    dplyr::filter(kohteen.taso == 63) %>% # Ei kokoomarivejä
    dplyr::mutate(kartoittaja = ifelse(is.na(sukelluslinjan.kartoittaja), videon.analysoija, sukelluslinjan.kartoittaja), .after = epavarma.pohja) %>%
    dplyr::mutate(peit_tai_lkm = ifelse(is.na(lajin.peittavyys), lajin.lukumaara, lajin.peittavyys)) %>% # Siirretään kartoittajat ja peittävyydet yhteen sarakkeeseen
    dplyr::select(-c(lajin.peittavyys, lajin.lukumaara, lajin.maaran.yksikko, # Poistetaan lajikohtaiset muuttujat
                     lajin.korkeus, laji.epifyyttinen, lajihavainnon.laatu, laji.huomautukset)) %>%
    dplyr::select(-c(avoimuusindeksi, kohteen.huomautukset, secchi.syvyys, kartoituskerran.huomautukset))
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

# Anynomous function to find and display duplicated rows
find_duplos <- function(dat)dat[dat$kohteen.nro %in% dat$kohteen.nro[duplicated(dat$kohteen.nro)], ]

# Bring it all together in a pipe and turn into function:
find_duplicated_data <- function(dataframe3){
  dataframe3 %>%
    filter_df_all() %>% # From fct_import_excel.R
    prepare_for_widepivot() %>%
    pivot_data_wider() %>%
    find_duplos() %>%
    dplyr::arrange(desc(kohteen.nro))
}

# LAJIGIS pakolliset tiedot sarakkeissa 
testaa_pakolliset_sarakkeet <- function(data_to_use){
  data_to_use %>%
    filter_df_all() %>%
    dplyr::filter(kohteen.taso == 63) %>%
    dplyr::select(kohteen.nro, kohteen.taso, kohteen.nimi, kartoituspvm, ruudun.koordinaatti.E, ruudun.koordinaatti.N) %>% # Pakolliset sarakkeet
    purrr::map_df(~sum(is.na(.))) %>% # Summataan yhteen NAt
    dplyr::select(where(~ any(. != 0)))
}

# Display plot if these > 0 rows. dat should be reactive_df()
show_if_not_empty_fun <- function(dat_in){
  if(length(dat_in) != 0){
    dat_in
  }
}

# Duplikaattilajit
filter_dupl_lajit <- function(dat){
  dat %>%
    dplyr::select(kohteen.nro, lajihavainto, lajihavainnon.laatu, lajin.maaran.yksikko, laji.epifyyttinen) %>%
    janitor::get_dupes()
}

#### Tulosta duplikaattien sarakkeiden nimet ####

# TÄMÄ EI TOIMI 17.3.2021, näyttää vääriä arvoja!!
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


select_unimportant_vars <- function(renamed_df) {
  
  renamed_df %>%
    dplyr::select(-c(1:4, # Perustiedot
                  9:16, # Koordinaatit
                  14:20, # Menetelmät, huomiot
                  23:39, # Kartoituskerran tietoja
                  47:55, # Videon tiedot
                  57, 63:68, # Linjan tietoja
                  90:110, # Pohjanlaadut
                  117:126, # Lajitiedot & uhanalaiskuvaus
                  148)) # Hanke 
}

sum_na_nonessential_vars <- function(data_in) {
  data_in %>%
    filter(kohteen.taso == 63) %>%
    select_unimportant_vars() %>%
    purrr::map_df(~sum(!is.na(.))) %>% # Summataan yhteen NAt
    dplyr::select(where(~ any(. != 0)))
}

