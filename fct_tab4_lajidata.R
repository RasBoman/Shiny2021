#'
#' Funktiot liittyen lajidataan 
#' Created by: Rasmus Boman
#' Date: 18.3.2021
#' 
lajinimet <- read_rds("data/lajit.rds")
vesikasvirajat <- read_csv("data/VesikasviSummary.csv")
hertta_lajit_excel <- read_xlsx(path = "data/lajinimet_hertta.xlsx")
hertan_lajinimet <- as.list(hertta_lajit_excel)[[1]] # The äs didn't want to import correctly, thus the extra steps.


# List species occurrence in ascending order
return_lajilista <- function(data_in){
  data_in %>%
    dplyr::filter(kohteen.taso == 63) %>%
    dplyr::select(lajihavainto) %>%
    dplyr::group_by(lajihavainto) %>%
    dplyr::tally(sort = T) %>%
    dplyr::left_join(lajinimet, by = "lajihavainto") %>%
    dplyr::arrange(n)
}

# Compare the species list to official LAJIGIS list
compare_species_to_hertta <- function(data_in){
  data_in %>%
    dplyr::filter(!lajihavainto %in% hertan_lajinimet & !is.na(lajihavainto)) %>%
    dplyr::filter(lajihavainto != "Ei lajihavaintoa",
                  lajihavainto != "Lajia ei listassa") %>%
    dplyr::select(kohteen.nro, kohteen.nimi, lajihavainto)
}

# Lajia ei listassa
return_new_species <- function(data_in){
  data_in %>%
    dplyr::filter(lajihavainto == "Lajia ei listassa") %>%
    dplyr::select(kohteen.nro, kohteen.nimi, lajihavainto, laji.huomautukset)
}

# Leaflet map for species
create_leaflet_species <- function(data_in){
  data_in %>%
    leaflet::leaflet() %>%
    addTiles() %>%
    addCircleMarkers(~ruudun.koordinaatti.E,
                     ~ruudun.koordinaatti.N,
                     popup = paste("Kohteen nimi:", data_in$kohteen.nimi, "<br>",
                                   "Kartoittaja:", data_in$kartoittaja, "<br>",
                                   "Pisteen syvyys:", data_in$syvyys),
                     color = "coral4",
                     radius = 7,
                     fillOpacity = 0.8)
}

# Joins the original table with species growth limits 
join_growth_limits <- function(data_in){
  data_in %>%
    edit_kartoittaja_syvyys() %>% # Mutates surveyor and depth from four into two columns (fct_tab4_lajidata.R) 
    left_join(vesikasvirajat, by = "lajihavainto") %>%
    left_join(lajinimet, by = "lajihavainto") %>%
    dplyr::select(kohteen.nro, lajihavainto, Tarkennus, syvyys, min_1pros, max_1pros) 
}

# Filter only rows which are over / below the last 98% of observations
filter_based_on_growth_limits <- function(data_in){
  data_in %>%
    join_growth_limits() %>%
    dplyr::filter(!str_detect(Tarkennus, "Tunnistamattomat|Kalat|Pohjael|Bakteerit")) %>% #Äs are a pain in the ass for some reasons
    dplyr::filter(lajihavainto != "Ei lajihavaintoa") %>%
    dplyr::mutate(outlier = case_when(syvyys < min_1pros ~ "Matala",
                                      syvyys > max_1pros ~ "Syva",
                                      TRUE ~ "NA")) %>%
    dplyr::filter(outlier != "NA")
     #      Tarkennus == "Kalat")
}
