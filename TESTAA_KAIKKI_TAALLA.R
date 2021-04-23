library(tidyverse)

source("fct_menetelmakohtainen_select.R")
source("fct_import_excel.R")
source("fct_tab3_typos_errors.R")
######

aineisto12 <- return_filtered_df("data/testilinjat.xlsx")

linjat %>%
  filter(kohteen.taso == 62) %>%
  count()

linjat %>%
  filter_df_linjat_ruudut() %>%
  mutate(kohde = gsub("\\..*$","", kohteen.nro), .before = kohteen.nro) #Dot followed by any number of any chars

linjat %>%
  filter_df_linjat_ruudut() %>%
  rowwise() %>%
  mutate(kohde = str_sub("[:digit:]*", kohteen.nro), .before = kohteen.nro)


imap_chr(sample(10), ~ paste0(.y, ": ", .x))
iwalk(mtcars, ~ cat(.y, ": ", median(.x), "\n", sep = ""))

iwalk(mtcars, ~ cat(.y, ": ", median(.x), "\n", sep = ""))
iwalk(graphs, ~print({paste0("plot_", .y)}))


fct_5_data <- videot %>% edit_kartoittaja_syvyys()

return_lajilista(videot)
######
dupl <- videot %>%
  mutate_velmu_xl() %>%
  find_duplicated_data()

aineisto %>%
  filter(kohteen.taso == 63) %>%
  select_unimportant_vars() %>%
  na.omit()
  purrr::map_df(~sum(!is.na(.))) %>% # Summataan yhteen NAt
  dplyr::select(where(~ any(. != 0)))

aineisto %>%
  filter(kohteen.taso == 63) %>%
  sum_na_nonessential_vars()

dat %>%
  dplyr::select(kohteen.nro, lajihavainto, lajihavainnon.laatu, lajin.maaran.yksikko, laji.epifyyttinen) %>%
  janitor::get_dupes()

# JATKA TIISTAINA 16.3.2021
# kohdenro <- 1
DetectTyposInCol2 <- function(vdata, kohdenro) {
  dat.t <- vdata[vdata$kohteen.nro == kohdenro, ] %>% dplyr::select(-kohteen.nro) # Filtteröidään kohdenron perusteella.
  unique.row.information <- apply(dat.t, 2, function(x) {length(unique(x))}) # LASKEE sarakkeista uniikkien arvojen määrän.
  potential.typo <- names(unique.row.information[unique.row.information > 1]) # jos Nimiä on enemmän kuin yksi, tulostukseen.
  typos <- paste(potential.typo, collapse = ", ")
  print(paste0("Kohteessa ", kohdenro, " solujen tiedot eroavat seuraavissa sarakkeissa: ", typos)) # Tulostetaan info
}

duplos_kohteet <- unique(dupl$kohteen.nro)
vdata2 <- dupl
vdata <- dupl
kohdenro <- "95"
for (i in 1:length(duplos_kohteet)) {
  print(i)
  print(duplos_kohteet[[i]])
  DetectTyposInCol2(vdata2, duplos_kohteet[[i]])
}

list_dupl <- vector(mode = "list", length = 0)
dupl_teksti_ulos <- for (i in 1:length(duplos_kohteet)) {
  zx <- DetectTyposInCol2(vdata2, duplos_kohteet[[i]])
  c(list_dupl, zx)
  list_dupl
}









# Luodaan uniikeista arvoista vektori
duplos_kohteet <- unique(dupl$kohteen.nro)

vdata2 <- dupl

DetectTyposInCol2 <- function(vdata, kohdenro) {
  dat.t <- vdata[vdata$kohteen.nro == kohdenro, ]
  
  unique.row.infromation <- apply(dat.t, 2, function(x) {length(unique(x))}) # Poimii sarakkeista uniikkien arvojen määrän.
  
  potential.typo <- names(unique.row.infromation[unique.row.infromation > 1])
  print(paste0("Kohteessa ", kohdenro, " solujen tiedot eroavat seuraavissa sarakkeissa: ", potential.typo)) # Tulostetaan info
}


for (i in 1:length(duplos_kohteet)) {
  DetectTyposInCol2(vdata2, duplos_kohteet[[i]])
}


DetectTyposInCol2 <- function(vdata) {
  
  dupl_kohdenrot <- unique(vdata$kohteen.nro)
  
  for (x in (dupl_kohdenrot)){
    print(x)
  }
  
  unique.row.infromation <- apply(dupl_kohdenrot[1], 2, function(x) {length(unique(x))}) # Poimii sarakkeista uniikkien arvojen määrän.
  
  potential.typo <- names(unique.row.infromation[unique.row.infromation > 1])
  print(paste0("Kohteessa ", kohdenro, " solujen tiedot eroavat seuraavissa sarakkeissa: ", potential.typo)) # Tulostetaan info
}

DetectTyposInCol2(vdata2)
vdata <- vdata2
#########

vesikasvirsajat <- read_csv("data/VesikasviSummary.csv")

lajinimet <- read_rds("data/lajit.rds")


join_growth_limits <- function(data_in){
  data_in %>%
    edit_kartoittaja_syvyys() %>% # Mutates surveyor and depth from four into two columns (fct_tab4_lajidata.R) 
    left_join(vesikasvirajat, by = "lajihavainto") %>%
    left_join(lajinimet, by = "lajihavainto") %>%
    select(kohteen.nro, lajihavainto, Tarkennus, syvyys, min_1pros, max_1pros) %>%
    filter(lajihavainto != "Ei lajihavaintoa",
           !Tarkennus %in% c("Tunnistamattomat levämassat", "Pohjaeläimet", "Bakteerit"))
}





ui <- fluidPage(
  tab6_ui_main("counter1")
)

server <- function(input, output, session) {
  tab6LinegraphServer("counter1")
}

shinyApp(ui, server)



linjat <- return_filtered_df("C:/Users/Rasmusbo/OneDrive - Metsahallitus/Data2020/SYKE/Velmu2020_sukelluslinjat_yhdistetty.xlsx") %>%
  mutate_velmu_xl()

zz <- xxx %>% mutate_velmu_xl()

zzz <- zz %>% 
  prepare_for_widepivot %>%
  pivot_data_wider()

prepare_kohde_linjat(test_df) %>%
  select(kohde) %>%
  unique





