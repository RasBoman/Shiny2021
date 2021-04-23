#
#' Helper functions for leaflet map on tab5
#' By: Rasmus Boman
#' Date created: 22.3.2021
#xxx <- videot
library(leaflet.extras)
#mutate_for_map(xxx)
# Sequencing the depths to display on leaflet. At the time of writing you can't directly color
# the circles with a variable and a factor has to be created:
depth_seq <- function(data_in){
  
    data_in$depth_levels <- cut(data_in$syvyys, 
                                c(-2, 1, 2, 3, 4, 6, 8, 10, 12, 100), 
                                include.lowest = T,
                                labels = c('< 1 m', '1-2 m', '2-3 m', 
                                           '3-4 m','4-6 m', '6-8 m', '8-10 m',
                                           '10-12 m', '12+ m'))
    return(data_in)
}

# Small mutations to wrangle the data for the map:
mutate_for_map <- function(data_in){
  data_in %>%
    filter_df_all() %>%
    dplyr::filter(kohteen.taso == 63) %>%
    edit_kartoittaja_syvyys() %>% # fct_menetelmakohtainen_select.R
    depth_seq() %>% # Function above
    dplyr::select(kohteen.nro, kohteen.nimi, syvyys, depth_levels, kartoittaja, 
                  ruudun.koordinaatti.N, ruudun.koordinaatti.E, lajihavainto) %>%
    group_by(across(c(-"lajihavainto"))) %>%
    summarise("lajisto" = toString(lajihavainto),
              "lajimaara" = n()) %>%
    ungroup()
}

# Create a leaflet map with depth as legend and variable
leaflet_depth_map <- function(data_in){
  
  syvyysCol <- colorFactor(palette = "YlOrRd", data_in$depth_levels, reverse = T)
  
  data_in %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = data_in, lat = ~ruudun.koordinaatti.N, lng = ~ruudun.koordinaatti.E,
                     color = ~syvyysCol(depth_levels),
                     opacity = 0.9, fillOpacity = 0.9, 
                     radius = ~ceiling(sqrt(lajimaara)*3),
                     popup = paste(
                       "Kohteen nimi:", data_in$kohteen.nimi, "<br>",
                       "Kartoittaja:", data_in$kartoittaja, "<br>", 
                       "Ruudun syvyys:", data_in$syvyys, "<br>",
                       "Lajimäärä:", data_in$lajimaara, "<br>",
                       "Lajisto:" , data_in$lajisto)) %>%
    addLegend('bottomright', 
             pal = syvyysCol, 
              values = data_in$depth_levels,
              title = 'Syvyys',
              opacity = 1) %>%
    addDrawToolbar(editOptions = editToolbarOptions(),
                   polylineOptions = FALSE,
                   markerOptions = FALSE,
                   circleOptions = FALSE,
                   circleMarkerOptions = FALSE)

}

#leaflet_depth_map(videot)      
