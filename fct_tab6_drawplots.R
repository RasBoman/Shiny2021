
# lajinimet löytyy ylätasot

library(tidyverse)
library(ggridges)
library(ggrepel)
library(scales)
library(cowplot)
library(extrafont)
library(plotly)

lajinimet <- read_rds("data/lajit.rds")

source("fct_tab1_import_excel.R")
source("fct_menetelmakohtainen_select.R")
source("fct_tab6b_wrangling.R")

#test_df <- return_filtered_df("C:/Users/Rasmusbo/OneDrive - Metsahallitus/Data2020/SYKE/Velmu2020_sukelluslinjat_yhdistetty.xlsx")
#spec_df <- linja_nest_species(test_df)
#subst_df <- linja_nest_substrates(test_df)
#subst_df_factorized <- factorize_substrates(subst_df)

# For making the plots, the two relevant functions from fct_tab6b_wrangling.R are
# linja_nest_species for species plot and linja_nest_substrates for substrate plot

#### PALETTES could be moved to source file ####

# Paletti lajien visualisointia varten
tarkennus.cols <- length(levels(lajinimet$Tarkennus)) # Create a int of how many colors = how many substrates
tarkennus.colors <- c("#D7301F", "#016C59", "#8c510A", "#EC7014", "#C7E9C0", "#238B45", 
                      "#FEB24C", "#DFC27D" ,"#807DBA", "#807DBA" ,"#969696") #74A9CF, "#35978F"
names(tarkennus.colors) <- c(levels(lajinimet$Tarkennus))
lajitarkennus_scale <- (scale_color_manual(name = "lajitarkennus", values = tarkennus.colors))

# Paletti pohjanlaadun visualisointia varten
#nb.cols <- reactive(length(levels(factor_subst_df()$pohjanlaatu))) # Luodaan int, pohjanlaatujen määrä = värien määrä
#colcodes_for_pohjat <- reactiveVal(1 %>% c(levels(factor_subst_df()$pohjanlaatu)))

#names(colcodes_for_pohjat) <<- reactive(c(levels(factor_subst_df()$pohjanlaatu))) # Nimetään värit pohjanlaatujen mukaan
#pohjalaadut_scale <<- scale_fill_manual(name = "Pohjanlaatu", values = colcodes_for_pohjat()) # Manuaalinen täyttöscale pohjanlaaduille

#### Luodaan df kuvaajaa varten, top lajit, joihin slider ####

top_cover_lajit <- function(luok_lajit){
  luok_lajit %>%
    group_by(etais_luokka) %>%
    arrange(etais_luokka, desc(lajin.peittavyys)) %>%
    slice_max(n = 5, #### change to REACTIVE
              order_by = lajin.peittavyys,
              with_ties = F) %>% # From each group (above) take the 6 species with biggest coverage
    ungroup() %>%
    arrange(desc(lajin.peittavyys)) %>%
    mutate(text.for.plot = paste(lajihavainto, lajin.peittavyys, "%"))
}

# Pohjanlaatukuvaaja
subs_plot <- function(nested_subst_df) {
  nested_subst_df %>%
    ggplot(aes(arviointiruudun.etaisyys, 
               y = fct_rev(pohjanlaatu), 
               height = pohj_peitt, 
               fill = pohjanlaatu)) +
    geom_density_ridges2(stat = "identity", 
                         alpha = 0.9,
                         scale = 1,
                         show.legend = F) +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    theme_minimal() +
    labs(x = "Etäisyys linjalla (m)") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 14, angle = 45, hjust = 0.9, family = "Candara"),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 14, family = "Candara"),
          axis.title.x = element_text(size = 14, family = "Candara")) #+
    #pohjalaadut_scale
}
#### SPECIES PLOT : Depth profile & with species data ####

syv_plot <- function(linja_nest_species, luok_lajit, top_cover_lajit_df){ 
  linja_nest_species %>% 
    ggplot(aes(x = arviointiruudun.etaisyys, 
               y = arviointiruudun.syvyys)) +
    stat_smooth(se = F,
                method = "gam",
                geom = "area",
                alpha = 0.3,
                span = 0.4,
                fill = "#1D91C0") +
    stat_smooth(se = F,
                method = "gam",
                geom = "line",
                alpha = 0.5,
                span = 0.4,
                size = 2) +
    theme_minimal()  +
    labs(y = "Syvyys (m)") +
    geom_jitter(data = luok_lajit, 
                height = 0.15,
                width = 1,
                alpha = 0.6,
                shape = 19,
                aes(x = arviointiruudun.etaisyys, 
                    y = arviointiruudun.syvyys, 
                    size = lajin.peittavyys,
                    color = Tarkennus)) + 
    guides(size = FALSE,
           color = guide_legend(override.aes = list(size = 5))) +
    scale_size_continuous(range = c(2,25)) +
    geom_label_repel(data = top_cover_lajit_df, # lajien_alarajat
                     max.iter = 5000,
                     size = 5.5,
                     nudge_x = 20,
                     nudge_y = 2,
                     segment.size = 0.15,
                     segment.alpha = 0.7,
                     # hjust = -2.5,
                     #  vjust = -1.5,
                     #direction = "x",
                     force = 4,
                     min.segment.length = 1,
                     family = "Candara",
                     aes(label = text.for.plot, 
                         fontface = "italic")) +
    scale_x_continuous(breaks = seq(0, 100, 20),
                       limits = c(min(luok_lajit$arviointiruudun.etaisyys), max(luok_lajit$arviointiruudun.etaisyys)),
                       oob = scales::squish) +
    scale_y_continuous(breaks = seq(ceiling(min(linja_nest_species$arviointiruudun.syvyys)), 0, 1),
                       oob = scales::squish) +
    theme(axis.text.x = element_blank(),
          axis.text = element_text(family = "Candara"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, angle = 45, vjust = 0.5, hjust = 0.5, family = "Candara"),
          axis.text.y = element_text(size = 15, family = "Candara"),
          legend.position = c(0.07, 0.15),
          legend.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          text = element_text(family = "Candara")) +
    lajitarkennus_scale 
}


#### Lajimäärät ####

lajimaaraplot <- function(luok_lajit){
  luok_lajit %>%
  group_by(arviointiruudun.etaisyys) %>%
  count() %>%
  ggplot(aes(x = arviointiruudun.etaisyys, y = n)) +
  geom_bar(stat = "identity", width = 0.5, color = "#238B45", fill = "#016C59", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     limits = c(min(luok_lajit$arviointiruudun.etaisyys), max(luok_lajit$arviointiruudun.etaisyys)),
                     oob = scales::squish) +
  scale_y_continuous(breaks = c(0, 4, 8, 12),
                     limits = c(0, 13)) +
  theme_minimal() +
  labs(y = expression("Lajien lkm / piste")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13, angle = 45, vjust = 0.5, hjust = 0.5, family = "Candara"),
        axis.text.y = element_text(size = 13, angle = 0, vjust = 0.5, hjust = 1),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = "Candara")) 
}


#plot_grid(syv_plot, lajimaaraplot, sedi_plot, subs_plot, 
 #         ncol = 1, 
  #        align = "v",
   #       rel_heights = c(7, 1.3, 0.7, 4))

#ggsave(paste0("C:/Users/Rasmusbo/Documents/2019RaportitYm/", "TESTI", ".jpg"),
#       dpi = 320,
#       height = 15,
#       width = 15)
