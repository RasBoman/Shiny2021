
# lajinimet löytyy ylätasot

library(tidyverse)
library(ggridges)
library(ggrepel)
library(scales)
library(cowplot)
library(extrafont)

lajinimet <- read_rds("data/lajit.rds")

source("fct_tab1_import_excel.R")
source("fct_menetelmakohtainen_select.R")
source("fct_tab6b_wrangling.R")

test_df <- return_filtered_df("C:/Users/Rasmusbo/OneDrive - Metsahallitus/Data2020/SYKE/Velmu2020_sukelluslinjat_yhdistetty.xlsx")
spdf <- linja_nest_species(test_df)



# Pohjanlaadun faktorointi graafia varten
linja_nest_substrates$pohjanlaatu <- factor(linja_nest_substrates$pohjanlaatu, 
                                    levels = c("kallio", "lohkareab", "lohkarebb", "lohkarecb", "glasiaalisavi", "kivias", 
                                               "kivibs", "sora", "hiekka", "siltti", "savi", "muta", "konkreetiot", 
                                               "hiekkakivi", "keinotekoinen.alusta", "turve", "puun.rungot"),
                                    labels = c("Kallio", "Lohkare > 3m", "Lohkare 1.2-3 m", "Lohkare 0.6-1.2 m", "Glasiaalisavi", "Kivi 10-60 cm", 
                                               "Kivi 6-10 cm", "Sora", "Hiekka", "Siltti", "Savi", "Muta", "Konkreetiot", 
                                               "Hiekkakivi", "Keinotekoinen alusta", "Turve", "Puun rungot"))

#### PALETTES could be moved to source file ####
# Paletti pohjanlaadun visualisointia varten

colcodes_for_pohjat <- c("#525252", "#00564C", "#0D726A", "#2E9088", "#969696", #Kallio, lohkare, glasiaalisavi
                         "#2171B5", "#4292C6", "#CFA155", "#E1C685", "#F0DEB1", # Kivet -> Siltti
                         "#774408", "#543005", "#F0F0F0", "#F5ECD5", "#252525", 
                         "#E5F5E0", "#A1D99B") # Puunrungot, turva


nb.cols <- length(levels(linja_nest_substrates$pohjanlaatu)) # Luodaan int, pohjanlaatujen määrä = värien määrä
mycolors <- colcodes_for_pohjat # Aiemmin määritety värikoodit
names(mycolors) <- c(levels(linja_nest_substrates$pohjanlaatu)) # Nimetään värit pohjanlaatujen mukaan
pohjalaadut_scale <- scale_fill_manual(name = "Pohjanlaatu", values = mycolors) # Manuaalinen täyttöscale pohjanlaaduille
scales::show_col(mycolors) # Näyttää väripaletin

tarkennus.cols <- length(levels(lajinimet$Tarkennus)) # Create a int of how many colors = how many substrates
tarkennus.colors <- c("#D7301F", "#016C59", "#8c510A", "#EC7014", "#C7E9C0", "#238B45", "#FEB24C", "#DFC27D" ,"#807DBA", "#807DBA" ,"#969696") #74A9CF, "#35978F"
names(tarkennus.colors) <- c(levels(lajinimet$Tarkennus))
scales::show_col(tarkennus.colors)
lajitarkennus_scale <- (scale_color_manual(name = "lajitarkennus", values = tarkennus.colors))

# Lajitellaan linja viiteen segmenttiin etäisyyden perusteella
lines_sep <- seq(from = min(linja_nest_species$arviointiruudun.etaisyys),
                 to = max(linja_nest_species$arviointiruudun.etaisyys),
                 by = (max(linja_nest_species$arviointiruudun.etaisyys) / 5))

#### Erotellaan lajidata ####

# Lajitellaan lajit etäisyysluokkiin
luok_lajit <- linja_nest_species %>% 
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

alueen.secchi <- linja_nest_substrates$secchi.syvyys[1]

# Luodaan df kuvaajaa varten, top lajit, joihin slider
top_cover_lajit <- luok_lajit %>%
  filter(Tarkennus != inputATnaytapohjeliot) %>% #### REACTIVE 
  group_by(etais_luokka) %>%
  arrange(etais_luokka, desc(lajin.peittavyys)) %>%
  slice_max(n = inputATlajimaara, #### REACTIVE
            order_by = lajin.peittavyys,
            with_ties = F) %>% # From each group (above) take the 6 species with biggest coverage
  ungroup() %>%
  arrange(desc(lajin.peittavyys)) %>%
  mutate(text.for.plot = paste(lajihavainto, lajin.peittavyys, "%"))

# Pohjanlaatukuvaaja
subs_plot <- linja_nest_substrates %>%
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
  theme(axis.title.y= element_blank(),
        axis.text.y = element_text(size = 14, angle = 45, hjust = 0.9, family = "Candara"),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 14, family = "Candara"),
        axis.title.x = element_text(size = 14, family = "Candara")) +
  pohjalaadut_scale

#### SPECIES PLOT : Depth profile & with species data ####

syv_plot <- linja_nest_species %>% 
  ggplot(aes(x = arviointiruudun.etaisyys, 
             y = arviointiruudun.syvyys)) +
  stat_smooth(se = F,
              method = "gam",
              geom = "area",
              alpha = 0.3,
              span = 0.5,
              fill = "#1D91C0") +
  stat_smooth(se = F,
              method = "gam",
              geom = "line",
              alpha = 0.5,
              span = 0.5,
              size = 2) +
  geom_hline(yintercept = (alueen.secchi), linetype = "dashed", color = "black", alpha = 0.7, size = 1) +
  theme_minimal()  +
  labs(y = "Syvyys (m)") +
  geom_jitter(data = luok_lajit, 
              height = 0.15,
              width = 2,
              alpha = 0.6,
              shape = 19,
              aes(x = arviointiruudun.etaisyys, 
                  y = arviointiruudun.syvyys, 
                  size = lajin.peittavyys,
                  color = Tarkennus)) + 
  guides(size=FALSE,
         color = guide_legend(override.aes = list(size = 5))) +
  scale_size_continuous(range = c(2,25)) +
  geom_label_repel(data = top_cover_lajit, # lajien_alarajat
                   max.iter = 5000,
                   size = 4.5,
                   nudge_x = 25,
                   nudge_y = 3,
                   segment.size = 0.15,
                   segment.alpha = 0.7,
                  # hjust = -2.5,
                 #  vjust = -1.5,
                   #direction = "x",
                   force = 5,
                   min.segment.length = 1,
                   family = "Candara",
                   aes(label = text.for.plot, 
                       fontface = "italic")) +
  geom_label(x = 0,
             y = alueen.secchi,
             family = "Candara",
             size = 5,
             hjust = 0,
             alpha = 0.9,
             fill = "#F5ECD5",
             aes(label = paste("Secchi-syvyys:", -alueen.secchi, "m"),
                 alpha = 0.4)) +
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
        axis.title.y = element_text(size = 14, angle = 45, vjust = 0.5, hjust = 0.5, family = "Candara"),
        axis.text.y = element_text(size = 13, family = "Candara"),
        legend.position = c(0.07, 0.15),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        text = element_text(family = "Candara")) +
  lajitarkennus_scale 
syv_plot

#### SEDIMENT PLOT ####

sedi_plot <- linja_nest_species %>%
  select(arviointiruudun.etaisyys, sedimentin.maara) %>%
  distinct() %>%
  ggplot(aes(x = arviointiruudun.etaisyys, y = sedimentin.maara)) +
  #geom_segment(aes(xend = arviointiruudun.etaisyys, yend=sedimentin.maara), color="black") +
  geom_bar(stat = "identity", width = 0.25) +
  geom_point( color="orange", size=4) +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     limits = c(min(luok_lajit$arviointiruudun.etaisyys), max(luok_lajit$arviointiruudun.etaisyys)),
                     oob = scales::squish) +
  scale_y_continuous(breaks = c(0, 1, 2, 3), limits = c(-0.2,3.2)) +
  theme_minimal() +
  labs(y = expression("Sedimentin määrä")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13, angle = 45, vjust = 0.5, hjust = 0.5, family = "Candara"),
        axis.text.y = element_text(size = 13, angle = 0, vjust = 0.5, hjust = 1),
        axis.text.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(family = "Candara")) 

#### Lajimäärät ####

lajimaaraplot <- luok_lajit %>%
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

plot_grid(syv_plot, lajimaaraplot, sedi_plot, subs_plot, 
          ncol = 1, 
          align = "v",
          rel_heights = c(7, 1.3, 0.7, 4))

#ggsave(paste0("C:/Users/Rasmusbo/Documents/2019RaportitYm/", "TESTI", ".jpg"),
#       dpi = 320,
#       height = 15,
#       width = 15)
