# Functions to create boxplots
library(tidyverse)

#### A NON-WORKING MESS 16.3.2021 #### -Rasmus

# Define the colnames that we want to plot either in video or regular

var_boxplotit <- c("veden.lampotila",	"lampotilan.mittaussyvyys",	"secchi.syvyys", "tuulen.voimakkuus", 
                   "videon.syvyyden.korjaus", "videon.alkusyvyys", "videon.loppusyvyys",
                   "sukelluslinjan.syvyyden.korjaus", "arviointiruudun.syvyys", "arviointiruudun.etaisyys", 
                   "pohjanlaadut.yhteensa", "lajin.peittavyys", "lajin.lukumaara", "lajin.korkeus")

# Make sure the data is properly named, not a problem in the App
aineisto <- read_velmu_xl("data/testilinjat.xlsx") %>%
  mutate_velmu_xl()

# Remove empty cols as we don't want to plot these
tt <- aineisto %>%
  filter(kohteen.taso == 63) %>%
  select(any_of(var_boxplotit)) %>%
  janitor::remove_empty("cols")

# Create the ggplot function
create_boxplot <- function(variable) {
  tt %>%
  ggplot(aes(x = .data[[variable]])) + # Double curly brackets for colnames
      geom_boxplot() +
    theme_minimal_grid()
}

#Create histograms
create_hist <- function(variable) {
  tt %>%
    ggplot(aes(x = .data[[variable]])) +
             geom_histogram()
}

map(colnames(tt), create_hist)
# Map through the columns that remain
purrr::map(colnames(tt), create_boxplot)
purrr::map(colnames(tt), create_hist)

tt %>%
  map(create_boxplot(v))
  ggplot(aes(x = veden.lampotila)) +
  geom_boxplot()

map(create_boxplot, tt, variable)

map(draw_these, create_boxplot(tt, draw_these))




melt(mtcars) %>%
  split(.$variable) %>%
  map(., ~ggplot(.x, aes(x=value)) + 
        geom_histogram())


draw_these <- colnames(testeste)

sdd <- set_names(draw_these)

create_boxplot(tt, draw_these[1])

map(vars, ~ggplot(data = mpg) +
      geom_point(aes(x = cyl, y = .data[[.x]]) ) +
      labs(y = .x)