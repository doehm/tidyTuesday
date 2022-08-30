# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

library(tidyverse)
library(ggtext)
library(showtext)
library(countrycode)

# load data ---------------------------------------------------------------

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

# fonts and palettes ------------------------------------------------------

spec <- c("#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012")
blue_pink <<- c(lighten("#084B83", 0.3), "#42BFDD", "#BBE6E4", "#F0F6F6", "#FF66B3")
lakes <- c("#788FCE", "#e07a5f", "#f2cc8f", "#81b29a", "#f4f1de")

pal <- sunset
bg <- "grey20"
txt_col <- "white"

font_add_google("Kanit", "kanit")
font_add_google("Cinzel", "cin")
showtext_auto()

ft_text <- "kanit"
ft_title <- "cin"

# wrangle -----------------------------------------------------------------

df_base <- wheels |>
  filter(construction_cost != "Unknown") |>
  select(name, height, hourly_capacity, construction_cost, number_of_cabins, country) |>
  mutate(
    construction_cost = as.numeric(str_extract(construction_cost, "[:digit:]+\\.[:digit:]+|[:digit:]+")),
    continent = countrycode(
      sourcevar = country,
      origin = "country.name",
      destination = "continent"
    ),
    asia = ifelse(continent == "Asia", "Asia", "Other")
  )



# titles ------------------------------------------------------------------

caption <- "Graphic: <img src='C:/Users/Dan/Downloads/fontawesome/twitter-fill.png' height = 10 width = 10> @danoehm / Source: / Code: <img src='C:/Users/Dan/Downloads/fontawesome/github-fill.png' height = 10 width = 10> doehm/tidytuesday #rstats #tidytuesday"
text <- glue(
  "Bigger doesn't necessarily mean higher<br>
  hourly capacity. The biggest Ferris Wheels<br>
  tend to be in <span style='color:#ffd23f';>Asia</span>"
)

# plot --------------------------------------------------------------------

df_base |>
  ggplot(aes(height, hourly_capacity, size = number_of_cabins, colour = asia)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(aes(label = name),
                  filter(df_base, asia == "Asia"), family = ft_text, colour = bright[3], size = 8) +
  annotate("richtext", x = 420, y = 4800, label = text, family = "cin", colour = txt_col, size = 10, lineheight = 0.5,
           fill = NA, label.colour = NA) +
  scale_colour_manual(values = c(bright[3], "grey50")) +
  scale_x_continuous(breaks = seq(100, 600, 100)) +
  scale_size(guide = "none") +
  labs(
    title = "Ferris Wheels",
    y = "Hourly\nCapacity",
    x = "Height"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = txt_col, size = 20),
    axis.text = element_text(margin = margin(t = 5, r = 5)),
    axis.title = element_text(margin = margin(t = 5, r = 5), lineheight = 0.35),
    axis.line = element_line(colour = txt_col),
    plot.title = element_text(family = ft_title, size = 128, hjust = 0.5),
    plot.background = element_rect(fill = bg),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )

ggsave("2022/week32-ferriswheel/ferriswheel.png", height = 6, width = 6)
