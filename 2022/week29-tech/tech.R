# https://github.com/rfordatascience/tidytuesday

# source("scripts/startup.R")

library(ggchicklet)
library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)
library(glue)

# functions ---------------------------------------------------------------

choose_font_colour <- function(bg, dark = "#000000", light = "white") {
  x <- drop(c(0.299, 0.587, 0.114) %*% col2rgb(bg) > 186)
  if(x) {
    dark
  } else {
    light
  }
}

cg <- function(text, colour = NULL) {
  if(is.null(colour)) {
    colour <- pal[names(pal) == text]
  }
  glue("<span style='color:{colour}'>{text}</span>")
}

# load data ---------------------------------------------------------------

technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

# fonts and palettes ------------------------------------------------------

pal <- c("grey10", "grey30", "#c9ecb4", "#93d3ab", "#35b0ab")
names(pal) <- c("Coal", "Gas", "Hydro", "Solar", "Wind")
bg <- "white"
txt_col <- "grey20"

font_add_google("Kanit", "kan")
ft_text <- "kan"
ft_title <- ft_text
showtext_auto()

# wrangle -----------------------------------------------------------------

df_base <-
  technology |>
  filter(
    str_detect(variable, "elec_[:alpha:]"),
    str_detect(variable, "coal|gas|hydro|wind|solar"),
    year %in% c(2000, 2020),
    iso3c == "AUS",
    # removing solar and wind for 2000 because you can't see it on
    # the chart and the chicklet leaves an annoying artifact
    !(str_detect(variable, "wind|solar") & year == 2000)
    ) |>
  group_by(year, variable) |>
  summarise(value = sum(value)) |>
  mutate(
    energy = str_to_title(str_extract(variable, "(?<=elec_)[:alpha:]+")),
    energy = factor(energy)
  )

df_point <-
  df_base |>
  arrange(year, energy) |>
  group_by(year) |>
  mutate(
    y = cumsum(value) - value/2,
    pct = scales::percent(round(value/sum(value), 3)),
    txt_col = map_chr(energy, ~choose_font_colour(pal[.x], dark = txt_col))
    )

# titles ------------------------------------------------------------------

title <- "Australia's Electricity Production"
subtitle <- glue(
  "Electricity production has increased by ~47TWH between 2000<br>
  and 2020. In 2000 {cg('wind', pal[5])} and {cg('solar', pal[4])} contribution was negligible,<br>
  in 2020 it accounts for 18%. Coal electricity production has declined<br>
  by ~28TWH but (unfortunately) is still over half of Australia's<br>
  electricity production in 2020."
  )
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: data.nber.org / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday")

# plot --------------------------------------------------------------------

df_base |>
  ggplot(aes(year, value, fill = energy)) +

  # geoms
  geom_chicklet(colour = bg, width = 14, radius = grid::unit(9, "pt")) +
  geom_text(aes(year, y, label = pct), df_point, family = ft_text, size = 24, colour = df_point$txt_col, fontface = "bold") +

  # theme and scales and labs
  scale_fill_manual(
    breaks = c("Coal", "Gas", "Hydro", "Solar", "Wind"),
    values = pal
  ) +
  scale_x_continuous(
    breaks = c(2000, 2020),
    labels = c(2000, 2020)
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = "Electricity\nproduction\nfrom...",
    y = "Electricity production (TWH)"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 86, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_markdown(hjust = 0, family = ft_text, size = 48, lineheight = 0.35, margin = margin(b = 20)),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, lineheight = 0.25),
    legend.text = element_text(family = ft_text),
    axis.text = element_text(),
    axis.title.y = element_text(angle = 90, margin = margin(r = 10))
  )

# save --------------------------------------------------------------------

ggsave("2022/week29-tech/tech.png", height = 12, width = 8)
