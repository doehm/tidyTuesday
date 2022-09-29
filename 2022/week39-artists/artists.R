# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(janitor)
library(glue)
library(ggtext)
library(forcats)
library(ggbeeswarm)

# load data ---------------------------------------------------------------

artists <- tidytuesdayR::tt_load(2022, 39)$artists

# fonts and palettes ------------------------------------------------------

pal <- c("#7B2CBF", "#953B98", "#AF4A72", "#CA5A4C", "#E46926", "#FF7900")
bright <- c("#540d6e", "#ee4266", "#ffd23f", "#3bceac")
bg <- "white"
txt <- "grey20"

font_add_google("Josefin Sans", "txt")
font_add_google("Shrikhand", "title")
showtext_auto()
ft <- "txt"
ft_title <- "title"

# wrangle -----------------------------------------------------------------

df_mean <- artists |>
  group_by(type) |>
  summarise(n_artists = sum(artists_n, na.rm = TRUE)) |>
  ungroup() |>
  mutate(p_mean = n_artists/sum(n_artists)) |>
  select(type, p_mean)

df_base <- artists |>
  group_by(type, state) |>
  summarise(
    n_artists = sum(artists_n, na.rm = TRUE),
    lq = sum(log(location_quotient)*artists_n/sum(artists_n, na.rm = TRUE), na.rm = TRUE)
    ) |>
  filter(is.finite(lq)) |>
  group_by(state) |>
  mutate(p = n_artists/sum(n_artists)) |>
  ungroup() |>
  mutate(
    lab = type,
    type = as.numeric(fct_reorder(type, lq, median))
    )

# titles ------------------------------------------------------------------

caption <- str_wrap(glue(
  "Graphic: @danoehm /
   Source: arts.gov by way of Data is Plural /
   Code: doehm/tidytuesday #rstats #tidytuesday"), 1000)
subtitle <- str_wrap(
  "The Location quotients (LQ) measure an artist occupation's concentration in the labor force,
  relative to the U.S. labor force share. For example, an LQ of 1.2 indicates that the state's
  labor force in an occupation is 20 percent greater than the occupation's national labor force
  share. An LQ of 0.8 indicates that the state's labor force in an occupation is 20 percent below
  the occupation's national labor force share. California has proportionally more artists in every
  category, paritcularly actors.", 100)

# plot --------------------------------------------------------------------

make_plot <- function(.state, .x) {

  title <- glue("Artists  in <span style='color:{.x}'>{.state}</span>")

  df_state <- df_base |>
    filter(state == .state)

  df_base |>
    ggplot(aes(type, lq)) +
    geom_text(aes(type, -4, label = str_wrap(lab, 25)), df_state, family = ft, size = 20, colour = "grey20", hjust = 0, lineheight = 0.3, vjust = 0.5) +
    geom_beeswarm(size = 4, alpha = 0.5, colour = "grey40") +
    geom_point(aes(x, y), tibble(x = 1:13, y = 0), colour = "grey20", size = 3) +
    geom_point(aes(type, lq), df_state, size = 6, colour = .x) +
    annotate("text", y = -1.2, x = 14, label = "Less than the national share", family = ft, size = 12, colour = "grey20") +
    annotate("text", y = 1.2, x = 14, label = "More than the national share", family = ft, size = 12, colour = "grey20") +
    scale_y_continuous(breaks = log(c(0.1, 0.25, 0.5, 1, 2, 4, 8)), labels = round(c(0.1, 0.25, 0.5, 1, 2, 4, 8), 1)) +
    coord_flip(clip = "off") +
    theme_void() +
    labs(
      y = "Location Quotient (log scale)",
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme(
      text = element_text(family = ft, colour = txt, size = 36),
      plot.background = element_rect(fill = bg),
      plot.margin = margin(t = 30, b = 10, l = 30, r = 30),
      plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 15)),
      plot.title = element_markdown(size = 100, hjust = 0.5, family = ft_title),
      plot.subtitle = element_text(size = 36, hjust = 0.5, family = ft, lineheight = 0.25),
      axis.text.x = element_text(),
      axis.title.x = element_text(margin = margin(t = 10)),
    )

  ggsave(glue("2022/week39-artists/artists-{.state}.png"), height = 12, width = 8.5)

}


# generate plots ----------------------------------------------------------

states <- c("South Dakota", "District of Columbia", "Nevada", "New York")
walk2(states, bright, make_plot)
