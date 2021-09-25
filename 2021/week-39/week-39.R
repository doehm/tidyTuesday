# week 37 -----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(showtext)
library(cowplot)
library(ggimage)
library(magick)
library(ggforce)
library(ggfx)
# extrafont::loadfonts(quiet = TRUE)


# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 39)
tt

sick_palettes <- list(
  bj = c("#f4f1de", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f"),
  sunset = c("#ffcdb2", "#ffb4a2", "#e5989b", "#b5838d", "#6d6875")
)

nom <- tt$nominees

top_distributors <- nom |>
  mutate(distributor = case_when(
    str_detect(distributor, "CBS") ~ "CBS",
    str_detect(distributor, "National Geo") ~ "National Geographic",
    str_detect(distributor, "Disney") ~ "Disney",
    str_detect(distributor, "NBC") ~ "NBC",
    # str_detect(distributor, )
    TRUE ~ distributor
  )) |>
  count(distributor) |>
  arrange(desc(n)) |>
  slice(1:48) |>
  pull(distributor)

df <- expand_grid(distributor = top_distributors, type = c("Winner", "Nominee")) |>
  left_join(nom, by = c("distributor", "type")) |>
  count(distributor, type)

# the good ----------------------------------------------------------------

font_add_google("Cinzel Decorative")
font_add_google("Kaisei Decol")
font_add_google("Bonheur Royale")
font_add(family = "deco",
         regular = "C:/Users/Dan/Documents/fonts/Cinzel_Decorative/CinzelDecorative-Regular.ttf",
         bold = "C:/Users/Dan/Documents/fonts/Cinzel_Decorative/CinzelDecorative-Black.ttf"
         )
font_add_google("Stick No Bills")
font_add_google("Gochi Hand", "gochi")
font_add_google("Bebas Neue", "bebas")
showtext_auto()
showtext_end()

gold <- rgb(243, 175, 20, maxColorValue = 255)
brown <- rgb(124, 95, 56, maxColorValue = 255)

df |>
  group_by(distributor) |>
  mutate(
    ratio = n/sum(n),
    xmin = ifelse(type == "Nominee", ratio, 0),
    xmax = ifelse(type != "Nominee", 1-ratio, 1),
    ymin = 0,
    ymax = 1,
    x_text = 0.5,
    y_text = 0.5,
    distributor = str_wrap(distributor, 8)
  ) |>
  ungroup() |>
  mutate(distributor = fct_reorder(distributor, ratio, max)) |>
  # View()
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type)) +
  geom_text(aes(x = x_text, y = y_text, label = distributor), size = 18, family = "bebas", fontface = "bold", lineheight = 0.3) +
  scale_fill_manual(values = c(gold, brown)) +
  facet_wrap(~distributor, nrow = 8) +
  labs(
    title = "<img src='./2021/week-39/emmys-logo.png'><span style='color:#7C5F38; font-size:200px'>N O M I N E E S</span><span style='font-size:200px'>     •     </span><span style='color:#F3AF14; font-size:200px'>W I N N E R S</span><img src='./2021/week-39/emmys-logo-r.png'>",
    subtitle = "<span>The proportion of Emmy nominees and winners are shown for the 6 largest distributors.<br>
    <span style='color:#7C5F38'>HBO</span> have the most nominations and winners with <span style='color:#F3AF14'>4442</span> nominations but also the highest<br>proportion of winners with <span style='color:#F3AF14'>30%</span> taking the gong.
    They really do punch out some good stuff"
  ) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "bebas", face = "bold", hjust = 0.5, margin = margin(t = 50, b = 20)),
    plot.subtitle = element_markdown(size = 48, family = "bebas", hjust = 0.5, lineheight = 0.3, margin = margin(b = 20)),
    legend.position = "none",
    plot.background = element_rect(fill = "grey20"),
    strip.text = element_blank(),
    plot.margin = margin(l = 20, b = 20, r = 20)
  ) +
  ggsave("./2021/week-39/emmy.png", height = 12, width = 8)

