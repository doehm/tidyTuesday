# week 37 -----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
library(ggfx)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 39)

nom <- tt$nominees |>
  mutate(distributor = case_when(
    str_detect(distributor, "CBS") ~ "CBS",
    str_detect(distributor, "National Geo") ~ "National Geographic",
    str_detect(distributor, "Disney") ~ "Disney",
    str_detect(distributor, "NBC") ~ "NBC",
    str_detect(distributor, "HBO") ~ "HBO",
    TRUE ~ distributor)
  )

top_distributors <- nom |>
  count(distributor) |>
  slice_max(n, n = 48, with_ties = FALSE) |>
  pull(distributor)

df <- expand_grid(distributor = top_distributors, type = c("Winner", "Nominee")) |>
  left_join(nom, by = c("distributor", "type")) |>
  count(distributor, type)

# fonts and palettes ------------------------------------------------------

font_add_google("Bebas Neue", "bebas")
showtext_auto()

gold <- rgb(243, 175, 20, maxColorValue = 255)
brown <- rgb(124, 95, 56, maxColorValue = 255)

# text --------------------------------------------------------------------

title_text <- "<img src='./2021/week-39/emmys-logo.png'><span style='color:#7C5F38; font-size:200px'>N O M I N E E S</span><span style='font-size:200px'> • </span><span style='color:#F3AF14; font-size:200px'>W I N N E R S</span><img src='./2021/week-39/emmys-logo-r.png'>"

subtitle_text <- "<span>The proportion of Emmy nominees and winners are shown for the 48 largest distributors.<br>
    <span style='color:#7C5F38'>HBO</span> have the most nominations and winners with <span style='color:#F3AF14'>4442</span> nominations but also the highest<br>proportion of winners with <span style='color:#F3AF14'>30%</span> taking the gong. They really do punch out some good stuff.<br>
<span style='color:#7C5F38'>Adult Swim</span> and <span style='color:#7C5F38'>CNN</span> both have their proporiton of wins being <span style='color:#F3AF14'>>50%</span>."

# plot --------------------------------------------------------------------

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
  mutate(distributor = fct_reorder(distributor, xmax, min)) |> View()
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type)) +
  geom_text(aes(x = x_text, y = y_text, label = distributor), size = 18, family = "bebas", fontface = "bold", lineheight = 0.3) +
  scale_fill_manual(values = c(gold, brown)) +
  facet_wrap(~distributor, nrow = 8) +
  labs(
    title = title_text,
    subtitle = subtitle_text
  ) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = "bebas", face = "bold", hjust = 0.5, margin = margin(t = 50, b = 20)),
    plot.subtitle = element_markdown(size = 48, family = "bebas", hjust = 0.5, lineheight = 0.3, margin = margin(b = 20)),
    plot.background = element_rect(fill = "grey20"),
    plot.margin = margin(l = 20, b = 20, r = 20),
    legend.position = "none",
    strip.text = element_blank()
  ) +
  ggsave("./2021/week-39/emmys.png", height = 12, width = 8)


tt$nominees |>
  filter(str_detect(distributor, "Adult")) |>
  count(title, sort = TRUE) |>
  View()
