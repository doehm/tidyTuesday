# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

library(tidyverse)
library(ggtext)
library(geomtextpath)
library(patchwork)

# load data ---------------------------------------------------------------

frog <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frog.csv') |>
  clean_names()

# fonts and palettes ------------------------------------------------------

forest_pal <- c("#445544", "#779977", "#99AA77", "#CCCC88", "#BBBB88")
water_pal <- c("#244157", "#227F9A", "#43A4BF", "#88B2C7")
sunset <- c("#ffcdb2", "#ffb4a2", "#e5989b", "#b5838d", "#6d6875")
bg <- "white"
txt_col <- "grey20"

font_add_google("Barlow Condensed", "barlow")
font_add_google("Karla", "karla")
showtext_auto()

ft_text <- "karla"
ft_title <- "barlow"

# titles ------------------------------------------------------------------

title <- "Oregon Spotted Frog"
subtitle <- str_rich_wrap("Where will you find them?", 100)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: USGS.gov @FGazzelloni  / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

df_annotate <- tribble(
  ~x, ~y, ~label,
  1, 0.85, "Water depth",
  1, 0.5, "Water type",
  1, 0.15, "Structure"
)

# plot --------------------------------------------------------------------

g_type <- frog |>
  time_log() |>
  count(type) |>
  arrange(desc(type)) |>
  mutate(
    x = 1,
    x0 = c(3, 2, 3, 2) + 0.5,
    x1 = c(3, NA, 3, NA),
    y = cumsum(n) - n/2,
    label = paste0(n, "\n", type),
    pct = paste0(round(n/sum(n)*100,0), "%")
    ) |>
  ggplot() +
  geom_col(aes(x, n, fill = type)) +
  geom_textpath(aes(x0, y, label = type), family = ft_text, colour = txt_col, text_only = TRUE, angle = 90, size = 10, lineheight = 0.25) +
  geom_text(aes(0, y, label = pct), family = ft_text, colour = txt_col, text_only = TRUE, size = 8, lineheight = 0.25) +
  geom_segment(aes(x = x, xend = x1-0.5, y = y, yend = y)) +
  geom_point(aes(x, y), size = 2) +
  scale_fill_manual(values = water_pal) +
  xlim(c(-2, 3.5)) +
  coord_polar("y") +
  theme_void() +
  theme(legend.position = "none")

g_water <- frog |>
  time_log() |>
  count(water) |>
  arrange(desc(water)) |>
  mutate(
    x = 1,
    x0 = c(3, 2, 3, 2) + 0.5,
    x1 = c(3, NA, 3, NA),
    y = cumsum(n) - n/2,
    label = paste0(n, "\n", water),
    pct = paste0(round(n/sum(n)*100,0), "%")
  ) |>
  ggplot() +
  geom_col(aes(x, n, fill = water)) +
  geom_textpath(aes(x0, y, label = water), family = ft_text, colour = txt_col, text_only = TRUE, angle = 90, size = 10, lineheight = 0.25) +
  geom_text(aes(0, y, label = pct), family = ft_text, colour = txt_col, text_only = TRUE, size = 8, lineheight = 0.25) +
  geom_segment(aes(x = x, xend = x1-0.5, y = y, yend = y)) +
  geom_point(aes(x, y), size = 2) +
  scale_fill_manual(values = rev(sunset)[1:4]) +
  xlim(c(-2, 3.5)) +
  coord_polar("y") +
  theme_void() +
  theme(
    legend.position = "none"
  )

g_structure <- frog |>
  time_log() |>
  count(structure) |>
  arrange(desc(structure)) |>
  mutate(
    x = 1,
    x0 = c(2, 3, 2, 3, 3) + 0.5,
    x1 = c(NA, 3, NA, 3, 3),
    y = cumsum(n) - n/2,
    label = paste0(n, "\n", structure),
    pct = paste0(round(n/sum(n)*100,0), "%")
  ) |>
  ggplot() +
  geom_col(aes(x, n, fill = structure)) +
  geom_textpath(aes(x0, y, label = structure), family = ft_text, colour = txt_col, text_only = TRUE, angle = 90, size = 10, lineheight = 0.25) +
  geom_text(aes(0, y, label = pct), family = ft_text, colour = txt_col, text_only = TRUE, size = 8, lineheight = 0.25) +
  geom_segment(aes(x = x, xend = x1-0.5, y = y, yend = y)) +
  geom_point(aes(x, y), size = 2) +
  scale_fill_manual(values = forest[1:5]) +
  xlim(c(-2, 3.5)) +
  coord_polar("y") +
  theme_void() +
  theme(
    legend.position = "none"
  )

g_base <- ggplot() +
  geom_text(aes(x, y, label = label), df_annotate, family = "karla", size = 26, colour = txt_col, hjust = 1, lineheight = 0.35) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, size = 36, colour = txt_col),
    plot.title = element_text(size = 120, hjust = 0.5, face = "bold", family = ft_title),
    plot.subtitle = element_text(size = 48, hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5),
    plot.margin = margin(t = 20, b = 10, r = 20)
  )

g_base +
  inset_element(g_type, left = -0.05, right = 0.8, top = 0.68, bottom = 0.31) +
  inset_element(g_water, left = -0.05, right = 0.8, top = 1, bottom = 0.62) +
  inset_element(g_structure, left = -0.05, right = 0.8, top = 0.37, bottom = 0)

ggsave("2022/week31-frog/frog.png", height = 12, width = 7.5)

