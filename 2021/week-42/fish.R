
# week 42 -----------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(janitor)
library(magick)
library(scales)
library(glue)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 42)

df <- tt$`capture-fisheries-vs-aquaculture` |>
  clean_names() |>
  set_names("entity", "code", "year", "aqua", "capture") |>
  inner_join(
    countrycode::codelist %>%
      select(code = genc3c, continent) %>%
      distinct() %>%
      filter(
        !is.na(code),
        !is.na(continent)
      ),
    by = "code")

dat <- df |>
  group_by(entity) |>
  mutate(first_aqua = aqua > capture) |>
  group_by(year) |>
  summarise(p = mean(first_aqua, na.rm = TRUE)) |>
  mutate(year_max = year + 1)

increase_df <- df |>
  filter(year %in% c(1960, 2018)) |>
  group_by(year) |>
  summarise(
    aqua = sum(aqua, na.rm = TRUE),
    capture = sum(capture, na.rm = TRUE)
    ) |>
  mutate_all(~paste0(round(.x/lag(.x))*100, "%")) |>
  slice(2)

sustainability <- tt$`fish-stocks-within-sustainable-levels` |>
  clean_names() |>
  set_names("entity", "code", "year", "sustainable", "overfished") |>
  filter(entity == "World") |>
  mutate(
    p = 0,
    label = paste0(round(sustainable), "%"),
    sustainable = sustainable/100
    )

year_labs <- tibble(
  year = c(1974, 2017),
  y = 0.95,
  p = 0
)

legend_text <- glue("More <strong><span style='color:{pal[1]}'>green</span></strong>, more countries where <strong>aquaculture</strong> production exceeds <strong>capture</strong>. Each band represents a year.")
title_text <- glue("<strong>Aquaculture and Sustainability</strong>")
subtitle_text <- glue("Even though more and more countries adopt aquaculture the percentage of<br>sustainably sourced
                      fish stock is decreasing.<br><br>From 1960 to 2018 <strong>capture</strong> stock production
                      increased <strong>{increase_df$capture}</strong> while <strong>aquaculture</strong><br>production increased <strong>{increase_df$aqua}</strong>.
                      Meanwhile the percentage of sustainably sourced<br>stock has dropped from <strong>91%</strong> in 1978 to <strong>66%</strong> in 2017 across the globe. The demand<br>
                      for seafood is so great, farming can't keep up.")

# fontsa nd palettes ------------------------------------------------------

download.file("https://github.com/doehm/evoPalette/raw/master/inst/extdata/palettes.rds", destfile = "./2021/week-42/palettes.rds")
palettes <- readRDS("./2021/week-42/palettes.rds")

pal <- palettes$palette[[4]]
show_col(pal)

font_add_google("Mukta", "muk")
showtext_auto()

# plots -------------------------------------------------------------------

dat |>
  ggplot(aes(year, 1, fill = p)) +
  geom_rect(aes(xmin = year, xmax = year_max, ymin = 0, ymax = 1, fill = p)) +
  geom_line(aes(year, sustainable), sustainability, size = 2) +
  geom_segment(aes(x = 2017, xend = 2017, y = 0.66, yend = 0.92), size = 0.5, linetype = 2) +
  geom_point(aes(year, sustainable), sustainability, size = 14) +
  geom_text(aes(year, sustainable, label = label), sustainability, size = 14, colour = pal[5], fontface = "bold", family = "muk") +
  geom_text(aes(year, y, label = year), year_labs, size = 20, fontface = "bold", family = "muk") +
  geom_text(aes(year, 0.03, label = round(p, 2)), slice(dat, 3, 15, 30, 45, 57), family = "muk", fontface = "bold", size = 16) +
  geom_richtext(aes(x = 1970, y = 0.08, label = legend_text), family = "muk", fill = NA, size = 16, label.color = NA, hjust = 0) +
  geom_richtext(aes(x = 1967, y = 0.52, label = title_text), family = "muk", fill = NA, size = 48, label.color = NA, hjust = 0) +
  geom_richtext(aes(x = 1967, y = 0.35, label = subtitle_text), family = "muk", fill = NA, size = 16, label.color = NA, hjust = 0, lineheight = 0.4) +
  scale_fill_gradientn(colours = rev(pal)) +
  labs(caption = "#TidyTuesday week 41 2021 / Data: OurWorldinData.org / Graphic: @danoehm") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    plot.caption = element_text(family = "muk", colour = "white", size = 24, margin = margin(b = 7), hjust = 0.95)
  ) +
  coord_cartesian(clip = "off") +
  ggsave("./2021/week-42/fish.png", height = 8, width = 16)
