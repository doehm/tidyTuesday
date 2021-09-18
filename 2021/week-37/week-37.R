
# week 37 -----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(ggtext)
extrafont::loadfonts(quiet = TRUE)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 37)

# wrangle -----------------------------------------------------------------

results <- tt$results |>
  select(raceId, driverId, positionOrder)

drivers <- tt$drivers |>
  select(driverId, forename, surname)

df <- tt$pit_stops |>
  left_join(results, by = c("raceId", "driverId")) |>
  left_join(drivers, by = "driverId") |>
  filter(milliseconds < 500000) |>
  mutate(hamilton = driverId == 1)

df_hamilton_text = tibble(
  x = 11.25,
  y = c(4, 6),
  label = c("Hamilton", "The rest")
)

x <- c(15, 20, 30, 50, 100)

# palette and fonts -------------------------------------------------------

pal <- c("black", rgb(255, 35, 28, maxColorValue = 255))

ftb <- "Verdana Pro Black"
ft <- "Verdana Pro Light"

# the plot ----------------------------------------------------------------

df |>
  filter(!hamilton) |>
  ggplot(aes(x = log(milliseconds), y = -positionOrder)) +
  geom_point(pch = 92, size = 6, alpha = 0.6) +
  geom_point(aes(x = log(milliseconds), y = -positionOrder), filter(df, hamilton), colour = pal[2], pch = 92, size = 6) +
  geom_text(aes(x, -y, label = label), df_hamilton_text, colour = rev(pal), size = 10, family = ftb, fontface = "italic", hjust = 0) +
  theme_void() +
  theme(
    plot.title = element_markdown(family = ftb, hjust = 0.5, size = 24, face = "italic"),
    axis.title.x = element_text(family = ft, margin = margin(t = 10, b = 5)),
    axis.text = element_text(family = ft, margin = margin(l = 10)),
    legend.position = "none",
    plot.margin = margin(l = 10, r = 10, b = 10, t = 10),
    plot.caption = element_text(family = ft)
  ) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(breaks = c(-1, -5, -10, -15, -20), labels = c("First", "5th", "10th", "15th", "20th")) +
  scale_x_continuous(breaks = log(x*1000), labels = x) +
  labs(
    title = glue("<span style='font-size:100px'>F<span style='color:{pal[2]}'>1</span></span> Faster pit stops may convert to better positions"),
    x = "Pit stop duration (seconds / log scale)",
    caption = "@danoehm"
  ) +
  ggsave("./2021/week-37/week-37.png", height = 8, width = 16)
