library(magick)
library(imager)
library(tidyverse)
library(scales)
library(grid)
library(crayon)
library(glue)
library(cowplot)
library(ggforce)
library(survivoR)
library(extrafont)
library(showtext)

font_add("coke", "C:/Users/Dan/Downloads/Fonts/loki_cola/LOKICOLA.ttf")
showtext_auto()

coke_bottle <- load.image("C:/Users/Dan/Pictures/R/coke.jpeg")
plot(coke_bottle)

font_import("C:/Users/Dan/Downloads/Fonts/loki_cola")

# plastics data
# Get the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics

plastics %>%
  mutate_if(is.character, tolower) %>%
  filter(
    !parent_company %in% c("null", "unbranded", "grand total"),
    !country %in% c("empty")
  ) %>%
  group_by(country) %>%
  mutate(n_brands = n()) %>%
  arrange(desc(n_brands)) %>%
  group_b



bottle <- record_coords("bottle")
cap <- record_coords("cap")
label <- record_coords("label")

df <- bottle %>%
  bind_rows(cap) %>%
  bind_rows(label) %>%
  mutate(
    x = scale_coords(x),
    y = scale_coords(y)
  )

coke_dark <- rgb(9, 16, 12, maxColorValue = 255)
coke_light <- rgb(140, 30, 13, maxColorValue = 255)

coke_ramp <- colorRampPalette(c(coke_light, coke_dark))(100)

coke %>%
  ggplot(aes(x = x, y = y)) +
  geom_bspline_closed0(
    data = dplyr::filter(coke, name == "bottle"),
    mapping = aes(x = x, y = y),
    alpha = 1, fill = coke_ramp[50]
  ) +
  geom_bspline_closed(data = filter(coke, name == "cap"), mapping = aes(x = x, y = y), colour = "black", fill = "red") +
  geom_polygon(data = filter(coke, name == "label"), mapping = aes(x = x, y = y), colour = "black", fill = "red") +
  geom_text(x = 0.5, y = 0.54, label = "America", family = "Loki Cola", size = 18, colour = "white") +
  theme_void()

n <- 2e4
shape <- 100
alpha <- runif(n, 0, 3)
x <- runif(n)
y <- alpha
bubble_colour <- c(rev(coke_ramp), coke_ramp)

bubbles <- tibble(
  alpha = alpha,
  x = x,
  y = y
)

ggplot() +
  geom_point(data = bubbles, mapping = aes(x, y, colour = x), size = 5, alpha = 0.6) +
  geom_rect(aes(xmin = 0-wd/2, xmax = 1+wd/2, ymin = 0, ymax = 3.2), fill = "grey50", alpha = 0.2, colour = "black") +
  geom_shape(
    data = glass, aes(x = x, y = y), colour = "grey20", fill = "grey90",
    expand = unit(0, 'mm'), radius = unit(3, 'mm')
  ) +
  scale_colour_gradientn(colours = bubble_colour) +
  theme_void() +
  theme(
    legend.position = "none"
  )

wd <- 0.07
base_wd <- 0.1
glass <- tribble(
  ~x, ~y,
  0.1, 0.1+base_wd,
  0, 0.2+base_wd,
  0, 3.2,
  -wd, 3.2,
  -wd, -wd,
  1+wd, -wd,
  1+wd, 3.2,
  1, 3.2,
  1, 0.2+base_wd,
  0.9, 0.1+base_wd
)

ggplot() +
  geom_shape(
    data = glass, aes(x = x, y = y), colour = "#8ecae6", fill = "#8ecae6", alpha = 0.05,
    expand = unit(0, 'mm'), radius = unit(3.2, 'mm'), size = 1
  ) +
  theme_void()

