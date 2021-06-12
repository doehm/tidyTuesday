# week 13
library(tidyverse)
library(janitor)
library(glue)
library(snakecase)
library(lubridate)
library(extrafont)
library(cowplot)
library(ggforce)
extrafont::loadfonts(quiet = TRUE)

font_import("")

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 14)
tt

tt$allShades %>%
  count(brand, sort = TRUE)

tt$allShades %>%
  filter(hue < 50) %>% {
  # filter(brand == "Clinique") %>% {
    ggplot(., aes(x = hue, y = lightness)) +
    geom_point(colour = .$hex, size = 2)
  }

a <- 0.9
b <- 0.45
df <- tt$allShades %>%
  filter(hue < 50) %>%
  group_by(brand) %>%
  summarise(
    n = n(),
    hue = sd(hue),
    sat = sd(sat),
    lightness = sd(lightness)
  ) %>%
  filter(n >= 30) %>%
  arrange(desc(hue)) %>%
  ungroup() %>%
  mutate(
    huec = floor(hue/max(hue)*20),
    lightc = floor(lightness/max(lightness)*20),
    huec_end = huec + a,
    lightc_end = lightc + a
  )

n <- df %>%
  count(huec, lightc)

# x <- tt$allShades %>%
#   filter(hue < 50) %>%
#   filter(brand == "Maybelline")
#
# kmeans(x[,c("hue", "sat", "lightness")], 6)$centers

#### colours ####
bg <- "white"
font <- "grey10"

#### fonts ####
ft <- "Amatic"
ftc <- "Amatic SC"

#### palettes ####


#### plot ####
df %>%
  ggplot(aes(x = huec, y = lightc)) +
  geom_rect(aes(xmin = huec, xmax = huec_end, ymin = lightc, ymax = lightc_end), fill = NA, colour = font) +
  geom_text(data = n, mapping = aes(huec+b, lightc+b, label = n), family = ft, size = 8, colour = font) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = font),
    plot.title = element_text(family = ft, size = 36),
    plot.background = element_rect(fill = bg),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
  ) +
  labs(
    title = "Variation of Hue and Lightness in Foundation Colour Palettes"
  ) +
  ggsave("./2021/week-14/make-up.png")

