library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(janitor)
library(glue)
library(cowplot)
library(ggforce)
library(purrr)
library(png)

df <- tt_load(2021, week = 6)

#### helpers ####
placement <- function(r, d, p) {
  expand_grid(x = seq(0, r, d), y = seq(0, r, d)) %>%
    sample_n(p*n())
}

save_k <- function(loc, filename, format = "png") {
  k <- max(as.numeric(str_extract(list.files(loc), "[:digit:]+")) + 1)
  if(is.infinite(k)) k <- "001"
  glue("{loc}/{filename}-{str_pad(k, 3, 'left', '0')}.{format}")
}

#### colours ####
bg <- "black"
tiles <- "white"
font <- "white"

#### fonts ####
loadfonts()
# font_import("C:/Users/danie/Downloads/Fonts/amatic", prompt = FALSE)
# font_import("C:/Users/danie/Downloads/Fonts/Sarabun", prompt = FALSE)
ft <- "Sarabun ExtraLight"
ftb <- "Sarabun ExtraBold"

#### titles ####
subtitle <-
  "The total number of enrollments has steadily increased from 222k students in 1976 to reaching a maximum number of
  <span style = 'color:#7CCD7C'>323k students</span> in 2010. The proportion of female students ranges from 53% in 1976 to a
  maximum of <b><span style = 'color:#CD5555'>61.8%</span></b> in 2004. The proportion of females has remained
  above 60% since 1997. The area and alpha of the square represents the total number of enrollments and the density represents
  the proportion of female students."

#### plot ####
sc <- 1
df$hbcu_all %>%
  clean_names() %>%
  select(year, males, females) %>%
  pivot_longer(-year, names_to = "gender", values_to = "enrolled") %>%
  group_by(year) %>%
  mutate(
    p = enrolled/sum(enrolled),
    total = sum(enrolled)
    ) %>%
  filter(gender == "females") %>%
  ungroup() %>%
  mutate(
    r = sqrt(total),
    r = r/min(r),
    coords = map2(r, p, ~placement(.x, 0.1, .y)),
    tile_fill = case_when(
      year == 2004 ~ "indianred3", # "#CD5555"
      year == 2010 ~ "palegreen3", # "#7CCD7C"
      TRUE ~ "white"
      ),
    label = paste(round(p, 3)*100, "%")
    ) %>%
  unnest(coords) %>%
  mutate(x = x - r/2) %>%
  right_join(tibble(year = 1976:2015)) %>%
  mutate(year = paste(year, "/", ifelse(is.na(p), "-", paste(round(p, 3)*100, "%")))) %>% {
  ggplot(., aes(x, y, alpha = r)) +
  geom_tile(width = 0.07, height = 0.07, fill = .$tile_fill) +
  # geom_text(data = distinct(., year, label, r), mapping = aes(x = 0, y = -0.15, label = label), colour = tiles, size = 2) +
  facet_wrap(~year, ncol = 5) +
  labs(
    title = "HBCU Enrollments",
    subtitle = subtitle,
    caption = "Source: Data.World / Graphic: @danoehm"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    legend.position = "none",
    plot.margin = margin(t = 0, b = 30, l = 60, r = 60),
    text = element_text(colour = font, family = ft),
    plot.title = element_text(family = ftb, margin = margin(t = 20, b = 0), size = 36, lineheight = 0.8),
    plot.subtitle = element_textbox_simple(family = ft, margin = margin(t = 10, b = 30), size = 12, lineheight = 1),
    plot.caption = element_text(family = ft, margin = margin(t = 10)),
    strip.text = element_text(family = ft, margin = margin(t = 10)),
  ) +
  ggsave(save_k("./2021-week-6/plots", "hbcu"), height = 13.5, width = 7.5)
  }


