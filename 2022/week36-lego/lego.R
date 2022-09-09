# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggchicklet)
library(magick)

# load data ---------------------------------------------------------------

sets <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

# fonts and palettes ------------------------------------------------------

sysfonts::font_add(
  family = "lego",
  regular = "fonts/lego/Legothick.ttf"
)

font_add_google("Oswald", "oswald")
showtext_auto()

pal_lego <- c("#FDFDFD", "#F5EE03", "#FD0000", "#060404")
bg <- pal_lego[3]
ft_txt <- "oswald"

# wrangle -----------------------------------------------------------------

df_top_n_sets <- sets |>
  left_join(
    themes |>
      rename(
        theme_id = id,
        theme_name = name
      ),
    by = "theme_id") |>
  count(theme_name) |>
  slice_max(n, n = 20) |>
  arrange(n) |>
  mutate(
    y = 1:n(),
    theme_name = toupper(theme_name),
    theme_name = fct_reorder(theme_name, n, max)
    )

# titles ------------------------------------------------------------------

# get_icon: my custom function in startup.R
caption <- glue("Graphic: {get_icon('twitter', 15, fill = list(bg = bg, img = txt))} @danoehm / Source: rebrickable / Code: {get_icon('github', 15, fill = list(bg = bg, img = txt))} doehm/tidytuesday #rstats #tidytuesday")
subtitle <-
"The top 20
LEGO themes
with the
most sets"

df_y_lab <- tibble(
  y = seq(0, 2000, 500),
  x = 21
)

# plot --------------------------------------------------------------------

df_top_n_sets |>
  ggplot() +
  geom_chicklet(aes(y, n), fill = "white", colour = pal[2], size = 3, width = 0.75, radius = grid::unit(9, "pt")) +
  geom_chicklet(aes(y, n), fill = "white", colour = "black", size = 1, width = 0.75, radius = grid::unit(9, "pt")) +
  geom_text(aes(y, -100, label = theme_name), family = "lego", size = 36, hjust = 1, colour = "black", fontface = "bold") +
  annotate("rect", xmin = 0, xmax = 10, ymin = 500, ymax = 2150, fill = pal_lego[3]) +
  geom_richtext(aes(x, y, label = y), df_y_lab, size = 16, family = ft_txt, fill = pal_lego[3], label.colour = NA, colour = txt, fontface = "bold") +
  geom_image(aes(x = 8, y = 1500, image = "2022/week36-lego/lego_cropped.png"), asp = 1, size = 0.25) +
  annotate("text", x = 4, y = 1420, label = subtitle, family = ft_txt, colour = txt, lineheight = 0.3, size = 30, fontface = "bold", hjust = 0.5) +
  annotate("richtext", x = -1, y = 0, label = caption, size = 16, family = ft_txt, fill = pal_lego[3], label.colour = NA, colour = txt)+
  scale_y_continuous(limits = c(-1800, 2150), position = "right") +
  labs(x = "Number of sets") +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = "black", size = 10),
    plot.margin = margin(t = 50, b = 20, l = 20, r = 20)
  )

ggsave("2022/week36-lego/lego.png", height = 10, width = 10.5)

image_read("2022/week36-lego/lego.png") |>
  image_fill(point = "+100+100", color = pal_lego[3]) |>
  image_write("2022/week36-lego/lego.png")
