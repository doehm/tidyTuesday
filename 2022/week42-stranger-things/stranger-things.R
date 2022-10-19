# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(tidytext)
library(ggfx)
library(emojifont)
library(colorspace)

# load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 42)

# fonts and palettes ------------------------------------------------------

pal <- c("#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012")
txt <- "white"
txt1 <- "red"
txt2 <- darken(txt1, 0.25)
bg <- "black"

font_add_google("Heebo", "heebo")
font_add_google("Libre Baskerville", "lb")
showtext_auto()
ft <- "heebo"
ft1 <- "lb"

# wrangle -------------------------------------------------------------

df_base <- dat$stranger_things_all_dialogue |>
  filter(
    # season == 1,
    !is.na(dialogue)
    ) |>
  mutate(duration = end_time - start_time) |>
  unnest_tokens(word, dialogue) |>
  left_join(get_sentiments("afinn"), by = "word") |>
  group_by(season, episode) |>
  mutate(
    value = replace_na(value, 0),
    value = cumsum(value),
    x = 1:n()/n(),
    positive = value > 0
    )

df_labels <- tibble(
  season = 1:4,
  episode = 5,
  x = 0.5,
  y = 140,
  label = paste("Season", 1:4)
)

# titles ------------------------------------------------------------------

twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt2}'>{emojifont::fontawesome('fa-twitter')}</span>")
github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt2}'>{emojifont::fontawesome('fa-github')}</span>")
caption <- str_wrap(glue(
  "Graphic: {twitter} @danoehm /
  Source: 8flix.com /
  Code: {github} doehm/tidytuesday #rstats #tidytuesday"),
  1000)

# plot --------------------------------------------------------------------

g_base <- df_base |>
  ggplot(aes(x, value)) +
  geom_hline(yintercept = 0, colour = txt1, linetype = 2) +
  with_blur(geom_smooth(se = FALSE, colour = txt2), sigma = 4) +
  geom_smooth(se = FALSE, colour = txt, size = 0.2) +
  geom_ribbon(aes(x = x, ymin = 0, ymax = value, fill = positive), alpha = 0.25) +
  geom_text(aes(x, y, label = label), df_labels, family = ft1, colour = txt2, size = 36) +
  facet_grid(season ~ episode, switch = "x") +
  scale_fill_manual(values = c(txt1, txt)) +
  scale_colour_manual(values = c(txt1, txt)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "<img src='2022/week42-stranger-things/stranger-things-title.jpg' height = 400>",
    caption = caption,
    x = "Episode\n\nCumulative sentiment score for each episode. Season 4 has the most\nnegative sentiment and is the darkest of the series"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 64, colour = txt2),
    plot.background = element_rect(fill = bg),
    plot.title = element_markdown(hjust = 0.5, margin = margin(b=5)),
    plot.subtitle = element_text(hjust = 0.5, lineheight = 0.3, margin = margin(t = 0, b = 50)),
    plot.margin = margin(t = 0, b = 20, l = 50, r = 50),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 30, b = 20)),
    strip.text.x = element_text(),
    axis.title.x = element_text(margin = margin(t = 10), lineheight = 0.3),
    legend.position = "none"
  )

ggsave("2022/week42-stranger-things/stranger-things.png", plot = g_base, height = 16, width = 12)

