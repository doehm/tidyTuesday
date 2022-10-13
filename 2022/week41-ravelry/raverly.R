# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(janitor)
library(glue)
library(ggtext)
library(ggforce)
library(ggfx)
library(emojifont)

# load data ---------------------------------------------------------------

yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

# fonts and palettes ------------------------------------------------------

pal <- tribble(
  ~r, ~g, ~b,
  198, 114, 67,
  220, 166, 146,
  180, 185, 163,
  129, 160, 149,
  30, 166, 179
) |>
  mutate(pal = rgb(r, g, b, maxColorValue = 255)) |>
  pull(pal)
pal <- colorRampPalette(pal)(8)

txt <- "grey20"
bg <- "white"

font_add_google("Fasthand", "fh")
font_add_google("Karla", "karla")
showtext_auto()
ft <- "fh"
ft1 <- "karla"

# wrangle -----------------------------------------------------------------

df_base <- yarn |>
  group_by(yarn_company_name) |>
  summarise(
    rating = mean(rating_average, na.rm = TRUE),
    n = n()
    ) |>
  arrange(desc(n)) |>
  head(8) |>
  ungroup() |>
  mutate(
    p = n/sum(n),
    x0 = 1:n()
    )

# yarn
df_yarn <- map_dfr(1:nrow(df_base), ~{
  tibble(
    x = 3*df_base$p[.x]*sin(seq(0, 2*pi, length = 200)) + df_base$x0[.x],
    y = 3*df_base$p[.x]*cos(seq(0, 2*pi, length = 200)) + df_base$rating[.x] - 3*df_base$p[.x],
    id = runif(200),
    yarn_company_name = df_base$yarn_company_name[.x]
    )
  }) |>
  mutate(y = -y) |>
  arrange(yarn_company_name, id)

df_yarn |>
  filter(yarn_company_name == "Katia") |>
  ggplot(aes(x, y)) +
  with_blur(geom_bspline0(), sigma = 2)

# titles ------------------------------------------------------------------

twitter <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt}'>{emojifont::fontawesome('fa-twitter')}</span>")
github <- glue("<span style='font-family:\"fontawesome-webfont\";color:{txt}'>{emojifont::fontawesome('fa-github')}</span>")
caption <- str_wrap(glue(
  "Graphic: {twitter} @danoehm /
  Source: ravelry /
  Code: {github} doehm/tidytuesday #rstats #tidytuesday"),
  1000)

# plot --------------------------------------------------------------------

bg <- "#fefae0"
wood <- "#d4a373"

df_yarn |>
  ggplot(aes(x, y)) +
  geom_rect(aes(xmin = 0.5, xmax = 8.5, ymin = -1.2, ymax = -1), fill = lighten(wood, 0.2)) +
  with_blur(geom_segment(aes(x = x0, xend = x0, y = -1, yend = -rating+0.2, colour = yarn_company_name), df_base, size = 0.6), sigma = 3) +
  with_blur(geom_bspline0(aes(colour = yarn_company_name), size = 0.6), sigma = 3) +
  geom_text(aes(x0, -rating-0.3, label = paste0(yarn_company_name, "\n", round(rating, 1), " | ", n)), df_base, family = ft, size = 24, colour = txt, lineheight = 0.3) +
  coord_cartesian(clip = "off") +
  scale_colour_manual(
    values = pal,
    breaks = df_base$yarn_company_name
  ) +
  scale_x_continuous(
    breaks = 1:8,
    labels = df_base$yarn_company_name
  ) +
  ylim(-5, -1) +
  labs(
    title = "Yarn Company Ratings",
    subtitle = "Top 8 companies with the most yarn\nRating | Number of threads",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 64, colour = txt),
    plot.title = element_text(size = 240, hjust = 0.5),
    plot.subtitle = element_text(size = 80, hjust = 0.5, lineheight = 0.25, family = ft1),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(margin = margin(t = 20, b = 40), hjust = 0.5, family = ft1),
    legend.position = "none",
    plot.margin = margin(t = 60, b = 0, l = 60, r = 60)
  )

ggsave("2022/week41-ravelry/ravelry.png", height = 12, width = 16)
