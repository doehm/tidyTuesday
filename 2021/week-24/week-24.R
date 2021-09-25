# tidy tuesday week 24

library(tidyverse)
library(janitor)
library(glue)
library(extrafont)
library(ggtext)
library(forcats)
library(ggfx)
extrafont::loadfonts(quiet = TRUE)


# data load ---------------------------------------------------------------

data <- tidytuesdayR::tt_load(2021, week = 24)

df <- data$stocked |>
  clean_names()


# palette -----------------------------------------------------------------

bg <- "#3d405b"
tx <- "#f4f1de"
pal1 <- c("#788FCE", "#e07a5f", "#f2cc8f", "#81b29a", "#f4f1de")


# fonts -------------------------------------------------------------------

ft <- "Gill Sans MT"
ftb <- "Gill Sans Ultra Bold"


# species dot plot --------------------------------------------------------

df |>
  mutate(
    species = fct_lump(species, 7),
    lake = fct_lump(lake, 2),
    ) |>
  filter(
    !is.na(weight),
    !is.na(length),
    species != "Other",
    lake != "Other"
  ) |>
  mutate(
    weight = log(weight),
    length = log(length)
  ) |>
  ggplot(aes(x = species, y = length, colour = species, fill = species)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = 1,
    .width = 0,
    justification = -.3,
    point_colour = NA,
    alpha = 0.7
  ) +
  geom_boxplot(
    width = .25,
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.3,
    alpha = .05,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) +
  facet_wrap(~lake, ncol = 2) +
  scale_colour_manual(values = colorRampPalette(pal1)(7)) +
  scale_fill_manual(values = colorRampPalette(pal1)(7)) +
  coord_flip() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    text = element_text(colour = tx)
  )


# wrangling ---------------------------------------------------------------

text <- tibble(
  x = 7.5,
  y = 0.25,
  species = c("BNT", "CHS", "LAT", "RBT"),
  lake = NA
)

df_wt <- df |>
  filter(lake %in% c("ER", "HU", "MI", "ON", "SU")) |>
  mutate(species = fct_lump(species, 4)) |>
  filter(
    !is.na(weight),
    !is.na(length),
    species != "Other",
    lake != "Other"
  ) |>
  mutate(
    weight = log(weight),
    length = log(length)
  )

df_median <- df_wt |>
  group_by(species, lake) |>
  summarise(
    weight = round(median(weight), 1)
  ) |>
  mutate(
    y = -0.1,
    y_text = as.numeric(as.factor(lake))/10+0.25
    )


# histogram ---------------------------------------------------------------

df_wt |>
  ggplot(aes(x = weight, colour = NA, fill = lake)) +
  as_reference(
    geom_density(alpha = 0.8),
    id = "density"
  ) +
  with_blend(
    geom_text(aes(x+1, y-0.15, label = species), data = text, size = 12, colour = "white", family = ftb),
    bg_layer = "density",
    blend_type = "xor"
  ) +
  geom_point(aes(x = weight, y = y, colour = lake), data = df_median, size = 6) +
  # geom_text(aes(x = weight, y = y, label = as.character(round(exp(weight)))), data = df_median, size = 2, angle = 90, colour = bg) +
  facet_wrap(~species, ncol = 2) +
  scale_colour_manual(values = pal1) +
  scale_fill_manual(values = pal1) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    text = element_text(colour = tx, family = ftb),
    strip.text = element_blank(),
    legend.position = "none",
    plot.title = element_markdown(family = ft, face = "bold", hjust = 0.5, margin = margin(t = 20, l= 150, r = 100)),
    plot.subtitle = element_markdown(family = ft, face = "bold", hjust = 0.5, margin = margin(b = 20, l= 150, r = 100)),
    plot.caption = element_markdown(family = ft, face = "bold"),
    plot.margin = margin(r = 75, b = 10)
  ) +
  labs(
    fill = "Lake",
    title = glue("<span style='color:{pal1[1]}; font-size:32pt'>E R I E </span><span style='color:{pal1[2]}; font-size:32pt'>H U R O N </span><span style='color:{pal1[3]}; font-size:32pt'>M I C H I G A N </span><br><span style='color:{pal1[4]}; font-size:32pt'>O N T A R I O </span><span style='color:{pal1[5]}; font-size:32pt'>S U P E R I O R</span>"),
    subtitle = "Distribution of the weight of fish species (log scale) within the Great Lakes. Fish tend to be the largest in Lake Huron with<br>the exception of Brown Trout in Lake Ontario. Median value shown as the dot underneath the denisty.",
    caption = "Source: Great Lakes Fishery Commission / Graphic: @danoehm"
    ) +
  xlim(c(2.5, 9)) +
  coord_cartesian(clip = "off") +
  ggsave("./2021/week-24/week-24.png", width = 12, heigh = 7)

