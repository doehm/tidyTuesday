
# week 2 ------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(showtext)
library(broom)
library(geojsonio)
library(rgdal)
library(rgeos)

# load data ---------------------------------------------------------------

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# fonts and palettes ------------------------------------------------------

honey_pal <- c("#E3D7C1", "#C8B188", "#C4952E", "#BE7C22", "#93500C")

font_add_google("Shadows Into Light", "shadow")
font_add_google("Karla", "karla")
showtext_auto()

ft_text <- "shadow"
ft_text2 <- "karla"

# wrangle -----------------------------------------------------------------

# hex map
# file from https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data |>
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# create hex base
df_base <- spdf_fortified |>
  left_join(
    colony |>
      filter(
        state != "United States",
        year == "2020"
      ) |>
      group_by(state) |>
      summarise(colony_lost_pct = mean(colony_lost_pct, na.rm = TRUE)),
    by = c("id" = "state")
  ) |>
  mutate(colony_lost_pct = replace_na(colony_lost_pct, 0))

# basic way to scale the stress percentage to 1-5 score
# so states have a score relative to each other
stars <- function(x) {
  a <- (x - min(x)) / (max(x) - min(x))
  floor(a*4.99) + 1
}

# get scores for each state
stressors_risk <- stressor |>
  group_by(state, stressor) |>
  summarise(stress_pct = mean(stress_pct, na.rm = TRUE)) |>
  group_by(stressor) |>
  mutate(
    stars = stars(stress_pct),
    stressor = ifelse(stressor == "Disesases", "Diseases", stressor)
  )

# expand to give coordinate for each score dot
vars <- c("Diseases", "Pesticides", "Other pests/parasites", "Varroa mites")
y_adj <- c(0.2, -0.2, -0.6, -1)
df_scores <- map2_dfr(vars, y_adj, ~{
  stressors_risk |>
    filter(stressor == .x) |>
    mutate(x_adj = purrr::map(stars, function(star) {
      seq(1, star, 1)/2
    }),
    y_adj = .y
    ) |>
    unnest(x_adj)
  }) |>
  left_join(tibble(state = state.name, state_abb = state.abb), by = "state") |>
  left_join(centers, by = c("state_abb" = "id")) |>
  ungroup()

# in chart labels
df_labs <- tibble(
    stressor = c("DSE", "PST", "OPP", "VRM"),
    x_lab_adj = -1.1,
    y_lab_adj = y_adj,
    join_id = 1
  ) |>
  right_join(
    df_scores |>
      distinct(state, x, y) |>
      mutate(join_id = 1),
    by = "join_id"
  )

# make legend
x_legend <- -91

df_legend <- tribble(
  ~x, ~y, ~stressor,
  x_legend, 53, "Disease (DSE)",
  x_legend, 52.5, "Pesticide (PST)",
  x_legend, 52, "Other pests / parasites (OPP)",
  x_legend, 51.5, "Varroa Mites (VRM)"
)

df_legend_dots <- tibble(
  x = rep(x_legend + 1, 7) + c(0, 1, 2, 0, 1, 0, 0)*0.5,
  y = c(53, 53, 53, 52.5, 52.5, 52, 51.5)
)


# titles ------------------------------------------------------------------

title <- ""
subtitle <- ""
caption <- "Graphic: @danoehm / Source: / #rstats #tidytuesday"
fill <- ""
colour <- ""

# plot --------------------------------------------------------------------

ggplot() +

  # hex map
  geom_polygon(aes(x = long, y = lat, group = group, fill = colony_lost_pct), df_base, color="black", size = 2) +
  geom_polygon(aes(x = long, y = lat, group = group), df_base, color = honey_pal[1], size = 1, fill = NA) +
  geom_text(aes(x=x, y=y+1, label = id), centers, family = ft_text2, size = 24, fontface = "bold") +

  # scores
  geom_text(aes(x = x + x_lab_adj, y = y + y_lab_adj, label = stressor), df_labs, family = ft_text2, size = 11, fontface = "bold") +
  geom_point(aes(x + x_adj - 0.6, y + y_adj), df_scores, size = 2.8, pch = 15) +

  # legend
  geom_text(aes(x=x, y=y, label=stressor), df_legend, family = ft_text2, size = 12, fontface = "bold", hjust = 1) +
  geom_point(aes(x, y), data = df_legend_dots, pch = 15, size = 2.8) +
  annotate(
    "text", x = x_legend-29, y = 53.15, family = ft_text2, fontface = "bold", size = 13, lineheight = 0.3, hjust = 0, vjust = 1,
    label = str_wrap("Bee colonies are affected by various stressors. The severity score ranges from 1-5, less severe to most severe. States with a more severe score are at greater risk of loss.", 50)) +

  # theme and scales
  theme_void() +
  coord_map() +
  scale_fill_gradientn(colours = c(honey_pal, colorspace::darken(honey_pal[5], 0.2))) +
  labs(
    title = "Save the Bees",
    subtitle = str_wrap("The busy little bees are very important to us and they are losing their colonies. These are the areas of the US that were affected the most in 2020.", 100),
    fill = str_wrap("% of bee colonies that have been lost in 2020", 14)
  ) +
  theme(
    plot.background = element_rect(fill = honey_pal[1], colour = NA),
    plot.title = element_text(hjust = 0.5, family = ft_text, size = 250),
    plot.subtitle = element_text(hjust = 0.5, family = ft_text, size = 64, lineheight = 0.35),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text2, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text2, size = 36, face = "bold")
  ) +
  ggsave("2022/week02/bees.png", height = 12.25, width = 18)
