# https://github.com/rfordatascience/tidytuesday

# for libraries and custom functions
source("scripts/startup.R")

log_file <<- "2022/week09-energy/log.txt"

# load data ---------------------------------------------------------------

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv') |>
  clean_names()

# fonts and palettes ------------------------------------------------------

bg <- "white"

ft_text <- "barlow"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_base <- stations |>
  time_log() |>
  group_by(state) |>
  summarise(p = sum(fuel_type_code == "ELEC")/n()) |>
  mutate(
    code = state,
    info_graph = glue("<img src='2022/week09-energy/bolts/{state}.png' width = 35 height = 50>")
  )

# create the fill charts and merge with the bolts
for(k in 1:nrow(df_base)) {
  tibble(x = 0, y = df_base$p[k]) |>
    ggplot(aes(x, y)) +
    geom_col(fill = "#219ebc") +
    ylim(c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "black"), plot.margin = margin(0, -100, -50, -100)) +
    ggsave(glue("2022/week09-energy/bolts/{df_base$state[k]}.png"), height = 4, width = 3)

  base <- image_read('C:/Users/Dan/Downloads/fontawesome/lightning-bolt.png') |>
    image_resize("220x180")

  measure <- image_read(glue("2022/week09-energy/bolts/{df_base$state[k]}.png")) |>
    image_resize("220x180")

  image_composite(base, measure, "plus") |>
    image_write(glue("2022/week09-energy/bolts/{df_base$state[k]}.png"))
}

# titles ------------------------------------------------------------------

title <- "Alternative Fuel Stations"
subtitle <- str_rich_wrap("
The proportion of fuel stations offering <strong><span style='color:#219ebc'>electric charging facilities</span></strong> is
very high in the East and West of the US but lacking in the central states.
", 100)
caption <- glue("Graphic: {get_icon('twitter', 10)} @danoehm / Source: US DOT / Code: {get_icon('github', 10)} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

df_base |>
  time_log() |>
  ggplot() +
  geom_richtext(aes(0, 0, label = info_graph), label.colour = NA, fill = NA) +
  facet_geo(~code, grid = us_state_grid1) +

  # theme and scales and labs
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(colour = dark),
    plot.background = element_rect(fill = bg, colour = NA),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 200, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 64, lineheight = 0.35, margin = margin(b = 10), halign = 0),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(family = ft_text, size = 60, margin = margin(b = 5), face = "bold")
  ) +
  ggsave("2022/week09-energy/energy.png", height = 11.5, width = 16)

