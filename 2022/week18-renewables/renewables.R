# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week18-renewables/log.txt"

# load data ---------------------------------------------------------------

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

# fonts and palettes ------------------------------------------------------

pal <- spec[c(5, 2)]
bg <- dark
txt_col <- "white"

ft_text <- "sec"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_capacity <- bind_rows(
  "Solar" = solar |>
    select(date, capacity = solar_capacity),
  "Wind" = wind |>
    select(date, capacity = wind_capacity),
  .id = "energy_type"
)

df_cost <- bind_rows(
  "Solar" = solar |>
    select(date, mwh = solar_mwh),
  "Wind" = wind |>
    select(date, mwh = wind_mwh),
  .id = "energy_type"
)

# titles ------------------------------------------------------------------

title <- glue("<span style='color:{pal[2]}'>Wind</span> and <span style='color:{pal[1]}'>Solar</span> Power Generation")
subtitle <- glue("The cost of <span style='color:{pal[1]}'>solar</span> has dropped by <strong>77%</strong> over the last 13 years and now on par with <span style='color:{pal[2]}'>wind</span>")
subtitle_cost <- str_rich_wrap(glue("Whereas the average cost per MWh has plummeted suggesting there are other factors than just capacity influencing price."), 62)
subtitle_cap <- str_rich_wrap(glue("The capacity of <span style='color:{pal[2]}'>wind</span> and <span style='color:{pal[1]}'>solar</span> power generation has steadily increased over the last 13 years, particulalry <span style='color:{pal[1]}'>solar</span> in the last 5"), 62)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = dark, img = light))} @danoehm / Source:  Berkeley Lab / Code: {get_icon('github', 10, fill = list(bg = dark, img = light))} doehm/tidytuesday #rstats #tidytuesday")
colour <- "Energy type"

# plot --------------------------------------------------------------------

g_base <- df_cost |>
  time_log() |>
  ggplot() +
  geom_point(aes(date, mwh, colour = energy_type), size = 2, alpha = 0.3) +
  geom_smooth(aes(date, mwh, colour = energy_type), se = FALSE, method = "gam") +

  # theme and scales and labs
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(
    subtitle = subtitle_cost,
    y = "$/MWh",
    fill = fill,
    colour = colour
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 250),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35, halign = 0),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "bottom",
    axis.text = element_text(size = 48),
    axis.title.y = element_text(size = 48)
  )


g_cap <- df_capacity |>
  mutate(year = year(date)) |>
  time_log() |>
  ggplot() +
  geom_point(aes(date, capacity, colour = energy_type), size = 2, alpha = 0.3) +
  geom_smooth(aes(date, capacity, colour = energy_type), se = FALSE, method = "gam", span = 0.9) +

  # theme and scales and labs
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  labs(
    subtitle = subtitle_cap,
    y = "GW",
    colour = colour
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 250),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35, halign = 0),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "bottom",
    axis.text = element_text(size = 48),
    axis.title.y = element_text(size = 48)
  )

# patch it together
g_cap +
  g_base +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = theme(
      text = element_text(colour = txt_col),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.title = element_markdown(hjust = 0.5, family = ft_title, size = 120, margin = margin(t = 20)),
      plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 64, lineheight = 0.35, margin = margin(t = 10)),
      plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 0, b =5))
    )
  ) +
  ggsave("2022/week18-renewables/renewables.png", height = 9, width = 18)

