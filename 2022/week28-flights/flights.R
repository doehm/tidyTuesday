# https://github.com/rfordatascience/tidytuesday

# note: you will need to run the start up script to load some functions,
# fonts and palettes
source("scripts/startup.R")

library(tidyverse)
library(fable)
library(fabletools)
library(tsibble)
library(ggtext)

# load data ---------------------------------------------------------------

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv') |>
  clean_names()

# fonts and palettes ------------------------------------------------------

bg <- lakes[5]
txt_col <- "grey20"

ft_text <- "barlow"
ft_title <- ft_text

ft_annotate_size <- 16

# titles ------------------------------------------------------------------

title <- "Commercial Flights in the EU"
title1 <- "An estimated"
title2 <- "flights did not depart due to the impact\nof COVID-19 or 51% of expected volume"

text_lockdown <- str_rich_wrap("COVID-19 lockdowns started to be introduced in countries within the EU", 23)
text_forecast <- str_rich_wrap("Forecast of flight depatures if lockdowns / COVID-19 did not occur", 40)
text_obs <- str_rich_wrap("Observed flight departures from 1st March 2020", 18)

caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Eurocontrol / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# wrangle -----------------------------------------------------------------

df_base <- flights |>
  time_log() |>
  mutate(flt_date = floor_date(ymd(flt_date), unit = "month")) |>
  group_by(year, month_mon, flt_date) |>
  summarise(
    dep = sum(flt_dep_1),
    arr = sum(flt_arr_1)
  ) |>
  mutate(yr_mth = yearmonth(paste(year, month_mon)))

# fit time series model
mod <- df_base |>
  filter(flt_date < ymd("2020-02-01")) |>
  as_tsibble(index = yr_mth) |>
  model(ARIMA(dep))

# set new data
new_data <- df_base |>
  filter(flt_date >= ymd("2020-02-01")) |>
  as_tsibble(index = yr_mth)

# forecast
fcst <- mod |>
  forecast(new_data = new_data)

# plot forecast
fcst |>
  autoplot(as_tsibble(df_base, index = yr_mth))

# plot diagnostics
mod |>
  gg_tsresiduals(lag_max = 12)

# get predictions
pred <- fcst |>
  as_tibble() |>
  left_join(
    df_base |>
      ungroup() |>
      select(flt_date, obs = dep),
    by = "flt_date"
  )

# summary stats
pred |>
  mutate(d = .mean - obs) |>
  summarise(
    d = sum(d),
    obs = sum(obs),
    pred = sum(.mean)
    ) |>
  mutate(
    pct = obs/pred,
    x = pred - obs
    )

df_curves <- tribble(
  ~id, ~x, ~xend, ~y, ~yend,
  1, "2019-06-01", "2020-01-25", 425000, 580000,
  2, "2021-01-11", "2021-07-01", 920000, 880669,
  3, "2021-11-20", "2021-04-01", 250000, 227637
) |>
  mutate(
    x = ymd(x),
    xend = ymd(xend)
  )

df_text <- tribble(
  ~id, ~x, ~y, ~text,
  1, "2019-01-25", 420000, text_lockdown,
  2, "2021-01-01", 940000, text_forecast,
  3, "2021-12-01", 300000, text_obs
) |>
  mutate(x = ymd(x))

# plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_line(aes(x = flt_date, y = dep), colour = txt_col, size = 1.5) +
  geom_line(aes(x = flt_date, y = .mean), pred, lty = 2, colour = lakes[2], size = 1.5) +
  geom_ribbon(aes(x = flt_date, ymin = obs, ymax = .mean), pred, fill = "grey90", colour = NA, alpha = 0.5) +
  annotate(
    "text", x = ymd("2017-01-01"), y = 430000, label = title1,
    family = ft_text, size = 18, hjust = 0, colour = txt_col
    ) +
  annotate(
    "text", x = ymd("2017-01-01"), y = 350000, label = "21,250,000",
    family = ft_text, size = 64, hjust = 0, colour = txt_col, fontface = "bold"
  ) +
  annotate(
    "text", x = ymd("2017-01-01"), y = 270000, label = title2,
    family = ft_text, size = 18, hjust = 0, colour = txt_col, lineheight = 0.35, vjust = 1
  ) +

  geom_richtext(aes(x, y, label = text, group = id), df_text,
    family = ft_text, size = ft_annotate_size, hjust = c(0, 1, 0), colour = txt_col, lineheight = 0.35, vjust = 1,
    label.colour = NA, fill = NA
    ) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend, group = id), df_curves,
    arrow = arrow(length = unit(0.015, "npc"), type = "closed"), curvature = -0.3
    ) +

  # # lockdown
  # annotate(
  #   "richtext", x = ymd("2019-01-25"), y = 420000, label = text_lockdown,
  #   family = ft_text, size = ft_annotate_size, hjust = 0, colour = txt_col, lineheight = 0.35, vjust = 1,
  #   label.colour = NA, fill = NA
  # ) +
  # annotate(
  #   "curve", x = ymd("2019-06-01"), xend = ymd("2020-01-25"), y = 425000, yend = 580000,
  #   arrow = arrow(length = unit(0.015, "npc"), type = "closed"), curvature = -0.3
  # ) +
  #
  # # forecast
  # annotate(
  #   "richtext", x = ymd("2021-01-01"), y = 940000, label = text_forecast,
  #   family = ft_text, size = ft_annotate_size, hjust = 1, colour = txt_col, lineheight = 0.35, vjust = 1,
  #   label.colour = NA, fill = NA
  # ) +
  # annotate(
  #   "curve", x = ymd("2021-01-11"), xend = ymd("2021-07-01"), y = 920000, yend = 880669,
  #   arrow = arrow(length = unit(0.015, "npc"), type = "closed"), curvature = -0.3
  # ) +
  #
  # # observed
  # annotate(
  #   "richtext", x = ymd("2021-12-01"), y = 300000, label = text_obs,
  #   family = ft_text, size = ft_annotate_size, hjust = 0, colour = txt_col, lineheight = 0.35, vjust = 1,
  #   label.colour = NA, fill = NA
  # ) +
  # annotate(
  #   "curve", x = ymd("2021-11-20"), xend = ymd("2021-04-01"), y = 250000, yend = 227637,
  #   arrow = arrow(length = unit(0.015, "npc"), type = "closed"), curvature = -0.3
  # ) +

  # scales
  scale_y_continuous(
    breaks = 1:9*100000,
    labels = paste0(1:9*100, "k"),
    position = "right",
    limits = c(NA, 940000)
  ) +
  scale_x_date(
    breaks = ymd("2015-01-01") + years(1:7),
    labels = 2016:2022,
    position = "top"
  ) +

  # labs and theme
  labs(
    title = title,
    caption = caption,
    y = "Monthly commercial flight departures"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 128, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 64, lineheight = 0.35),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.grid.major = element_line(colour = "white", size = 0.5),
    axis.text = element_text(hjust = 0.5, family = ft_text, size = 48),
    axis.title.y = element_text(family = ft_text,size = 48, lineheight = 0.3, hjust = 0.5, angle = 270, margin = margin(l = 10))
  )

# save
ggsave("2022/week28-flights/flights.png", height = 8, width = 16)

