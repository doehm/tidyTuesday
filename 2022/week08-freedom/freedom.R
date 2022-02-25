# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week08-freedom/log.txt"

# load data ---------------------------------------------------------------

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') |>
  clean_names() |>
  mutate(
    status = factor(status, levels = c("F", "PF", "NF")),
    region_name = factor(region_name, levels = c("Asia", "Africa", "Americas", "Europe", "Oceania"))
  )

# fonts and palettes ------------------------------------------------------

pal <- c(good_pal[3:4], dark)

ft_text <- "barlow"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_base <- freedom |>
  time_log() |>
  group_by(year, region_name) |>
  count(status) |>
  group_by(year, region_name) |>
  mutate(p = n/sum(n))

df_p <- freedom |>
  time_log() |>
  group_by(country, region_name) |>
  count(status) |>
  filter(status == "NF") |>
  group_by(country, region_name) |>
  summarise(n = sum(n)) |>
  filter(
    n == 26,
    country != "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire"
    ) |>
  group_by(region_name) |>
  summarise(country = paste(country, collapse = "\n"))

# titles ------------------------------------------------------------------

title <- "Freedom in the World"
subtitle <- str_rich_wrap(glue("
~40%-50% of countries in the Asia and African regions are considered to be <strong>not free</strong>. Europe
and Oceania have the highest proportion of <strong><span style='color:{pal[1]}'>free</span></strong> countries. The countries in text are those
which have been considered <strong>not free</strong> every year for the past 25 years (1995-2020).
"), 150)
caption <- "Graphic: @danoehm / Source: Freedom House and the United Nations by way of Arthur Cheib / #rstats #tidytuesday week 8"
fill <- "Freedom Status"
colour <- "Colour"

# plot --------------------------------------------------------------------

df_base |>
  time_log() |>
  ggplot() +
  geom_area(aes(x = year, y = p, fill = status)) +
  geom_text(
    aes(x = 1997, y = 0.02, label = country), df_p,
    family = ft_text, size = 14.5, colour = light, vjust = 0,
    lineheight = 0.28, hjust = 0, fontface = "bold"
    ) +
  facet_wrap(~region_name, nrow = 1) +

  # theme and scales and labs
  scale_fill_manual(values = pal[1:3], breaks = c("F", "PF", "NF"), labels = c("Free", "Partially free", "Not free")) +
  scale_x_continuous(breaks = c(1995, 2020), labels = c(1995, 2020)) +
  scale_y_continuous(breaks = c(0.5, 1), labels = c("50%", "100%")) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = fill,
    colour = colour
  ) +
  theme_void() +
  theme(
    text = element_text(colour = light),
    plot.background = element_rect(fill = dark, colour = NA),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 250, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 64, lineheight = 0.35, margin = margin(b = 15), halign = 0),
    plot.caption = element_text(hjust = 0, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.text.y = element_text(family = ft_text, size = 36),
    axis.text.x = element_text(family = ft_text, size = 36, margin = margin(t = -20)),
    strip.text = element_text(family = ft_text, size = 96, face = "bold"),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.box.margin = margin(t = 10),
    legend.position = "bottom"
  ) +
  ggsave("2022/week08-freedom/freedom.png", height = 12.25, width = 18)
