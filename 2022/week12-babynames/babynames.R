# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week12-babynames/log.txt"

# load data ---------------------------------------------------------------

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

# fonts and palettes ------------------------------------------------------

bg <- "white"
ft_text_col <- dark

ft_text <- "hand"
ft_title <- "pm"

# wrangle -----------------------------------------------------------------

df_female <- babynames |>
  time_log() |>
  mutate(decade = floor(year/10)*10) |>
  filter(decade %in% c(1980, 2010)) |>
  group_by(decade, name, sex) |>
  summarise(n = sum(n)) |>
  pivot_wider(names_from = sex, values_from = n, values_fill = 0) |>
  rename(
    male = M,
    female = `F`
  ) |>
  mutate(
    total = male + female,
    p_female = female/total
  )

df_int <- df_female |>
  group_by(name) |>
  filter(n() > 1) |>
  mutate(grand_total = sum(total)) |>
  group_by(decade) |>
  slice_max(grand_total, n = 750, with_ties = TRUE) |>
  arrange(name, decade) |>
  group_by(name) |>
  mutate(
    lag_p_female = lag(p_female),
    change0 = p_female - lag_p_female,
    change = abs(p_female - lag_p_female),
    col = case_when(
      p_female < 0.5 & lag_p_female > 0.5 ~ "more_females",
      p_female > 0.5 & lag_p_female < 0.5 ~ "less_females",
      TRUE ~ "-"
    )
  )

df_col <- df_int |>
  drop_na() |>
  select(name, col, change, change0, p_female_2010 = p_female)

top_n <- 30
df_top <- df_int |>
  ungroup() |>
  drop_na() |>
  slice_max(change, n = top_n)

df_base <- df_int |>
  select(-col, -change, -change0) |>
  left_join(df_col, by = "name")


# titles ------------------------------------------------------------------

title <- "Baby Names"
subtitle <- str_rich_wrap(glue("
The popularity of baby names has changed over time, no only in absolute terms but also in terms of which sex the name is typically given to.
From the 1980s to the 2010s many names have remained traditionally <span style='color:{spec[2]}'>male</span>
and <span style='color:{spec[6]}'>female</span> names whereas some have
been given proportionally more to <span style='color:{spec[2]}'>males</span> or more to <span style='color:{spec[6]}'>females</span>. While
some names have bucked the trend there does seem to be convergence to either male or female.
The top 30 largest changes are listed, however it is suspected some may be influenced by data errors or small
sample sizes.
"), 110)
caption <- glue("Graphic: {get_icon('twitter', 10)} @danoehm / Source: babynames / Code: {get_icon('github', 10)} doehm/tidytuesday #rstats #tidytuesday")
fill <- "Fill"
colour <- "Colour"

# plot --------------------------------------------------------------------

df_base |>
  time_log() |>
  get_time() |>
  ggplot() +
  geom_line(aes(decade, p_female, group = name, colour = p_female_2010, alpha = change)) +
  geom_point(aes(decade, p_female, colour = p_female_2010, alpha = change), size = 2) +
  geom_text_repel(aes(decade, p_female, label = name), df_top, colour = ft_text_col, nudge_x = 10, family = ft_text, size = 18) +

  # theme and scales and labs
  scale_x_continuous(breaks = c(1980, 2002), labels = c("1980-1990", "2010-2020"), limits = c(1980, 2025)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = paste0(round(seq(0, 1, 0.25)*100), "%")) +
  scale_colour_gradientn(colors = spec[c(2, 4, 6)]) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = fill,
    colour = colour,
    y = str_wrap("Proportion of females", 30)
  ) +
  theme_void() +
  theme(
    text = element_text(colour = ft_text_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 180, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35, halign = 0),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 50, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(size = 48, family = ft_text, lineheight = 0.25, angle = 90),
    axis.text.y = element_text(size = 36, margin = margin(l = 10)),
    axis.text.x = element_text(size = 64, hjust = 0)
  ) +
  ggsave("2022/week12-babynames/babynames.png", height = 14, width = 11)


