# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week20-eurovision/log.txt"

# load data ---------------------------------------------------------------

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# fonts and palettes ------------------------------------------------------

pal_bright <- colorRampPalette(bright)(52)
pal <- c(rgb(167, 124, 107, maxColorValue = 255), rgb(124, 124, 124, maxColorValue = 255), rgb(155, 114, 30, maxColorValue = 255))
bg <- dark
txt_col <- "white"

ft_text <- "zen"
ft_title <- ft_text

ft1 <- 60

# wrangle -----------------------------------------------------------------

df_points <- eurovision |>
  group_by(year, artist_country) |>
  summarise(points = sum(total_points,na.rm = TRUE)) |>
  arrange(year, artist_country) |>
  group_by(artist_country) |>
  mutate(cm_points = cumsum(points))

df_base <- eurovision |>
  filter(year == 2022) |>
  group_by(artist_country) |>
  mutate(total = sum(total_points)) |>
  ungroup() |>
  mutate(
    artist_country = fct_reorder(artist_country, total, min),
    section = factor(section, levels = c("first-semi-final", "second-semi-final", "grand-final"))
    )

# titles ------------------------------------------------------------------

title <- "EUROVISION"
subtitle <- str_rich_wrap("Points awarded for the 2022 Eurovision Song Contest", 100)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Eurovision @tanya_shapiro / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")
fill <- "Section"

# plot --------------------------------------------------------------------

df_base |>
  ggplot(aes(artist_country, total_points, fill = section)) +
  geom_chicklet(radius = grid::unit(8, "pt"), colour = NA) +
  coord_flip() +

  # theme and scales and labs
  scale_fill_manual(values = pal) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    y = "Points",
    fill = fill,
    colour = colour
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 250),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 80, lineheight = 0.35),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = ft1, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = ft1, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = ft1, face = "bold"),
    legend.position = "bottom",
    axis.text = element_text(size = ft1, hjust = 1),
    axis.title.x = element_text(size = ft1)
  )

ggsave("2022/week20-eurovision/eurovision1.png", height = 12.25, width = 10)

# album cover outtake -----------------------------------------------------

bg <- "white"
txt_col <- dark
df_points |>
  ggplot(aes(year, points, fill = artist_country)) +
  geom_stream() +

  # theme and scales and labs
  scale_fill_manual(values = pal_bright) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = -50, b = 20, l = -50),
    legend.position = "none"
  )

ggsave("2022/week20-eurovision/eurovision.png", height = 12.25, width = 12.25)
