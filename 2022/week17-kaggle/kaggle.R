# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week17-kaggle/log.txt"

# load data ---------------------------------------------------------------

hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

# fonts and palettes ------------------------------------------------------

pal <- spec
bg <- "white"
txt_col <- dark
col1 <- spec[8]

ft_text <- "oreg"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_base <- hidden_gems |>
  time_log() |>
  arrange(title) |>
  mutate(
    ttl = title,
    tidy = str_detect(tolower(title), "tidy|dplyr"),
    x = seq(1, n()-1, length = 300),
    x = ifelse(x >= 150, x + 1, x),
    angle = c(seq(90, 0, length = 75), seq(360, 270, length = 75), seq(90, 0, length = 75), seq(360, 270, length = 75)),
    just = ifelse(x >= 150, 1, 0),
    title = ifelse(x >= 150, str_pad(title, pad = ".", width = 80, side = "right"), str_pad(title, pad = ".", width = 80, side = "left"))
    )

# titles ------------------------------------------------------------------

title <- glue("Kaggle Ain't<br>{col_generic('Tidy', col1)}")
subtitle <- str_rich_wrap(glue("Out of the 300 Kaggle hidden gems only {sum(df_base$tidy)} reference the {col_generic('tidyverse', col1)}"), 30)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Kaggle / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

df_base |>
  time_log() |>
  ggplot() +
  geom_text(aes(x, 0, label = title, colour = tidy, size = tidy), family = ft_text, angle = df_base$angle, hjust = df_base$just) +
  annotate("richtext", x = 1, y = -4, label = title, family = ft_text, size = 80, lineheight = 0.4, fill = NA, label.colour = NA, colour = txt_col) +
  annotate("richtext", x = 150, y = -3, label = subtitle, family = ft_text, size = 24, lineheight = 0.4, fill = NA, label.colour = NA, colour = txt_col) +
  scale_size_manual(values = c(10, 13)) +
  scale_colour_manual(values = c(txt_col, "darkred")) +
  labs(
    caption = caption
  ) +
  xlim(c(0.5, 300.5)) +
  ylim(c(-5, 2)) +
  coord_polar(clip = "off") +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 70, b = 20)),
    plot.margin = margin(t = 50, r = 20, b = 0, l = 20),
    legend.position = "none"
  ) +
  ggsave("2022/week17-kaggle/kaggle.png", height = 18, width = 16.45)
