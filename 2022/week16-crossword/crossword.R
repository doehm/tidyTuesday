# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week16-crossword/log.txt"

# load data ---------------------------------------------------------------

times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-19/times.csv')

# fonts and palettes ------------------------------------------------------

bg <- "#FDF8EC"
txt_col <- dark

ft_text <- "sec"
ft_title <- "nyt"

# wrangle -----------------------------------------------------------------

df_base <- times |>
  time_log() |>
  mutate(
    clue_num = as.numeric(str_extract(clue_number, "[:digit:]+")),
    alphas = str_length(str_extract(clue_number, "[:alpha:]+")),
    direction = case_when(
      str_detect(clue_number, "a") ~ "Across",
      str_detect(clue_number, "d") ~ "Down"),
    clue_length = str_length(clue),
    answer_length = str_length(answer)
  ) |>
  filter(
    clue_num < 100,
    alphas == 1,
    !is.na(answer)
    )

df_text <- df_base |>
  time_log() |>
  count(clue_num, answer, direction) |>
  group_by(clue_num, direction) |>
  slice_max(n, with_ties = TRUE) |>
  sample_n(1) |>
  mutate(
    label = paste0(clue_num, ". ", to_title_case(answer)),
    y = (clue_num - 1) %% 15 + 1,
    x = ifelse(clue_num <= 15, 1, 2)
    ) |>
  filter(clue_num <= 30)

df_grid0 <- tibble(
  x = 1:31,
  xend = 1:31,
  y = 1,
  yend = 31
) |>
  bind_rows(
    tibble(
      x = 1,
      xend = 31,
      y = 1:31,
      yend = 1:31
    )
  )

df_grid <- df_grid0 |>
  mutate(direction = "Across") |>
  bind_rows(
    df_grid0 |>
      mutate(direction = "Down")
  )

# titles ------------------------------------------------------------------

title <- "Crosswords"
subtitle <- str_rich_wrap(
"I looked at three seemingly unrelated features of the New York Times crosswords 1) clue number 2) direction and 3) answer length
in terms of the number of letters in the answer.
For answers that go across things don't seem too radical. Clue 1 has an average length around 8 letters. This is followed
by a gap between 2 and 8. From clue 9 things start to look normal again. With respect to the down clues the first 1 to 8
seem to more or less fill the hole in the across clues. Then there's this weird trend from clue 10 to 24 where
answers tend to be longer when closer to clue 10 and shorter when close to clue 24. This might make sense to people who regulalry do crosswords!", 110)

caption1 <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Cryptics.georgeho.org")
caption2 <- glue("Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

g_base <- ggplot() +
  annotate("text", x = 0.5, y = 1.01, label = "CROSSWORDS", family = "nyt", size = 74) +
  annotate("richtext", x = 0.05, y = 0.96, label = caption1, family = ft_text, size = 12, label.colour = NA, hjust = 0, fill = bg) +
  annotate("richtext", x = 0.95, y = 0.96, label = caption2, family = ft_text, size = 12, label.colour = NA, hjust = 1, fill = bg) +
  annotate("richtext", x = 0.05, y = 0.94, label = subtitle, family = ft_text, size = 16, lineheight = 0.35, vjust = 1, hjust = 0, label.colour = NA, fill = bg) +
  annotate("segment", x = 0.05, xend = 0.95, y = 0.95, yend = 0.95) +
  xlim(c(0, 1)) +
  ylim(c(0, 1.01)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg)
  )

g_crossword <- df_base |>
  time_log() |>
  count(direction, clue_num, answer_length) |>
  ggplot() +
  geom_tile(aes(clue_num, answer_length, fill = n), width = 1, height = 1) +
  geom_segment(aes( x = x - 0.5, xend = xend - 0.5, y = y-0.5, yend = yend-0.5), df_grid) +
  facet_wrap(~direction, nrow = 2, scales = "free") +
  scale_fill_gradientn(colours = colorRampPalette(c("grey90", "black"))(16)) +
  scale_x_continuous(breaks = 1:30, labels = 1:30, limits = c(0, 31)) +
  scale_y_continuous(breaks = 1:30, labels = 1:30) +
  labs(
    y = "Answer length (letters)",
    x = "Clue Number"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 64, margin = margin(b = 20, t = 20)),
    axis.text = element_text(size = 32),
    axis.title = element_text(size = 64, margin = margin(b = 20, t = 20, r = 20, l = 20)),
    axis.title.y = element_text(angle = 90, hjust = 0.5),
    panel.spacing = unit(1, "inches")
  )

g_text <- df_text |>
  mutate(direction = glue("The most common answers for {direction} clues<br>at the corresponding clue number")) |>
  ggplot() +
  geom_text(aes(x, -y, label = label), family = ft_text, size = 12, hjust = 0, lineheight = 0.2) +
  facet_wrap(~direction, nrow = 2) +
  coord_cartesian(clip = "off") +
  xlim(c(0.5, 3.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg),
    strip.text = element_markdown(family = ft_text, size = 48, lineheight = 0.25, margin = margin(b = 20), hjust = 0.4, halign = 0),
    panel.spacing = unit(2, "inches")
  )

# final
g_base +
  inset_element(g_crossword, top = 0.8, bottom = 0.01, left = 0.05, right = 0.55) +
  inset_element(g_text, top = 0.78, bottom = 0.1, left = 0.55, right = 1.05) +
  plot_annotation(
    caption = "Random draw when there is a tie for most common word.",
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg),
      plot.caption = element_text(family = ft_text, size = 32, hjust = 1, margin = margin(t = 5))
    )
  ) +
  ggsave("2022/week16-crossword/crossword.png", height = 18, width = 13)

