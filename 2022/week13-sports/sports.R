# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week13-sports/log.txt"

# load data ---------------------------------------------------------------

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

# fonts and palettes ------------------------------------------------------

ft_text <- "oswald"
ft_title <- ft_text

ft_text_col <- lakes[2]

scales_col <- rev(c(lakes[2], darken(lakes[1], 0.8)))
ft_text_col_v <- c(rep(scales_col[1], 34), darken(scales_col[2], 0.2), rep(scales_col[2], 2))

bg <- scales_col[2]

# wrangle -----------------------------------------------------------------

df_base <- sports |>
  time_log() |>
  group_by(sports, year) |>
  summarise(
    men = sum(partic_men, na.rm = TRUE),
    women = sum(partic_women, na.rm = TRUE)
  ) |>
  mutate(
    total = men + women,
    p_men = men/total,
    p_women = women/total,
    sports = fct_reorder(sports, p_men, min)
    ) |>
  ungroup()

# titles ------------------------------------------------------------------

subtitle <- str_rich_wrap("Gender split across college sports in the US. Shaded area indicates the variation from 2015", 40)
caption <- glue("Graphic: {get_icon('twitter', 15, fill = list(bg = bg, img = scales_col[1]))} @danoehm / Source: Equity in Athletics Data Analysis / Code: {get_icon('github', 15, fill = list(bg = bg, img = scales_col[1]))} doehm/tidytuesday #rstats #tidytuesday")

df_text <- tibble(
  x = 0,
  y = c(0.5, 0.89),
  label = c("Balanced (50%)", "More women (100%)")
)

# plot --------------------------------------------------------------------

df_base1 <- df_base |>
  filter(
    year == 2019,
    sports != "Team Handball"
  ) |>
  arrange(p_men) |>
  mutate(sports_num = 1:n()) |>
  select(sports, sports_num)

df_base2 <- df_base |>
  left_join(df_base1, by = "sports") |>
  pivot_longer(c(p_men, p_women), names_to = "gender", values_to = "p") |>
  time_log()

alpha <- 0.4

df_base2 |>
  ggplot() +
  geom_col(aes(x = sports_num, y = p, fill = gender), filter(df_base2, year == 2015), alpha = alpha, width = 1) +
  geom_col(aes(x = sports_num, y = p, fill = gender), filter(df_base2, year == 2016), alpha = alpha, width = 1) +
  geom_col(aes(x = sports_num, y = p, fill = gender), filter(df_base2, year == 2017), alpha = alpha, width = 1) +
  geom_col(aes(x = sports_num, y = p, fill = gender), filter(df_base2, year == 2018), alpha = alpha, width = 1) +
  geom_col(aes(x = sports_num, y = p, fill = gender), filter(df_base2, year == 2019), alpha = alpha, width = 1) +

  geom_text(aes(x = sports_num, y = 0.04, label = sports), df_base1,
            family = ft_text, size = 20, colour = ft_text_col_v, angle = 0, hjust = 0) +

  geom_text(aes(x = x, y = y, label = label), df_text, family = ft_text,
            colour = scales_col[1], size = 20, hjust = 0.5) +

  annotate("text", x = 18, y = 0.35, label = "WOMEN", family = ft_text, colour = scales_col[1], size = 64, fontface = "bold") +
  annotate("text", x = 18, y = 0.65, label = "MEN", family = ft_text, colour = scales_col[2], size = 64, fontface = "bold") +

  annotate("segment", x = 1, xend = 20, y = 0.5, yend = 0.5, colour = scales_col[1], lty = 2) +
  annotate("segment", x = 20, xend = 38, y = 0.5, yend = 0.5, colour = scales_col[2], lty = 2) +

  annotate("text", x = 33, y = 0.75, label = "College\nSports", lineheight = 0.35, fontface = "bold",
           size = 84, colour = scales_col[2], family = ft_text) +
  annotate("richtext", x = 27, y = 0.75, label = subtitle, lineheight = 0.6, size = 24,
           colour = scales_col[2], family = ft_text, fill = NA, label.colour = NA) +
  annotate("richtext", x = -1, y = 0.5, label = caption, lineheight = 0.6, size = 16,
           colour = scales_col[1], family = ft_text, fill = NA, label.colour = NA) +

  # theme and scales and labs
  scale_fill_manual(values = scales_col) +
  theme_void() +
  theme(
    text = element_text(colour = scales_col[1]),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(t = -85, r = -45, b = -30, l = -45),
    legend.position = "none"
  ) +
  coord_flip(clip = "off") +
  ggsave("2022/week13-sports/sports.png", height = 18, width = 12)
