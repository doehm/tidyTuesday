# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week21-rugby/log.txt"

# load data ---------------------------------------------------------------

sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv') |>
  clean_names() |>
  mutate(
    score_1 = as.numeric(score_1),
    score_2 = as.numeric(score_2)
  )
fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv') |>
  mutate(home_away_win = factor(home_away_win, levels = rev(c("H", "A", "D", "N"))))

# fonts and palettes ------------------------------------------------------

bg <- "grey20"
pal <- pror
pal2 <- pal[c(2, 1, 3, 4)]
txt_col <- "white"

ft_text <- "oswald"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_home <- fifteens |>
  count(home_away_win) |>
  mutate(p = n/sum(n))

df_score <- fifteens |>
  filter(home_away_win %in% c("H", "A")) |>
  mutate(score_win = ifelse(home_away_win == "H", score_1, score_2))

# simple prior: theta ~ beta(5, 5)
# sampling directly
df_country <- fifteens |>
  filter(home_away_win %in% c("H", "A")) |>
  select(home_away_win, winner, loser) |>
  pivot_longer(-home_away_win, names_to = "result", values_to = "country") |>
  count(country, result, home_away_win) |>
  group_by(country, home_away_win) |>
  pivot_wider(names_from = c(home_away_win, result), values_from = n, values_fill = 0) |>
  mutate(
    theta_A = map2(A_winner, A_loser, ~rbeta(1000, 5 + .x, 5 + .y)),
    theta_H = map2(H_winner, H_loser, ~rbeta(1000, 5 + .x, 5 + .y))
    ) |>
  unnest(c(theta_A, theta_H)) |>
  mutate(
    index = theta_H/theta_A,
    median = median(index)
    ) |>
  ungroup() |>
  mutate(country = fct_reorder(country, median, min)) |>
  group_by(country) |>
  summarise(
    q2.5 = quantile(index, 0.05),
    q10 = quantile(index, 0.1),
    q25 = quantile(index, 0.25),
    q50 = quantile(index, 0.5),
    q75 = quantile(index, 0.75),
    q90 = quantile(index, 0.9),
    q97.5 = quantile(index, 0.95),
    y = as.numeric(country)[1]
  )

# model -------------------------------------------------------------------

mod <- stan_glm(margin_of_victory ~ home_away_win, data = df_score)
beta <- mod$stanfit@sim$samples[[1]]$`beta[1]` # I need to commit tidybayes to memory. This'll do.

# titles ------------------------------------------------------------------

title <- "Womens World Rugby Series\nIs there a Home Ground Advantage?"
subtitle <- str_rich_wrap(
  "There is some evidence of a home ground advantage for some countries in the Womens World Rugby
  Fifteens competition, but not a general trend. Although winning scores tend to be higher when
  played at home. The data includes results from 1466 tests dating back to 1982", 90)
caption <- glue(
  "Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm /
  Source: ScrumQueens / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))}
  doehm/tidytuesday #rstats #tidytuesday")

text_bar <- str_wrap("There have been 137 more wins at home than away", 20)
text_margin <- str_wrap(
  "On average winning teams score +4.2 points more when they win at
  home than when they win playing away", 30)
text_bayes <- str_rich_wrap(glue(
  "Index: Proportion of wins at home / away<br><br>Bayesian estimate of the ratio of wins at home
  over wins away. Countries to the right of the line
  have a higher proportion of wins at home e.g. {cg('Spain', pal2[1])}, {cg('Italy', pal2[1])} and
  {cg('Ireland', pal2[1])} appear to have a home ground advantage. However, it doesn't seem to be a general
  trend across all countries. Intervals at 50%, 80%, 90%.
  "), 35)

# plot --------------------------------------------------------------------
# base plot ---------------------------------------------------------------

g_base <- ggplot() +
  annotate("point", 0, 0, colour = bg) +
  labs(
    title = title,
    subtitle = subtitle
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 20, r = 20, b = 20, l = 50),
    plot.title = element_text(hjust = 0, family = ft_title, size = 128, colour = txt_col, lineheight = 0.35, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0, family = ft_text, size = 48, lineheight = 0.4, colour = txt_col),
    plot.background = element_rect(fill = bg,  colour = bg),
  )

# bar chart ---------------------------------------------------------------

g_bar <- df_home |>
  ggplot(aes(home_away_win, n, fill = home_away_win)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_text(aes(y = n + 10, label = n), family = ft_text, size = 16, hjust = 0, colour = txt_col) +
  annotate("text", x = "A", y = 570, label = text_bar, family = ft_text, colour = txt_col, size = 16, lineheight = 0.35, hjust = 0, vjust = 1) +

  # theme and scales and labs
  labs(
    title = "Home ground wins",
    fill = "Location of win"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = pal[c(2, 1, 3, 4)],
    breaks = c("H", "A", "D", "N"),
    labels = c("Home", "Away", "Draw", "Both teams away")
    ) +
  scale_colour_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0, family = ft_title, size = 48, margin = margin(b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25),
    legend.text = element_text(family = ft_text, size = 36),
    legend.position = "top"
  )

# mrgin posterior density -------------------------------------------------

g_margin <- tibble(beta = beta, it = rep(1:5, 200)) |>
  ggplot(aes(beta, group = it)) +
  geom_density(colour = pal2[1], fill = pal2[1], alpha = 0.1) +
  geom_vline(xintercept = c(0, median(beta)), colour = txt_col, lty = 3) +
  annotate("text", x = 7, y = 0.2, label = text_margin, family = ft_text, colour = txt_col, size = 16, lineheight = 0.35, hjust = 0) +
  scale_x_continuous(
    breaks = c(0, median(beta)),
    labels = c(0, round(median(beta), 1)),
    limits = c(0, 12)
  ) +
  labs(title = "Posterior margin of victory at home") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0, family = ft_title, size = 48, margin = margin(b = 10)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.position = "none",
    axis.text.x = element_text(family = ft_text, size = 36)
  )


# country index (plot on the right) ---------------------------------------

g_country <- df_country |>
  ungroup() |>
  ggplot(aes(fill = q50)) +
  geom_rect(aes(xmin = q2.5, xmax = q97.5, ymin = y-0.4, ymax = y + 0.4), alpha = 0.33) +
  geom_rect(aes(xmin = q10, xmax = q90, ymin = y-0.4, ymax = y + 0.4), alpha = 0.33) +
  geom_rect(aes(xmin = q25, xmax = q75, ymin = y-0.4, ymax = y + 0.4), alpha = 0.33) +
  geom_point(aes(q50, y), size = 2, colour = "grey20") +
  annotate("segment", x = 1, xend = 1, y = 0, yend = 71, lty = 3, colour = "white") +
  annotate("richtext", x = 2.5, y = 35, label = text_bayes, family = ft_text, colour = txt_col,
           size = 16,
           lineheight = 0.4, hjust = 0, vjust = 1,
           label.colour = NA, fill = NA
           ) +
  annotate("text", x = 0.2, y = -0.5, label = "More wins away", colour = "white", family = "oswald", size = 14, hjust = 0) +
  annotate("text", x = 1.3, y = -0.5, label = "More wins at home", colour = "white", family = "oswald", size = 14, hjust = 0) +

  # themes and scales
  scale_y_continuous(
    breaks = 1:71,
    labels = df_country$country
  ) +
  scale_fill_gradient(low = pal[1], high = pal[2]) +
  labs(x = "Index: Proportion of wins at home / away") +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.text.y = element_text(family = ft_text, size = 32, lineheight = 0.25, hjust = 1),
    legend.position = "none"
  )


# patch them together -----------------------------------------------------

g_base +
  inset_element(g_bar, left = -0.03, right = 0.4, top = 1, bottom = 0.5) +
  inset_element(g_margin, left = -0.03, right = 0.45, top = 0.5, bottom = 0) +
  inset_element(g_country, left = 0.5, right = 1, top = 1.3, bottom = -0.05) +
  plot_annotation(
    caption = caption,
    theme = theme(
      text = element_text(colour = txt_col, family = ft_text),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35)
    )
  )

# save plot ---------------------------------------------------------------

ggsave("2022/week21-rugby/rugby.png", height = 12, width = 20)

