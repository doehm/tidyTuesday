# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week14-news/log.txt"

# load data ---------------------------------------------------------------

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

# fonts and palettes ------------------------------------------------------

pal <- spec
bg <- "white"

ft_text <- "crete"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df <- news_orgs |>
  time_log() |>
  select(publication_name, state, country, year_founded, coverage_topics, budget = budget_percent_revenue_generation) |>
  mutate(topics = map(coverage_topics, ~str_split(.x, ", ")[[1]])) |>
  unnest(topics)

df_base <- df |>
  count(year_founded, topics) |>
  drop_na() |>
  mutate(topics = fct_reorder(topics, n, max))

df_n_pubs <- df |>
  count(year_founded, budget)

# titles ------------------------------------------------------------------

title <- "Digital Publications"
subtitle <- str_rich_wrap("
The number of digital publications have been increasing over time. The 3 most population topics covered are
Government issues, business and, education and schools. 2010 was the biggest year with 20 publications launching
with a focus on government issues.
", 60)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = 'black'))} @danoehm / Source: Project Oasis / Code: {get_icon('github', 10, fill = list(bg = bg, img = 'black'))} doehm/tidytuesday #rstats #tidytuesday")
fill <- "Number of\npublications"

# plot --------------------------------------------------------------------

# titles
g_text <- ggplot() +
  annotate("text", x = -1, y = 0, label = title, family = ft_text, size = 48, hjust = 0) +
  annotate("richtext", x = -1, y = -1, label = subtitle, family = ft_text, size = 18, hjust = 0, label.color = NA,
           lineheight = 0.4) +
  xlim(c(-1, 2)) +
  ylim(c(-2, 1)) +
  coord_cartesian(clip = "off") +
  theme_void()

# base plot
g_base <- df_base |>
  time_log() |>
  ggplot() +
  geom_tile(aes(year_founded, topics, fill = n)) +

  # theme and scales and labs
  scale_y_discrete(position = "right") +
  scale_x_continuous(position = "top", limits = c(1980, 2020)) +
  scale_fill_gradientn(colors = pal) +
  labs(
    caption = caption,
    fill = fill,
    x = "Year founded"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = dark, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.4, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 48, hjust = 1, margin = margin(b = 30, t = 5)),
    axis.text.y = element_text(vjust = 0.5, size = 48, hjust = 0),
    axis.title.x = element_text(size = 48)
  )

# put it together
g_base +
  inset_element(g_text, 0.025, 0, 0.6, 0.4) +
  ggsave("2022/week14-news/news.png", height = 12, width = 16)
