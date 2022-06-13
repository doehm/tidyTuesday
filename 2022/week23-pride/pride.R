# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week23-pride/log.txt"

# load data ---------------------------------------------------------------

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')
fortune_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv')
static_list <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')
pride_sponsors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv')
corp_by_politicians <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politicians.csv')
donors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv')

# fonts and palettes ------------------------------------------------------

rainbow <- c(L = "#E50000", G = "#FF8C00", T = "#FDED05", B = "#018221", Q = "#014CFD", `+` = "#760188")
pal <- rainbow
dullbow <- colorRampPalette(c("#22223b", "#4a4e69", "#9a8c98", "#c9ada7", "#f2e9e4"))(6)
bg <- "white"
txt_col <- dark

ft_text <- "indie"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_pride <- static_list |>
  clean_names() |>
  select(company, pride = hrc_business_pledge)

df_base <- pride_aggregates |>
  time_log() |>
  clean_names() |>
  rename(
    total = total_contributed,
    politicians = number_of_politicians_contributed_to
    ) |>
  filter(company != "Grand Total") |>
  left_join(df_pride, by ="company") |>
  mutate(
    n_poli = politicians,
    politicians = min_max(-politicians, -0.5, 0.48),
    total = paste0("$", round(total/1000), "k"),
    a = 150,
    company = ifelse(str_detect(company, "Jack Daniel"), "Jack Daniel's", company),
    company = fct_reorder(company, -politicians, min),
    label = paste0(total, " / ", n_poli)
    )

df_rainbow <- map2_dfr(df_base$politicians, df_base$company, function(.t, .company) {
  map_dfr(1:100, ~{
    tibble(
      x = sin(seq(-0.5, .t, 0.01)*pi),
      y = cos(seq(-0.5, .t, 0.01)*pi),
      a = .x + 50 + 50
    ) |>
      mutate(
        x = a*x + 170 + 50,
        y = a*y,
        group = 1:n(),
        company = .company
      )
  })
}) |>
  left_join(select(df_base, company, pride), by = "company")

df_textline <- map2_dfr(df_base$politicians, df_base$company, function(.t, .company) {
    tibble(
      x = sin(seq(-0.5, 0.5, 0.01)*pi),
      y = cos(seq(-0.5, 0.5, 0.01)*pi),
      a = 170 + 50
    ) |>
      mutate(
        x = a*x + 170 + 50,
        y = a*y,
        group = 1:n(),
        company = .company,
        col = NA
      )
})

# titles ------------------------------------------------------------------

lgbt <- glue("{cg('L')}{cg('G')}{cg('B')}{cg('T')}{cg('Q')}{cg('+')}")

# The colourful rainbows are those that made the HRC business pledge. The dullbows are those that did not

title <- glue("Pride Sponsors and Political Ties")
subtitle <- glue(
"Each company is a pride sponsor although they also contributed to anti-{lgbt} politicians. The rainbow is a scale,<br>
more rainbow means more support for the {lgbt} community and less for the politicians relative to other companies on<br>
the list. For every Anti-{lgbt} politician supported, the rainbow is reduced.
")
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Data For Progress / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

g_base <- df_rainbow |>
  ggplot(aes(x, y, group = a, colour = a)) +

  # colour rainbows
  geom_line(size = 1) +
  scale_color_gradientn(colours = rev(rainbow)) +

  # start new scale
  new_scale_colour() +

  # dullbows
  geom_line(aes(x, y, group = a, colour = a), filter(df_rainbow, !pride), size = 1) +
  scale_color_gradientn(colours = rev(dullbow)) +

  # text
  geom_textpath(aes(x, y, label = company), df_textline, colour = txt_col, size = 14, family = ft_text, text_only = TRUE, fontface = "bold") +
  geom_text(aes(170 + 50, 0, label = label), df_base, colour = txt_col, size = 22, family = ft_text) +
  facet_wrap(~company) +
  ylim(c(0, 260)) +
  coord_cartesian(clip = "off") +

  # labels and themes
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_markdown(hjust = 0.15, family = ft_title, size = 128),
    plot.subtitle = element_markdown(hjust = 0, family = ft_text, size = 48, lineheight = 0.35, margin = margin(b = 40)),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    strip.text = element_blank(),
    legend.position = "none"
  )

ggsave("2022/week23-pride/pride.png", height = 12, width = 18)


# legend ------------------------------------------------------------------

df_legend <- df_rainbow |>
  filter(company %in% c("Bank of America", "Capital One"))

df_textline_legend <- df_textline |>
  filter(company %in% c("Bank of America", "Capital One")) |>
  mutate(label = ifelse(company == "Capital One", "Yes", "No"))

g_legend <- df_legend |>
  ggplot(aes(x, y, group = a, colour = a)) +

  # colour rainbows
  geom_line(size = 1) +
  scale_color_gradientn(colours = rev(rainbow)) +

  # start new scale
  new_scale_colour() +

  # dullbows
  geom_line(aes(x, y, group = a, colour = a), filter(df_legend, !pride), size = 1) +
  scale_color_gradientn(colours = rev(dullbow)) +

  # text
  geom_textpath(aes(x, y+10, label = label), df_textline_legend, colour = txt_col, size = 14, family = ft_text, text_only = TRUE, fontface = "bold") +
  geom_text(
    aes(170 + 50, 0, label = label),
    df_base |>
      filter(company %in% c("Bank of America", "Capital One")) |>
      mutate(label = c("X / Y", "X / Y")),
    colour = txt_col, size = 14, family = ft_text) +
  facet_wrap(~company) +
  ylim(c(-10, 260)) +
  coord_cartesian(clip = "off") +

  # labels and themes
  labs(
    subtitle = "Made the HRC business pledge?",
    caption = "X = $ contributed<br>Y = # of politicians supported<br>More rainbow = Fewer anti-LGBTQ+ politicians supported"
    ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_markdown(hjust = 0.5, family = ft_title, size = 128),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35, margin = margin(b = 0)),
    plot.caption = element_markdown(hjust = 0, family = ft_text, size = 36, lineheight = 0.3, margin = margin(t = 5)),
    plot.margin = margin(t = 00, r = 0, b = 0, l = 0),
    strip.text = element_blank(),
    legend.position = "none"
  )

ggsave("2022/week23-pride/pride-legend.png", height = 12, width = 18)

g_base +
  inset_element(g_legend, left = 0.7, bottom = 1, top = 1.20, right = 0.92)
ggsave("2022/week23-pride/pride-legend.png", height = 12, width = 18)
