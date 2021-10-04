
# week 40 -----------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)

# data --------------------------------------------------------------------

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

# data wrangling ----------------------------------------------------------

n_authors <- paper_authors |>
  count(paper, sort = TRUE, name = "n_authors")

programs <- paper_programs |>
  left_join(programs, by = "program")

df <- papers |>
  left_join(n_authors, by = "paper") |>
  left_join(programs, by = "paper") |>
  drop_na() |>
  group_by(program_desc, program_category, year) |>
  summarise(
    mean_authors = mean(n_authors),
    sd = sd(n_authors)
  )

# fonts and palettes ------------------------------------------------------

col <- list(
  text = "white",
  pal = c("#540d6e", "#ee4266", "#ffd23f"),
  bg = "black",
  strip_bg = "grey10"
)

font_add_google("Inconsolata", "incon")
showtext_auto()

fonts <- list(
  text = "incon",
  scale = 1
)

# text --------------------------------------------------------------------

subtitle <- "Collaboration has increased on NBER papers over the past 40 years. The average number of contributors on papers in the <span style='color:#ffd23f'>Micro</span> program<br>category has been increasing at a faster rate than papers in the <span style='color:#540d6e'>Finance</span> and <span style='color:#ee4266'>Macro/International</span> program categories"

# plot --------------------------------------------------------------------

df |>
  ggplot(aes(year, mean_authors, colour = program_category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~program_desc) +
  labs(
    title = "National Bureau of Economic Research Papers (NBER)",
    subtitle = subtitle,
    y = "Average number of authors per paper",
    colour = "Program category"
  ) +
  scale_colour_manual(values = col$pal) +
  theme_void() +
  theme(
    axis.text = element_text(size = 32),
    axis.text.x = element_text(margin = margin(b = 15)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 30)),
    legend.position = "bottom",
    legend.title = element_text(lineheight = 0.4),
    plot.background = element_rect(fill = col$bg),
    plot.margin = margin(30, 30, 30, 30),
    plot.title = element_text(face = "bold", size = 128),
    plot.subtitle = element_markdown(size = 64, margin = margin(b = 30, t = 15), lineheight = 0.3),
    strip.text = element_text(margin = margin(5, 5, 5, 5)),
    strip.background = element_rect(fill = col$strip_bg),
    text = element_text(colour = col$text, family = fonts$text, size = 48)
  ) +
  ggsave("./2021/week-40/week-40.png", height = 12, width = 24)

