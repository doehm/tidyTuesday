# week 37 -----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(showtext)
library(cowplot)
library(ggimage)
library(magick)
library(ggforce)
library(ggfx)
# extrafont::loadfonts(quiet = TRUE)


# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 40)

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

papers
authors |>
  arrange(author)

programs |>
  count(program_category)

paper_programs |>
  count(program)

paper_authors |>
  mutate(val = 1) |>
  pivot_wider(names_from = author, values_from = val)



# data wrangling ----------------------------------------------------------

n_authors <- paper_authors |>
  count(paper, sort = TRUE, name = "n_authors")

programs <- paper_programs |>
  left_join(programs, by = "program")

df <- papers |>
  left_join(n_authors, by = "paper") |>
  left_join(programs, by = "paper") |>
  drop_na() |>
  group_by(program_desc, year) |>
  summarise(
    mean_authors = mean(n_authors),
    sd = sd(n_authors)
    ) |>
  mutate(
    lower = mean_authors - 1.96*sd,
    upper = mean_authors + 1.96*sd,
    lower80 = mean_authors - qnorm(0.9)*sd,
    upper80 = mean_authors + qnorm(0.9)*sd
  )

col <- list(
  line = "grey80"
)

df |>
  ggplot(aes(year, mean_authors)) +
  # geom_line() +
  geom_segment(aes(x = year, xend = year, y = lower, yend = upper), size = 0.5, colour = col$line) +
  geom_segment(aes(x = year, xend = year, y = lower80, yend = upper80), size = 1, colour = col$line) +
  geom_point(colour = col$line) +
  facet_wrap(~program_desc) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )

dat <- papers |>
  left_join(n_authors, by = "paper") |>
  left_join(programs, by = "paper") |>
  drop_na() |>
  mutate(
    year = as.factor(year),
    program_desc = as.factor(program_desc)
    ) |>
  filter(program_desc %in% c("Public Economics", "Economic Fluctuations and Growth", "Health Economics", "International Trade and Investment"))

library(rstanarm)

mod <- stan_glm(n_authors ~ year + program_desc, data = dat)
pred <- predict(mod, newdata = distinct(dat, year, program_desc), se.fit = TRUE)
tibble(
  mean_authors = pred$fit,
  lower = pred$fit - 1.96*pred$se.fit,
  upper = pred$fit + 1.96*pred$se.fit,
  lower80 = pred$fit - qnorm(0.9)*pred$se.fit,
  upper80 = pred$fit + qnorm(0.9)*pred$se.fit
  ) |>
  # mutate_if(is.numeric, exp) |>
  bind_cols(distinct(dat, year, program_desc)) |>
  ggplot(aes(year, mean_authors)) +
  # geom_line() +
  geom_segment(aes(x = year, xend = year, y = lower, yend = upper), size = 0.5) +
  geom_segment(aes(x = year, xend = year, y = lower80, yend = upper80), size = 1) +
  geom_point() +
  facet_wrap(~program_desc) +
  theme_minimal()


posterior_interval(mod, prob = 0.95)
posterior_predict(mod, newdata = distinct(dat, year, program_desc))


mod <- MCMCpoisson(n_authors ~ year + program_desc, data = dat)

predict(mod)
