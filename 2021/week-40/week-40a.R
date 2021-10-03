# week 37 -----------------------------------------------------------------

library(rstanarm)
library(tidyverse)
library(glue)
library(ggtext)
library(showtext)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 40)

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
  ) |>
  mutate(
    lower = mean_authors - 1.96*sd,
    upper = mean_authors + 1.96*sd,
    lower80 = mean_authors - qnorm(0.9)*sd,
    upper80 = mean_authors + qnorm(0.9)*sd
  )

# fonts and palettes ------------------------------------------------------

col <- list(
  line = "grey80",
  text = "white",
  pal = c("#540d6e", "#ee4266", "#ffd23f")
)

font_add_google("Karla", "karla")
font_add_google("Inconsolata", "incon")
showtext_auto()

fonts <- list(
  text = "incon",
  scale = 1
)

# text --------------------------------------------------------------------

subtitle <- "The average number of authors on an NBER paper has increased over time. The number of contributors on papers in the <span style='color:#ffd23f'>Micro</span> program<br>category has been increasing at a faster rate than papers in the <span style='color:#540d6e'>Finance</span> and <span style='color:#ee4266'>Macro/International</span> program categories"

# plot --------------------------------------------------------------------


df |>
  ggplot(aes(year, mean_authors, colour = program_category)) +
  geom_line(size = 1) +
  # geom_segment(aes(x = year, xend = year, y = lower, yend = upper), size = 0.5) +
  # geom_segment(aes(x = year, xend = year, y = lower80, yend = upper80), size = 1) +
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
    plot.title = element_text(face = "bold", size = 128),
    plot.subtitle = element_markdown(size = 64, margin = margin(b = 30, t = 15), lineheight = 0.3),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(30, 30, 30, 30),
    strip.text = element_text(),
    text = element_text(colour = col$text, family = fonts$text, size = 48),
    legend.title = element_text(lineheight = 0.4),
    legend.position = "bottom"
  ) +
  ggsave("./2021/week-40/week-40.png", height = 12, width = 24)



# random ------------------------------------------------------------------



dat <- papers |>
  left_join(n_authors, by = "paper") |>
  left_join(programs, by = "paper") |>
  drop_na() |>
  # mutate(year = year - 1975) |>
  # filter(program_category == "Micro")
  # filter(program_category == "Finance")
  filter(program_category == "Macro/International")

mod <- glm(n_authors ~ year, data = dat)
summary(mod)
new_data <- expand_grid(
  year = c(5, 45),
  program_category = unique(df$program_category)
)
predict(mod, newdata = new_data)

new_data |>
  bind_cols(tibble(y_pred = predict(mod, newdata = new_data))) |>
  mutate(year = year + 1975) |>
  ggplot(aes(x = year, y = y_pred, colour = program_category)) +
  geom_line() +
  geom_point(size = 8)


# stan model --------------------------------------------------------------

dat <- papers |>
  left_join(n_authors, by = "paper") |>
  left_join(programs, by = "paper") |>
  drop_na() |>
  # filter(year > 2010) |>
  mutate(
    # year = as.factor(year),
    year = year - 1975,
    program_desc = as.factor(program_desc)
  ) |>
  filter(program_desc %in% c("Public Economics", "Economic Fluctuations and Growth", "Health Economics", "International Trade and Investment"))

mod <- stan_glm(n_authors ~ year + program_desc, data = dat)
plot(mod)
pred <- predict(mod, newdata = distinct(dat, year, program_desc), se.fit = TRUE)
tibble(
  mean_authors = pred$fit,
  lower = pred$fit - 1.96*pred$se.fit,
  upper = pred$fit + 1.96*pred$se.fit,
  lower80 = pred$fit - qnorm(0.9)*pred$se.fit,
  upper80 = pred$fit + qnorm(0.9)*pred$se.fit
) |>
  mutate_if(is.numeric, exp) |>
  bind_cols(distinct(dat, year, program_desc)) |>
  ggplot(aes(year, mean_authors)) +
  geom_segment(aes(x = year, xend = year, y = lower, yend = upper), size = 0.5, colour = col$line) +
  geom_segment(aes(x = year, xend = year, y = lower80, yend = upper80), size = 1, colour = col$line) +
  geom_point(colour = col$line) +
  facet_wrap(~program_desc) +
  theme_minimal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )


posterior_interval(mod, prob = 0.95)
x <- posterior_predict(mod, newdata = distinct(dat, year, program_desc))


apply(x, 2, mean)

mod <- MCMCpoisson(n_authors ~ year + program_desc, data = dat)

predict(mod)
