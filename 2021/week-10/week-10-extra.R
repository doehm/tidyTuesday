library(tidyverse)
library(janitor)
library(glue)
library(survivoR) # devtools::install_github("doehm/survivoR")
library(evoPalette) # devtools::install_github("doehm/evoPalette")
library(rstanarm)
library(snakecase)
library(lubridate)
library(ggtext)
library(tidybayes)
library(extrafont)
extrafont::loadfonts(quiet = TRUE)

#### helpers ####
# there's probably a way better way to do this but I couldn't find it
posterior_sims <- function(object, long = TRUE, ...) {
  nm <- names(object$coefficients)
  np <- length(nm)
  object$stanfit@sim$samples[[1]][1:np] %>%
    set_names(nm) %>%
    as_tibble() %>%
    clean_names() %>%
    mutate_at(vars(!contains("intercept")), ~.x + intercept) %>%
    mutate(iter = 1:n()) %>%
    # slice(100:1000) %>%
    {if(long) pivot_longer(., cols = -iter, names_to = "var", values_to = "beta") else .}
}


#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 10)
df <- tt$youtube %>%
  mutate(likes_per_view = like_count/view_count)

#### stan ####
v <- c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")
dfxx <- df %>%
  mutate_if(is.logical, as.integer) %>%
  filter(likes_per_view > 0) %>%
  select(likes_per_view, brand, view_count, like_count, dislike_count, favorite_count, comment_count, v) %>%
  mutate(
    log_lpv = log(1000*likes_per_view),
    log_views = log(view_count),
    log_likes = log(like_count),
    log_dislike = log(dislike_count),
    log_favourite = log(favorite_count),
    log_comment = log(comment_count)
  )

mod <- stan_glm(
  log_lpv ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex,
  iter = 2000,
  warmup = 1000,
  data = dfxx
)

mod_views <- stan_lmer(
  log_views ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex + (1 | brand),
  iter = 2000,
  warmup = 1000,
  data = dfxx
)
plot(mod_views)

beta_df <- posterior_sims(mod)
beta_df_view <- posterior_sims(mod_views)

# beta_df <- beta_df %>%
#   mutate(model = "lpv") %>%
#   bind_rows(
#     beta_df_view %>%
#       mutate(model = "views")
#     )


mod <- stan_lmer(
  log_lpv ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex + (1 |  brand),
  iter = 2000,
  warmup = 1000,
  data = dfxx
)
plot(mod)

recover_types(mod)

mod %>%
  spread_draws(funny, patriotic, celebrity) %>%
  pivot_longer(cols = c(funny, patriotic, celebrity), names_to = "var", values_to = "draw") %>%
  ggplot(aes(x = draw, y = var)) +
  stat_dots()



posterior_sims(mod) %>%
  filter(str_detect(var, "b_inter")) %>%
  group_by(var) %>%
  summarise(beta = median(beta)) %>%
  arrange(beta)

dfxx %>%
  group_by(brand) %>%
  summarise(beta = mean(log_lpv) - 1.14) %>%
  arrange(beta)


strip_vars <- mod$coefficients %>%
  names() %>%
  to_snake_case()
strip_vars <- strip_vars[str_detect(strip_vars, "b_inter")]
strip_titles <- c("Bud Light", "Budweiser", "Coca Cola", "Doritos", "E-Trade", "Hynudai", "Kia", "NFL", "Pepsi", "Toyota")
names(strip_titles) <- strip_vars

as_tibble(mod) %>%
  clean_names() %>%
  pivot_longer(everything()) %>%
  filter(str_detect(name, "b_inter")) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_density() +
  facet_wrap(~ name, ncol = 10, labeller = labeller(name = strip_titles)) +
  coord_flip() +
  scale_fill_discrete(name = "name", labels = strip_titles) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

as_tibble(mod) %>%
  clean_names() %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(median = median(value)) %>%
  filter(str_detect(name, "b_inter")) %>%
  ggplot(aes(x = name, y = median, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_survivor(25) +
  theme_minimal()


mod_views <- stan_glm(
  log_views ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex,
  iter = 2000,
  warmup = 1000,
  data = dfxx
)

mod_likes <- stan_glm(
  log_likes ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex,
  iter = 2000,
  warmup = 1000,
  data = dfxx
)

mod_dislike <- stan_glm(
  log_dislike ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex,
  iter = 2000,
  warmup = 1000,
  data = filter(dfxx, is.finite(log_dislike))
)

as_tibble(mod_views) %>%
  mutate(model = "views") %>%
  bind_rows(as_tibble(mod_likes) %>% mutate(model = "likes")) %>%
  bind_rows(as_tibble(mod_dislike) %>% mutate(model = "dislikes")) %>%
  clean_names %>%
  select(-intercept, -sigma) %>%
  pivot_longer(cols = -model, names_to = "var", values_to = "beta") %>%
  group_by(model, var) %>%
  summarise(median = median(beta)) %>%
  ggplot(aes(x = var, y = median, fill = var)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_survivor(25) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90)
  )
