library(tidyverse)
library(janitor)
library(glue)
library(survivoR) # devtools::install_github("doehm/survivoR")
library(evoPalette) # devtools::install_github("doehm/evoPalette")
library(rstanarm)
library(snakecase)
library(lubridate)
library(ggtext)
library(extrafont)
extrafont::loadfonts(quiet = TRUE)


#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 10)
df <- tt$youtube %>%
  mutate(likes_per_view = like_count/view_count)

#### stan ####
v <- c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")
dfxx <- df %>%
  mutate_if(is.logical, as.integer) %>%
  filter(likes_per_view > 0) %>%
  select(likes_per_view, brand, view_count, v) %>%
  mutate(
    log_lpv = log(1000*likes_per_view),
    log_views = log(view_count)
  )

mod <- stan_glm(
  log_lpv ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex,
  iter = 2000,
  warmup = 1000,
  data = dfxx
)

beta_df <- as_tibble(mod) %>%
  clean_names() %>%
  select(-sigma) %>%
  pivot_longer(cols = everything(), names_to = "var", values_to = "beta")


#### fonts ####
ftc <- "Verdana Pro Cond Black"
ft <- "Verdana Pro Cond Light"
font_size_low <- 14
font_size_high <- 26


#### palette ####
season <- 25
sp <- season_palettes$palette[[season]]
font_col <- sp[1]
bg <- colorRampPalette(c("black", sp[4]))(8)
show_palette(sp)

#### titles ####
wd <- 180
title <- str_wrap("Median Weekly Earnings", wd/3)
subtitle1 <- str_wrap("
  Superbowl ads that feature celebrities typically see an increase of 2.1 likes per 1000 views on Youtube.
  To a lesser degree ads featuring a certain level of danger or featuring animals tend to get higher
  number of likes / views. Curiously, ads that are funny (or try to be) and use
  sex tend to get lower number of likes / views. However, ads that are funny or are in some
  way patriotic tend to get more views overall, but proportionally less likes.
", 180)
subtitle2 <- str_wrap("Feature effects are estimated using rstanarm::stan_glm and 2000 sims. Each line is a
                      draw from the posterior.", 180)
subtitle <- paste(subtitle1, "\n\n", subtitle2)
strip_titles <- str_wrap(to_title_case(c("intercept", v)), 10)

stats_df <- beta_df %>%
  group_by(var) %>%
  summarise(median = median(beta)) %>%
  mutate(
    intercept = 1.22,
    exp_median = ifelse(var == "intercept", exp(intercept), exp(median + intercept) - exp(intercept))
    ) %>%
  mutate_if(is.numeric, ~round(.x, 1)) %>%
  mutate(
    median = ifelse(var == "intercept", 3.3, median),
    op = case_when(
      var == "intercept" ~ "",
      median < 0 ~ "",
      TRUE ~ "+"
      ),
    label_info = glue("{op}{exp_median}")
  ) %>%
  mutate(var = factor(var, levels = c("intercept", v))) %>%
  arrange(var)


#### labels ####
labels_df <- tibble(
  x = 8.5,
  y = 0,
  x_info = -0.5,
  labels = strip_titles,
  labels_info = stats_df$label_info,
  var = c("intercept", v)
) %>%
  mutate(var = factor(var, levels = c("intercept", v)))


#### plot ####
beta_df %>%
  mutate(var = factor(var, levels = c("intercept", v))) %>%
  # group_by(var) %>%
  # summarise(beta = median(beta)) %>%
  # sample_n(200) %>%
  ungroup() %>%
  ggplot(aes(x = beta, colour = var)) +
  geom_vline(mapping = aes(xintercept = exp(beta), colour = var), alpha = 0.07) +
  geom_vline(xintercept = 3.35, linetype = 2, colour = font_col, size = 1) +
  geom_text(data = labels_df, mapping = aes(x = x, y = y, label = labels, family = ft), colour = font_col) +
  geom_text(data = labels_df, mapping = aes(x = x_info, y = y, label = labels_info, family = ft), colour = font_col, size = 8) +
  facet_wrap(~ var, ncol = 8) +
  scale_colour_survivor(25) +
  coord_flip(clip = "off") +
  labs(
    title = "Which Content Increases <span style = 'color:#7B2B49'>Likes per View</span> for Ads Featured During the Superbowl?",
    subtitle = subtitle,
    x = "Likes\nper\n1000\nviews",
    y = "Likes / 1000 Views on Youtube",
    caption = "Source: @FiveThrityEight / Graphic: @danoehm"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = font_col),
    plot.title = element_markdown(family = ftc, size = font_size_high),
    plot.subtitle = element_text(margin = margin(b = 40, t = 20)),
    panel.spacing = unit(2, "cm"),
    plot.margin = margin(t = 20, b = 20, l = 50, r = 50),
    strip.placement = "bottom",
    strip.text = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = bg[3]),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_blank(),
    plot.caption = element_text()
  ) +
  ggsave(glue("./2021/week-10/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} superbowl.png"), height = 8, width = 16)










#### dev space ####
#### plot ####
df$youtube %>%
  ggplot(aes(x = log(view_count), y = log(like_count), colour = patriotic)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_colour_survivor(16)

df$youtube %>%
  ggplot(aes(x = log(view_count), fill = patriotic)) +
  geom_density(alpha = 0.8) +
  scale_fill_survivor(16)

ggsave(glue("./2021/week-9/plots/earn-{format(now(), '%Y%m%d-%H%M%S')}.png"), type = "cairo", height = 20, width = 8.72)

mod <- lm(log(view_count) ~ funny + celebrity + use_sex + patriotic + danger + animals, data = df$youtube)
summary(mod)
plot(mod)


mod <- lm(log(view_count) ~ funny:celebrity, data = df$youtube)
summary(mod)
plot(mod)

dfx <- df %>%
  mutate_if(is.logical, as.integer) %>%
  filter(likes_per_view > 0)
fm <- as.formula(~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex)
mod <- lm(log(likes_per_view) ~ funny + show_product_quickly + patriotic + celebrity + danger + animals + use_sex, data = dfx)
summary(mod)

df %>%
  select(id, likes_per_view, funny, show_product_quickly, patriotic, celebrity, danger, animals, use_sex) %>%
  pivot_longer(cols = c(-id, -likes_per_view), names_to = "feature", values_to = "vals") %>%
  # filter(vals) %>%
  ggplot(aes(x = log(likes_per_view), fill = vals)) +
  geom_density() +
  facet_wrap(~feature)


v <- c("funny", "show_product_quickly", "patriotic", "celebrity", "danger", "animals", "use_sex")
map(v, ~{
  x <- enquo(.x)
  dfx %>%
    filter(!!x) %>%
    summarise_if(is.logical, mean)
})
df_tile <- rbind(
df %>% filter(funny) %>% summarise_if(is.logical, mean) %>% mutate(feature = "funny"),
df %>% filter(show_product_quickly) %>% summarise_if(is.logical, mean) %>% mutate(feature = "show_product_quickly"),
df %>% filter(patriotic) %>% summarise_if(is.logical, mean) %>% mutate(feature = "patriotic"),
df %>% filter(celebrity) %>% summarise_if(is.logical, mean) %>% mutate(feature = "celebrity"),
df %>% filter(danger) %>% summarise_if(is.logical, mean) %>% mutate(feature = "danger"),
df %>% filter(animals) %>% summarise_if(is.logical, mean) %>% mutate(feature = "animals"),
df %>% filter(use_sex) %>% summarise_if(is.logical, mean) %>% mutate(feature = "use_sex"))

df_tile <- rbind(
  df %>% filter(funny) %>% summarise(lpv = mean(likes_per_view, na.rm = TRUE)) %>% mutate(feature = "funny"),
  df %>% filter(show_product_quickly) %>% summarise_if(is.logical, mean) %>% mutate(feature = "show_product_quickly"),
  df %>% filter(patriotic) %>% summarise_if(is.logical, mean) %>% mutate(feature = "patriotic"),
  df %>% filter(celebrity) %>% summarise_if(is.logical, mean) %>% mutate(feature = "celebrity"),
  df %>% filter(danger) %>% summarise_if(is.logical, mean) %>% mutate(feature = "danger"),
  df %>% filter(animals) %>% summarise_if(is.logical, mean) %>% mutate(feature = "animals"),
  df %>% filter(use_sex) %>% summarise_if(is.logical, mean) %>% mutate(feature = "use_sex"))

df_tile %>%
  pivot_longer(cols = -feature) %>%
  mutate(value = ifelse(value == 1, 0, value)) %>%
  ggplot(aes(x = name, y = feature, fill = value)) +
  geom_tile(width = 0.9, height = 0.9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    legend.position = "none"
  ) +
  scale_fill_gradient(low = c1, high = c2)


cor(select(df, v))
c1 <- "black"
c2 <- season_palettes$palette[[25]][5]
fn <- function(df, v, u) {
  x1 <- sym(v)
  x2 <- sym(u)
  df %>%
    filter(!!x1, !!x2) %>%
    summarise(lpv = mean(likes_per_view, na.rm = TRUE)) %>%
    mutate(x1 = v, x2 = u)
}
vu <- expand_grid(v = v, u = v)
map2_dfr(vu$v, vu$u, ~{
  fn(df, .x, .y)
  }) %>%
  ggplot(aes(x = x1, y = x2, fill = lpv)) +
  geom_tile(width = 0.9, height = 0.9) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    legend.position = "none"
  ) +
  scale_fill_gradient(low = c1, high = c2)


log(y) ~ b0 + b1x
y = exp(b0 + b1)

1.7 = 1.2 + 0.5
exp(1.7) = exp(1.2)*exp(0.5)
