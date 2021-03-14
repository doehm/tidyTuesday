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
library(cowplot)
library(magick)
extrafont::loadfonts(quiet = TRUE)

#### helpers ####
# theres probsbly a tidyverse way to do this already
# oh well made my own
slice_grp <- function(df, x, n) {
  x <- enquo(x)
  dfn <- df %>%
    count(!!x, name = "k") %>%
    arrange(desc(k)) %>%
    slice(1:n)
  df %>%
    semi_join(dfn)
}

minmax <- function(x, a, b) {
  (x - min(x))/(max(x) - min(x))*(b - a) + a
}

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 11)
df <- tt$movies %>%
  left_join(select(tt$raw_bechdel, imdb_id, rating), by = "imdb_id")

#### genre ####
df_genre <- tt$movies %>%
  left_join(select(tt$raw_bechdel, imdb_id, rating), by = "imdb_id") %>%
  select(imdb_id, title, year, rating, binary, genre) %>%
  filter(year > 1980) %>%
  mutate(genre = str_split(genre, ",")) %>%
  unnest(genre) %>%
  mutate(genre = str_trim(genre)) %>%
  filter(!is.na(genre)) %>%
  slice_grp(genre, 12) %>%
  mutate(
    val = 1,
    pass = as.numeric(binary == "PASS"),
    rating_of_3 = as.numeric(rating == 3)
    ) %>%
  pivot_wider(id_cols = c(imdb_id, title, year, rating_of_3, pass), names_from = "genre", values_from = "val", values_fill = 0) %>%
  clean_names() %>%
  select(-year, -pass)


#### stan model ####
mod <- stan_glm(rating_of_3 ~ ., data = select(df_genre, -imdb_id, -title), family = binomial(link = "logit"))
summary(mod)

#### actors ####
df %>%
  mutate(actors = str_split(actors, ",")) %>%
  select(imdb_id, year, rating, title, binary, actors) %>%
  unnest(actors) %>%
  mutate(actors = str_trim(actors)) %>%
  filter(str_detect(actors, "Van Damme|Schwarzenegger|Willis")) %>%
  left_join(df_genre, by = c("imdb_id", "title")) %>%
  group_by(actors) %>%
  summarise(
    movies_on_db = n(),
    rating_mean = mean(rating),
    n3 = sum(rating == 3),
    pass = sum(binary == "PASS")
  ) %>%
  filter(!is.na(actors)) %>%
  arrange(desc(pass))

#### top actors in genre ####
genres <- names(df_genre)[!names(df_genre) %in% c("imdb_id", "title", "rating_of_3")]
df_act_gen <- df %>%
  mutate(actors = str_split(actors, ",")) %>%
  select(imdb_id, year, rating, title, binary, actors) %>%
  unnest(actors) %>%
  mutate(actors = str_trim(actors)) %>%
  left_join(df_genre, by = c("imdb_id", "title")) %>%
  ungroup()

n <- 5
df_top_act <- map_dfr(genres, function(genre) {
    df_act_gen %>%
    filter_at(vars(genre), ~.x == 1) %>%
    group_by(actors) %>%
    summarise(n3 = sum(rating_of_3)) %>%
    arrange(desc(n3)) %>%
    head(n) %>%
    mutate(
      genre = genre,
      y = n:1,
      x = 0
      )
  })

#### model intervals ####
df_ints <- mod %>%
  as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "genre", values_to = "sim") %>%
  group_by(genre) %>%
  summarise(
    q025 = quantile(sim, 0.025),
    q05 = quantile(sim, 0.05),
    q10 = quantile(sim, 0.1),
    q50 = quantile(sim, 0.5),
    q90 = quantile(sim, 0.9),
    q95 = quantile(sim, 0.95),
    q975 = quantile(sim, 0.975)
  ) %>%
  filter(genre != "(Intercept)") %>%
  pivot_longer(cols = -genre, names_to = "q", values_to = "val") %>%
  ungroup() %>%
  mutate(val = minmax(val, -1, 0.5)) %>%
  pivot_wider(id_cols = genre, names_from = q, values_from = val) %>%
  mutate(n3 = 1) %>%
  left_join(tibble(genre = genres, beta = coefficients(mod)[-1]), by = "genre")


#### plot actors by genres ####
ftb <- "Verdana Pro Cond Black"
ft <- "Verdana Pro Cond Light"
ssn <- 5
cols <- season_palettes$palette[[ssn]]
bg <- colorRampPalette(c("white", cols[1]))(8)[3]
wd <- 120
subtitle1 <- str_wrap(
  "The top 5 actors who have appeared in the most movies which scored a 3 on the Bechdel scale are shown for each
  genre. The data includes 1528 movies released between 1980 and 2013 and the top 12 most popular genres.",
  wd
)
subtitle2 <- str_wrap(
  "Drama is the most prevalent movie genre but not the most likely to achieve a Bechdel rating of 3, that crown
  goes to horror. Romance and fantacy fare well and action gets the wooden spoon.",
  wd
)
subtitle3 <- str_wrap(
  "The bar below the text represents the credible interval at 80%, 90% and 95%. Positive values mean that genre is more likely to achieve a rating of 3, and negative
  values less likely",
  wd
)
subtitle <- paste0(subtitle1, "\n\n", subtitle2, "\n\n", subtitle3)
df_top_act %>%
  ggplot(aes()) +
  geom_text(mapping = aes(x = -1, y = 2, label = toupper(to_title_case(genre))), hjust = 0, size = 14, fontface = "italic", alpha = 0.04, family = ftb) +
  geom_text(mapping = aes(x = x + 0.25, y = y, label = actors), hjust = 1, family = ft, fontface = "italic") +
  geom_text(mapping = aes(x = 0*n3 + 0.45, y = y, label = n3), hjust = 1, family = ftc, fontface = "italic", size = 6) +
  geom_rect(mapping = aes(xmin = -1, xmax = 0.5, ymin = -1.5, ymax = 0), colour = "grey60", fill = NA) +
  geom_rect(mapping = aes(xmin = q025, xmax = q975, ymin = -1.5, ymax = 0, fill = q50), data = df_ints, alpha = 0.3) +
  geom_rect(mapping = aes(xmin = q05, xmax = q95, ymin = -1.5, ymax = 0, fill = q50), data = df_ints, alpha = 0.3) +
  geom_rect(mapping = aes(xmin = q10, xmax = q90, ymin = -1.5, ymax = 0, fill = q50), data = df_ints, alpha = 0.4) +
  geom_segment(aes(x = -0.296, xend = -0.296, y = -1.5, yend = 0), linetype = 2) +
  geom_text(aes(x = q50, y = -2, label = round(beta, 1)), data = df_ints, fontface = "italic") +
  facet_wrap(~ genre, ncol = 2) +
  labs(
    title = "BECHDEL TEST",
    subtitle = subtitle,
    caption = "Data: @FiveThirtyEight / Graphic: @danoehm"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "italic", family = ftc, size = 56, margin = margin(t = 20, b = 20)),
    plot.subtitle = element_text(family = ft, margin = margin(b = 20), size = 12),
    plot.caption = element_text(family = ft),
    plot.background = element_rect(fill = bg),
    strip.text = element_blank(),
    panel.spacing = unit(2, "cm"),
    plot.margin = margin(t = 20, b = 20, l = 40, r = 40),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off", xlim = c(-1, 0.5)) +
  scale_fill_gradient(low = cols[2], high = cols[5]) +
  ggsave(glue("./2021/week-11/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} bechdel.png"), height = 16.5, width = 10)





#### extra ###
#### bond ####
bond_movies <- c(
  "casino royal",
  "quantum",
  "skyfall",
  "die another day",
  "world is not enough",
  "tomorrow never",
  "goldeneye",
  "licence to kill",
  "the living day",
  "a view to a kill",
  "octopussy",
  "for your eyes only",
  "moonraker",
  "the spy who loved me",
  "the man with the golden done",
  "live and let die",
  "diamonds are forever",
  "on her majesty",
  "you only live twice",
  "thunderball",
  "goldfinger",
  "from russia with love",
  "dr. no"
)

df_bond <- tt$raw_bechdel %>%
  filter(str_detect(tolower(title), paste0(bond_movies, collapse = "|"))) %>%
  filter(!(year == 1967 & title == "Casino Royale")) %>%
  select(title, rating, year) %>%
  arrange(desc(rating)) %>%
  group_by(rating) %>%
  mutate(
    label = str_wrap(paste0(title, " (", year, ")"), 22),
    x = 3 - rating,
    y = 1:n() - n()
    )

#### bond plot ####
ht <- 5
bond <- image_read("C:/Users/Dan/Pictures/tidy-tuesday/week-11/bond-gun-barrel.jpg")
g_rect <-ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 11), fill = "black", alpha = 0.4) +
  theme_void()
  # coord_cartesian(clip = "off")
g_bg <- ggplot() +
  # geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 11), fill = "white", alpha = 0.4) +
  geom_text(data = df_bond, mapping = aes(x = 1, y = y, label = label), fontface = "italic", family = ftc, hjust = 1, size = 2.5, colour = "white", lineheight = 0.8) +
  facet_wrap(~ rating, ncol = 3) +
  theme_void() +
  labs(
    title = "The Name's Bond ... James Bond",
    subtitle = "Bond films and their Bechdel ratings"
  ) +
  theme(
    text = element_text(colour = "white"),
    plot.title = element_text(family = ftc, face = "italic", size = 24, hjust = 0.5, margin = margin(b = 10, t = 10)),
    plot.subtitle = element_text(family = ftc, face = "italic", size = 16, hjust = 0.5, margin = margin(b = 20, t = 10)),
    plot.margin = margin(l = 60, r = 20, b = 20),
    strip.text = element_text(family = ftc, face = "italic", size = 18, colour = "white")
  ) +
  coord_cartesian(clip = "off", xlim = c(-2, 3))
  # ggsave(glue("./2021/week-11/bond/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} bond.png"), height = ht, width = ht*1.5)

ggdraw() +
  draw_image(bond) +
  draw_plot(g_rect, x = -0.2, y = -0.2, width = 1.5, height = 1.5) +
  draw_plot(g_bg) +
  ggsave(glue("./2021/week-11/bond/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} bond.png"), height = ht, width = ht*1.5)



#### van damme, shwarzenegger, willis ####


tt$raw_bechdel %>%
  filter(str_detect(tolower(title), "john wick")) %>%
  select(title, rating)

df %>%
  mutate(actors = str_split(actors, ",")) %>%
  select(year, rating, title, binary, actors) %>%
  unnest(actors) %>%
  mutate(actors = str_trim(actors)) %>%
  filter(str_detect(actors, "Van Damme|Schwarzenegger|Willis")) %>%
  group_by(actors) %>%
  summarise(
    rating_mean = mean(rating),
    n3 = sum(rating == 3),
    pass = sum(binary == "PASS")
    )

