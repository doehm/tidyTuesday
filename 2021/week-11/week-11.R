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

#### helpers ####
slice_grp <- function(., x, n) {
  x <- sym(x)
  dfn <- . %>%
    count(!!x, name = k) %>%
    arrange(desc(k)) %>%
    slice(1:n)
  dfn
}


#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 11)
tt$raw_bechdel

tt$movies %>%
  left_join(select(tt$raw_bechdel, imdb_id, rating), by = "imdb_id") %>%
  select(imdb_id, year, rating, genre) %>%
  filter(year > 1980) %>%
  mutate(genre = str_split(genre, ",")) %>%
  unnest(genre) %>%
  mutate(genre = str_trim(genre)) %>%
  filter(!is.na(genre)) %>%
  mutate(val = 1) %>%
  pivot_wider(id_cols = c(imdb_id, year, rating), names_from = "genre", values_from = "val", values_fill = 0)



  ggplot(aes(x = rating, y = rated, fill = n)) +
  geom_tile(width = 0.8, height = 0.8) +
  theme_void() +
  theme(
    axis.text = element_text()
  ) +
  scale_fill_gradientn(colours = c("red", "white", "blue"))


df <- tibble(x = sample(letters[1:4], 20, TRUE))
df %>%
  count(x) %>%
  arrange(desc(n))

df %>%
  slice_grp(x, 2)


tt$movies %>%
  left_join(select(tt$raw_bechdel, imdb_id, rating), by = "imdb_id")
