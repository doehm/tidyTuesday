
# week 37 -----------------------------------------------------------------

library(tidyverse)
library(extrafont)
library(ggtext)
extrafont::loadfonts(quiet = TRUE)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 38)
tt

sick_palettes <- list(
  bj = c("#f4f1de", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f"),
  sunset = c("#ffcdb2", "#ffb4a2", "#e5989b", "#b5838d", "#6d6875")
)

tt$billboard


tt$billboard |>
  count(performer)

tt$audio_features |>
  count(spotify_genre) |>
  View()

audio <- tt$audio_features

extract_metal <- function(x) {
  str_split(x, ",") |>
    map(~{
      genres <- str_remove_all(.x, "[:punct:]") |>
        str_trim()
      metal <- str_detect(genres, "metal")
      genres[metal]
    })
}

extract_metal(audio$spotify_genre)

metal_df <- audio |>
  filter(str_detect(spotify_genre, "metal")) |>
  mutate(metal = extract_metal(spotify_genre)) |>
  # select(performer, song_id, metal) |>
  unnest(metal)

metal_df |>
  mutate(highlight = ifelse(metal == "thrash metal", "thrash metal", "other")) |>
  ggplot(aes(x = loudness, y = energy, colour = highlight, size = highlight)) +
  geom_point() +
  scale_colour_manual(values = c("white", "darkred")) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = "white")
  )
