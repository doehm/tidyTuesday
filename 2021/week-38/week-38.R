
# week 37 -----------------------------------------------------------------

library(tidyverse)
# library(extrafont)
library(ggtext)
library(patchwork)
library(showtext)
library(cowplot)
# extrafont::loadfonts(quiet = TRUE)

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

metal_df <- audio |>
  filter(str_detect(spotify_genre, "metal")) |>
  mutate(metal_genre = extract_metal(spotify_genre)) |>
  # select(performer, song_id, metal) |>
  unnest(metal_genre)

subgenres <- metal_df |>
  count(metal_genre) |>
  arrange(desc(n)) |>
  slice(1:20) |>
  pull(metal_genre)

metal_df |>
  count(metal_genre) |>
  arrange(desc(n))

metal_df |>
  filter(metal_genre == "nu metal") |>
  count(performer) |>
  arrange(desc(n)) |>
  as.data.frame()



# plot function -----------------------------------------------------------

plot_metal_subgenre <- function(subgenre) {

  bands <- metal_df |>
    filter(metal_genre == subgenre) |>
    count(performer) |>
    arrange(desc(n))

  metal_df |>
    ggplot(aes(x = loudness, y = energy)) +
    geom_point(colour = mass, size = 2) +
    geom_segment(aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2), colour = mass) +
    geom_point(data = filter(metal_df, metal_genre == subgenre), colour = accent, size = 3) +
    geom_segment(
      aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2),
      filter(metal_df, metal_genre == subgenre),
      colour = accent,
      lwd = 1
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = bg),
      text = element_text(colour = font),
      plot.title = element_text(family = ft, hjust = 0.5)
    ) |>
    labs(
      title = toupper(subgenre)
    )

  # plot +
  #   ggsave("./2021/week-38/metal.svg", height = 12, width = 24)

  # list(
  #   bands = bands,
  #   plot = plot
  # )

}

# palettes ----------------------------------------------------------------

font_add_google("Metal Mania")
showtext_auto()

mass <- "grey20"
accent <- sick_palettes$bj[4]
bg <- "black"
font <- "white"

ft <- "Metal Mania"

images <- list(
  thrash = "2021/week-38/metallica.jpg",
  nu = "2021/week-38/korn1.jpg"
)

# plots -------------------------------------------------------------------

all_subgenres <- imap(subgenres, ~{
  out <- plot_metal_subgenre(.x)
  out$plot
  }) |>
  set_names(subgenres)

wrap_plots(all_subgenres)

subgenre <- "nu metal"

fn <- function(subgenre) {


  plot <- metal_df |>
    ggplot(aes(x = loudness, y = energy)) +
    geom_point(colour = mass, size = 2) +
    geom_segment(aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2), colour = mass) +
    geom_point(data = filter(metal_df, metal_genre == subgenre), colour = accent, size = 3) +
    geom_segment(
      aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2),
      filter(metal_df, metal_genre == subgenre),
      colour = accent,
      lwd = 1
    ) +
    # geom_richtext(aes(-1, 0.5, label = "<img style=display: inline-block; src='2021/week-38/metallica.jpg' width='130' height='85'/>")) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black"),
      text = element_text(colour = font),
      plot.title = element_textbox(family = ft, hjust = 0),
      plot.margin = margin(l = 300)
    ) +
    labs(
      title = glue( "<span style='font-size:100px'>{toupper(subgenre)}</span>")
    )

  ggdraw() +
    draw_plot(plot) +
    draw_image(images[[str_remove(subgenre, " metal")]], x = -0.1, y = 0.1, height = 0.6, width = 0.5)

}

fn("thrash metal") / fn("nu metal")
