
# week 37 -----------------------------------------------------------------

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(showtext)
library(cowplot)
library(ggimage)
library(magick)
# extrafont::loadfonts(quiet = TRUE)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 38)
tt

sick_palettes <- list(
  bj = c("#f4f1de", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f"),
  sunset = c("#ffcdb2", "#ffb4a2", "#e5989b", "#b5838d", "#6d6875")
)

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
  filter(metal_genre == "speed metal") |>
  count(performer) |>
  arrange(desc(n)) |>
  as.data.frame()

# palettes ----------------------------------------------------------------

font_add_google("Metal Mania")
font_add_google("Oxygen")
showtext_auto()

mass <- "grey20"
accent <- sick_palettes$bj[4]
bg <- "black"
font <- "white"

ft <- "Metal Mania"
ft_sub <- "Oxygen"

images <- list(
  thrash = "2021/week-38/metallica.jpg",
  nu = "2021/week-38/korn1.jpg",
  groove = "2021/week-38/dimebag1.jpg",
  speed = "2021/week-38/megadeth.jpg",
  progressive = "2021/week-38/tool2.jpg"
)

image_scale <- list(
  thrash = 1,
  nu = 1.2,
  groove = 1,
  speed = 1,
  progressive = 1.5
)

# plots -------------------------------------------------------------------
style <- "<span style='color:#ffffff; font-size:48px'>"
size <- 64
text <- glue("<span style='color:#ffffff; font-size:72px'>Comparison of loudness, energy and valence of heavy metal subgenres that have featured on the Billboard Top 100</span><br>")
footnote_text <- glue("<span style='font-size:{size}px; color:{accent}'>Thrash Metal</span>: {style}
(or simply thrash) is an extreme subgenre of heavy metal music characterized by its overall aggression and<br>
often fast tempo, low-register guitar riffs and overlaid with shredding-style lead guitar work</span><br>
<span style='font-size:{size}px; color:{accent}'>Nu Metal</span>: {style}
is a subgenre of alternative metal that combines elements of heavy metal music with elements of other music<br>
genres such as hip hop, alternative rock, funk, industrial, and grunge</span><br>
<span style='font-size:{size}px; color:{accent}'>Groove Metal</span>: {style}
is inspired by thrash metal and traditional heavy metal, groove metal features raspy singing and screaming,<br>
down-tuned guitars, heavy guitar riffs, and syncopated rhythms</span><br>
<span style='font-size:{size}px; color:{accent}'>Speed Metal</span>: {style}
is usually considered less abrasive and more melodic than thrash metal, showing less influence from<br>
hardcore punk. However, speed metal is usually faster and more aggressive than traditional heavy metal,<br>
also showing more inclination to virtuoso soloing and featuring short instrumental passages between couplets</span><br>
<span style='font-size:{size}px; color:{accent}'>Progressive Metal</span>: {style}
(sometimes shortened to prog metal) is a broad fusion music genre melding heavy metal and progressive<br>
rock, combining the loud aggression and amplified guitar-driven sound of the former with the more<br>
experimental, cerebral or pseudo-classical compositions of the latter</span>")

ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = font),
    plot.title = element_text(family = ft, hjust = 0.5, size = 250),
    plot.subtitle = element_text(family = ft_sub, hjust = 0.5, size = 48),
    plot.margin = margin(t = 40)
  ) +
  labs(title = "Subgenres of Heavy Metal", subtitle = "Comparison of loudness, energy and valence of 5 heavy metal subgenres that have songs featured on the Billboard Top 100") +
  coord_cartesian(clip ="off") +
  ggsave(glue("./2021/week-38/title text.png"), height = 4, width = 14)

ggplot() +
  geom_richtext(aes(0, 0, label = footnote_text), fill = "black", family = ft_sub, hjust = 0, lineheight = 2) +
  xlim(0, 15) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = font),
  ) +
  coord_cartesian(clip ="off") +
  ggsave(glue("./2021/week-38/footnote text.png"), height = 4, width = 10)

fn <- function(subgenre) {

  plot <- metal_df |>
    ggplot(aes(x = loudness, y = energy)) +
    geom_point(colour = mass, size = 2) +
    geom_segment(aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2), colour = mass, lwd = 0.25) +
    geom_point(data = filter(metal_df, metal_genre == subgenre), colour = accent, size = 3) +
    geom_segment(
      aes(x = loudness, xend = loudness, y = energy - valence/2, yend = energy + valence/2),
      filter(metal_df, metal_genre == subgenre),
      colour = accent,
      lwd = 0.5
    ) +
    geom_text(aes(2, 0.75, label = str_wrap(subgenre, 5)), size = 48, colour = font, family = ft, hjust = 0.5, lineheight = 0.2) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black"),
      text = element_text(colour = font),
      plot.title = element_text(family = ft, hjust = 1, size = 250, margin = margin(t = 40)),
      plot.subtitle = element_text(family = ft_sub, hjust = 1, size = 48, margin = margin(b = 30, t = 20)),
      plot.margin = margin(l = 200, r = 80)
    ) +
    coord_cartesian(clip = "off")

  # plot
  short_name <- str_remove(subgenre, " metal")
  ggdraw() +
    draw_plot(plot) +
    draw_image(images[[short_name]], x = -0.125, y = 0.1, height = 0.6, width = 0.5, scale = image_scale[[short_name]]) +
    ggsave(glue("./2021/week-38/final {subgenre}.png"), height = 4, width = 14)

}

fn("thrash metal")
fn("nu metal")
fn("groove metal")
fn("speed metal")
fn("progressive metal")

ggplot() +
  geom_point(aes(0, 0), colour = accent, size = 5) +
  geom_segment(aes(x = 0, xend = 0, y = -0.8, yend = 0.8), colour = accent) +
  annotate("segment", x = -1, xend = 1, y = -1.1, yend = -1.1, arrow = arrow(type = "closed", angle = 20, length = unit(3, "mm")), colour = "grey50") +
  annotate("segment", x = -1, xend = -1, y = -1.1, yend = 1.1, arrow = arrow(type = "closed", angle = 20, length = unit(3, "mm")), colour = "grey50") +
  annotate("text", x = 0, y = -1.2, label = "Loudness", colour = "grey50", size = 10) +
  annotate("text", x = -1.1, y = 0, label = "Energy", colour = "grey50", angle = 90, size = 10) +
  annotate(
    geom = "curve", x = -0.5, y = 0.2, xend = -0.05, yend = -0.05, colour = "grey50",
    curvature = .3, arrow = arrow(length = unit(3, "mm"), type = "closed", angle = 20)
  ) +
  annotate("text", x = -0.5, y = 0.3, label = "Every point is a song", colour = "grey50", size = 10) +
  annotate(
    geom = "curve", x = 0.5, y = 0.2, xend = 0.05, yend = 0.8, colour = "grey50",
    curvature = .3, arrow = arrow(length = unit(3, "mm"), type = "closed", angle = 20)
  ) +
  annotate(
    geom = "curve", x = 0.5, y = -0.2, xend = 0.05, yend = -0.8, colour = "grey50",
    curvature = -.3, arrow = arrow(length = unit(3, "mm"), type = "closed", angle = 20)
  ) +
  annotate("text", x = 0.5, y = 0, label = "Longer the line the\nmore positive the song", colour = "grey50", size = 10, lineheight = 0.4) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    text = element_text(colour = font, family = ft_sub),
    plot.margin = margin(t = 50, b = 50, l = 20, r = 20),
    plot.caption = element_text(margin = margin(t = 20), hjust = 0.5,size = 24, family = ft_sub, lineheight = 0.5)
  ) +
  labs(caption = "Data: Data.World, Sean Miller, Billboard.com and Spotify \n Graphic: @danoehm / Text: Wikipedia.com") +
  ggsave("./2021/week-38/legend.png", height = 4, width = 4)

ftnote <- image_read("./2021/week-38/footnote text.png")
lgnote <- image_read("./2021/week-38/legend.png")

image_write(image_append(c(ftnote, lgnote)), path = "./2021/week-38/bottom.png")

title <- image_read("./2021/week-38/title text.png")
x1 <- image_read("./2021/week-38/final thrash metal.png")
x2 <- image_read("./2021/week-38/final nu metal.png")
x3 <- image_read("./2021/week-38/final groove metal.png")
x4 <- image_read("./2021/week-38/final speed metal.png")
x5 <- image_read("./2021/week-38/final progressive metal.png")
botttom <- image_read("./2021/week-38/bottom.png")

final <- image_append(c(title, x1, x2, x3, x4, x5, botttom), stack = TRUE)

image_write(final, path = "./2021/week-38/final.png")
image_write(final, path = "./2021/week-38/final.svg")


