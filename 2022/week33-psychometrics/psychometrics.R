# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(ggtext)
library(showtext)
library(glue)

# load data ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2022, week = 33)

# fonts and palettes ------------------------------------------------------

txt_col <- "white"
txt_col2 <- "grey20"

font_add_google("Mukta", "mukta")
font_add_google("Merriweather", "mw")
showtext_auto()
ft_text <- "mukta"
ft2 <- "mw"

green <- "#0D5134"

# wrangle -----------------------------------------------------------------

top10 <- tt$psych_stats |>
  filter(uni_id == "BB") |>
  group_by(question) |>
  summarise(
    n = n(),
    num_ratings = sum(number_ratings)
  ) |>
  arrange(desc(num_ratings)) |>
  slice_head(n = 10)

a <- 1
df_base <-
  tt$psych_stats |>
  filter(uni_id == "BB") |>
  semi_join(top10, by = "question") |>
  mutate(
    y = as.numeric(factor(question)) - 1,
    x = ifelse(str_locate(question, personality)[,1] > 1, 2, 1)+a,
    xmax = x+0.95,
    ymax = y+0.95,
    left_text = str_to_title(str_extract(question, "[:alpha:]+")),
    right_text = str_to_title(str_extract(question, "(?<=/).+")),
    x_text = x + 0.45,
    y_text = y + 0.45,
    x_left_text = 0.5+a,
    x_right_text = 3.5+a,
    square_text = str_to_title(str_sub(personality, 1, 2))
    ) |>
  group_by(char_name, question) |>
  slice_head() |>
  as.data.frame()

df_title <- tribble(
  ~x, ~xmax, ~y, ~ymax, ~x_text, ~y_text, ~label1, ~label2,
  1, 2.5, 13.5, 15, 1.75, 14.25, "Br", "eaking",
  2.5, 4, 12, 13.5, 3.25, 12.75, "Ba", "d"
  ) |>
  mutate(
    y = y + 1,
    ymax = ymax + 1,
    y_text = y_text + 1
  )

# Resize and make images darker -------------------------------------------

imgs <- c(paste0("0", 1:9), 10:12)
walk(imgs, ~{
  image_read(glue("2022/week33-psychometrics/bb{.x}.jpg")) |>
    image_resize("950x1689") |>
    image_modulate(brightness = 40, saturation = 40, hue = 100) |>
    image_write(glue("2022/week33-psychometrics/bb{.x}d.jpg"))
})

# set vector
characters <- unique(df_base$char_name)
img <- c(
  "Walter White" = "bb05",
  "Jesse Pinkman" = "bb03",
  "Mike Ehrmantraut" = "bb11",
  "Saul Goodman" = "bb12",
  "Hank Schrader" = "bb08",
  "Gus Fring" = "bb10",
  "Jane Margolis" = "bb06",
  "Skyler White" = "bb09",
  "Flynn White" = "bb01",
  "Marie Schrader" = "bb07")

# plot --------------------------------------------------------------------

make_plot <- function(char) {

  df_base |>
    filter(char_name == char) |>
    ggplot() +

    # background
    ggpubr::background_image(image_read(glue("2022/week33-psychometrics/{img[char]}d.jpg"))) +

    # element boxes
    geom_rect(aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), fill = green, colour = "white", size = 1) +
    geom_text(aes(x_left_text, y_text, label = left_text), family = ft_text, colour = txt_col, size = 18, hjust = 1) +
    geom_text(aes(x_right_text, y_text, label = right_text), family = ft_text, colour = txt_col, size = 18, hjust = 0) +
    geom_text(aes(x_text, y_text, label = square_text), family = ft_text, colour = txt_col, size = 42, hjust = 0.5) +

    geom_text(aes(x+0.85, y+0.85, label = paste0(avg_rating, "%")), family = ft_text, colour = txt_col, size = 8, hjust = 1) +
    geom_text(aes(x+0.85, y+0.1, label = number_ratings), family = ft_text, colour = txt_col, size = 8, hjust = 1) +
    geom_text(aes(x+0.1, y+0.85, label = rank), family = ft_text, colour = txt_col, size = 8, hjust = 0) +

    # title
    geom_rect(aes(xmin = x, xmax = xmax, ymin = y, ymax = ymax), df_title, fill = green, colour = "white", size = 1) +
    geom_text(aes(x_text, y_text, label = label1), df_title, family = ft_text, colour = txt_col, size = 78, hjust = 0.5) +
    geom_text(aes(xmax+0.1, y_text-0.1, label = label2), df_title, family = ft2, colour = txt_col, size = 54, hjust = 0) +
    annotate("text", 3.25, 12.65, label = "Psychometric Character Profiles", family = ft_text, size = 24, colour = txt_col) +

    geom_text(aes(x+1.36, y+1.36, label = "% votes"), df_title, family = ft_text, colour = txt_col, size = 9, hjust = 1) +
    geom_text(aes(x+1.36, y+0.14, label = "# ratings"), df_title, family = ft_text, colour = txt_col, size = 9, hjust = 1) +
    geom_text(aes(x+0.14, y+1.36, label = "Rank"), df_title, family = ft_text, colour = txt_col, size = 9, hjust = 0) +

    # name
    annotate("text", x = 3.25, y = 11.25, label = char, family = ft2, size = 54, colour = txt_col, fontface = "bold") +

    xlim(-1, 7.2) +
    ylim(0, 16) +
    theme_void()

  ggsave(glue("2022/week33-psychometrics/psychometrics {char}.png"), height = 12, width = 12*950/1689)

}

# make all plots
walk(characters, make_plot)
