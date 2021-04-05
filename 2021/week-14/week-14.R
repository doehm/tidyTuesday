# week 13
library(tidyverse)
library(janitor)
library(glue)
library(extrafont)
library(patchwork)
library(ggtext)
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 14)
tt

a <- 0.9
b <- 0.45
df <- tt$allShades %>%
  filter(hue < 50) %>%
  group_by(brand) %>%
  summarise(
    n = n(),
    hue = sd(hue),
    sat = sd(sat),
    lightness = sd(lightness)
  ) %>%
  filter(n >= 30) %>%
  arrange(desc(hue)) %>%
  ungroup() %>%
  mutate(
    huec = floor(hue/max(hue)*20),
    lightc = floor(lightness/max(lightness)*20),
    huec_end = huec + a,
    lightc_end = lightc + a
  )

n <- df %>%
  count(huec, lightc)

df1 <- df %>%
  distinct(huec, lightc) %>%
  arrange(huec, lightc) %>%
  mutate(id = 1:n())

df <- df %>%
  left_join(df1)

#### colours ####
bg <- "#c38e70"
font <- "grey10"

#### fonts ####
ft <- "Amatic"
ftc <- "Amatic SC"

#### palettes ####
make_palette <- function(.brand, n = 6, a = 0,  hex = FALSE) {

  pal_df <- tt$allShades %>%
    filter(hue < 50) %>%
    filter(brand == .brand)

  fills <- colorRampPalette(sort(pal_df$hex))(n)

  df <- pal_df %>%
    mutate(
      x = (1:n()) + a,
      xend = x+1,
      y = 0,
      yend = 1,
      huec = 10,
      lightc = 10
    ) %>%
    filter(x <= n + a)
    # ggplot() +
    # geom_rect(aes(xmin = x, xmax = xend, ymin = y, ymax = yend), fill = fills) +
    # theme_void()

  list(
    data = df,
    pal = fills
  )

}


#### plot ####
main <- df %>%
  ggplot(aes(x = huec, y = lightc)) +
  geom_hline(yintercept = 13, colour = "grey10", linetype = 2) +
  geom_vline(xintercept = 14, colour = "grey10", linetype = 2) +
  geom_rect(aes(xmin = huec, xmax = huec_end, ymin = lightc, ymax = lightc_end), fill = "white", colour = font) +
  geom_text(data = df, mapping = aes(huec+b, lightc+b, label = id), family = ft, size = 8, colour = font) +
  geom_text(aes(x = 21, y = 21, label = str_wrap("High variation in hue and lightness", 20)), family = ftc, size = 5, lineheight = 0.8) +
  geom_text(aes(x = 21, y = 6, label = str_wrap("High variation in hue", 20)), family = ftc, size = 5, lineheight = 0.8) +
  geom_text(aes(x = 5, y = 21, label = str_wrap("High variation in lightness", 20)), family = ftc, size = 5, lineheight = 0.8) +
  geom_text(aes(x = 5, y = 6, label = str_wrap("Low variation in hue and lightness", 20)), family = ftc, size = 5, lineheight = 0.8) +
  # geom_rect(data = cl$data, mapping = aes(xmin = x, xmax = xend, ymin = 15, ymax = 15.9), fill = cl$pal) +
  # geom_text(data = n, mapping = aes(27.5, 15+b, label = "Clinique"), family = ft, size = 8, colour = font, hjust = 1) +
  # scale_fill_gradientn(colours = pals[[1]]) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = font),
    plot.title = element_text(family = ft, size = 36),
    plot.background = element_rect(fill = bg, colour = NA),
    # axis.text = element_text(size = 16),
    # axis.title = element_text(size = 22),
    plot.margin = margin(t = 30,  b = 30, l = 30, r = 30),
    # axis.title.y = element_text(angle = 90)
  ) +
  labs(
    x = "variation in Hue",
    y = "variation in Lightness"
  ) +
  coord_cartesian(clip = "off")

g_title <- ggplot() +
  geom_text(aes(x = 0, y = 1, label = str_wrap("Anastasia Beverly Hills, Morphe and Maybelline all have high variation in Lightness and hue", 20)), family = ft, size = 13, lineheight = 0.8, hjust = 0) +
  geom_text(aes(x = 0, y = 0, label = str_wrap("The variation in hue and lightenss was calculated for each brand with more than 30 colours. Those in the top right hand corner have high variation in both lightness and hue, therefore covering more of the spectrum", 35)), family = ftc, size = 7, lineheight = 0.8, hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    text = element_text(family = ft, colour = font),
    plot.margin = margin(t = 30,  b = 10, l = 30, r = 10)
  ) +
  xlim(c(0, 1)) +
  coord_cartesian(clip = "off") +
  ylim(c(-0.5, 1.5))

g_legend <- df %>%
  arrange(id) %>%
  mutate(
    y = (1:68 - 1) %% 23 + 1,
    x = cumsum(y == 1)
  ) %>%
  ggplot(aes(x, -y, label = paste(id, "  ", brand))) +
  geom_text(family = ft, size = 7, colour = font, hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    text = element_text(family = ft, colour = font),
    plot.margin = margin(t = 30,  b = 10, l = 30, r = 10)
  ) +
  xlim(c(1, 4))

g_title +
  main +
  g_legend +
  plot_layout(widths = c(1, 2, 3)) +
  ggsave("./2021/week-14/make-up.png", height = 8, width = 21, dpi = 600)

