# week 3 ------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(showtext)
library(ggtext)
library(glue)
library(rstanarm)
library(colorspace)
library(snakecase)

# load data ---------------------------------------------------------------

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

# fonts and palettes ------------------------------------------------------

pal <- c("#987654","#7D5D3D","#634626","#4A2F11","#321A00","#1A0100")
bg <- lighten(pal[1], 0.85)
ft_col <- pal[5]

font_add_google("Karla", "karla")
font_add_google("Lobster", "lobster")
showtext_auto()

ft_text <- "lobster"
ft_text2 <- "karla"

# wrangle -----------------------------------------------------------------

df_choc <- chocolate |>
  mutate(char = str_split(most_memorable_characteristics, ",")) |>
  select(-most_memorable_characteristics) |>
  unnest(char) |>
  mutate(
    char = str_trim(char),
    char = fct_lump(char, 20),
    one = 1
    ) |>
  distinct()

mean_cocoa <- df_choc |>
  mutate(char = to_title_case(as.character(char))) |>
  mutate(cocoa_percent = as.numeric(str_extract(cocoa_percent, "[:digit:]+"))/10) |>
  group_by(char) |>
  summarise(mean_cocoa = mean(cocoa_percent))

df_choc <- df_choc |>
  pivot_wider(names_from = char, values_from = one) |>
  select(rating, Other:`dried fruit`) |>
  mutate_all(~replace_na(.x, 0))

# model -------------------------------------------------------------------

mod <- stan_glm(rating ~ ., data = df_choc)

df_coef <- mod$stan_summary |>
  as_tibble() |>
  clean_names() |>
  bind_cols(tibble(var = row.names(mod$stan_summary)))

# more wrangle ------------------------------------------------------------

df <- df_coef |>
  filter(var %in% colnames(df_choc)) |>
  mutate(
    var = to_title_case(var),
    var = fct_reorder(var, mean),
    y = as.numeric(var),
    char = as.character(var)
    ) |>
  left_join(mean_cocoa, by = "char")

df_labs <- tribble(
  ~x, ~y, ~lab,
  -1, 20, "Mouth\nwateringly\ndelicious",
  -1, 12, "Very\ndelicious",
  -1, 6, "Quite\ndelicious"
)

# titles ------------------------------------------------------------------

title <- "The Best Chocolate Is..."
subtitle <- str_rich_wrap("<strong>Rich and creamy with delicate cocoa flavours, fruit-driven undertones and warmth from a hint of spice</strong><br> Chocolate is rated on a scale of 1-5 and described with the most memorable characteristics. They can be described with multiple characteristics. The average chocolate rating is 3.3. The characteristics below are associated with either higher or lower ratings on average. Colour corresponds to the cocoa % of the chocolate. The perfect chocolate is <strong><span style='color:#4A2F11'>rich and creamy and 70% cocoa</span></strong>", 100)
caption <- "Graphic: @danoehm / Source: Flavors of Cacao by way of Georgios and Kelsey / #rstats #tidytuesday week 3"
colour <- "Mean Cocoa %"

# breaks ------------------------------------------------------------------

breaks <- round(seq(-0.3, 0.25, 0.1), 2)
breaks_lab <- ifelse(breaks <= 0, breaks, paste0("+", breaks))

# plot --------------------------------------------------------------------

df |>
  ggplot() +
  annotate("segment", x = 0, xend = 0, y = 1, yend = 20, lty = 3) +
  geom_segment(aes(x = x2_5_percent, xend = x97_5_percent, y = y, yend = y, colour = mean_cocoa), size = 1.5) +
  geom_segment(aes(x = x10_percent, xend = x90_percent, y = y, yend = y, colour = mean_cocoa), size = 3) +
  geom_point(aes(x50_percent, y), size = 12, colour = "black") +
  geom_point(aes(x50_percent, y, colour = mean_cocoa), size = 11) +
  geom_text(aes(x50_percent - 0.2, y = y, label = var), family = ft_text2, size = 20) +
  geom_text(aes(x+0.2, y = y-1, label = lab), df_labs, family = ft_text, size = 48, lineheight = 0.3, vjust = 1, colour = ft_col) +
  annotate("text", x = 0, y = 22, label = "Mean rating: 3.3", family = ft_text2, size = 24, fontface = "bold") +

  # theme and scales and labs
  scale_x_continuous(breaks = breaks, labels = breaks_lab, limits = c(-1, 0.35)) +
  coord_cartesian(clip = "off") +
  scale_colour_gradientn(colours = pal) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    colour = colour
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    plot.title = element_text(hjust = 0.5, family = ft_text, size = 250, colour = ft_col),
    plot.subtitle = element_markdown(hjust = 0.5, halign = 0, family = ft_text2, size = 64, lineheight = 0.35, margin = ggplot2::margin(b = -10, t = 10), colour = ft_col),
    plot.caption = element_text(hjust = 0.5, family = ft_text2, size = 48, lineheight = 0.35, colour = ft_col, margin = ggplot2::margin(t = 20)),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 20, l = 30),
    legend.title = element_text(family = ft_text2, size = 36, lineheight = 0.25, face = "bold", colour = ft_col, vjust = 0.85),
    legend.text = element_text(family = ft_text2, size = 36, colour = ft_col),
    legend.margin = ggplot2::margin(t = 15),
    legend.position = "bottom",
    axis.text.x = element_text(family = ft_text2, size = 64)
    ) +
  ggsave("2022/week03-chocolate/chocolate.png", height = 16, width = 16)

