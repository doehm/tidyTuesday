  # https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(janitor)
library(ggtext)
library(showtext)

source("scripts/startup.R")

# load data ---------------------------------------------------------------

chips <- read_csv("2022/week34-chips/chip.csv") |>
  clean_names()

# fonts and palettes ------------------------------------------------------

bright <- c("#540d6e", "#ee4266", "#ffd23f", "#3bceac")
bg <- "white"
txt_col <- "black"
ft_text <- "incon"

# wrangle -----------------------------------------------------------------

df_base <- chips |>
  filter(
    type == "GPU",
    vendor %in% c("AMD", "NVIDIA")
    ) |>
  mutate(
    transistors_million = log(transistors_million),
    FP16 = log(fp16_gflops),
    FP32 = log(fp32_gflops),
    FP64 = log(fp64_gflops),
    vendor = tolower(vendor)
  ) |>
  pivot_longer(c(FP16, FP32, FP64), names_to = "n_gflops", values_to = "gflops")

# titles ------------------------------------------------------------------

#  caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: CHIP dataset / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

df_base |>
  ggplot(aes(transistors_million, gflops, size = die_size_mm_2, colour = freq_m_hz)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~n_gflops, nrow = 3, strip.position = "right") +
  scale_colour_gradientn(colours = pal) +
  scale_y_continuous(breaks = seq(4, 12, 2), labels = c(50, 400, "3k", "22k", "163k")) +
  scale_x_continuous(breaks = seq(6, 12, 2), labels = c(400, "3k", "22k", "163k"), limits = c(5, 11)) +
  labs(
    x = "Transistors (Million)",
    y = "GFLOPS",
    size = "Die Size mm2",
    colour = "Freq Mhz",
    title = "GPU Performance",
    subtitle = "GPU performance improvement is a joint effect of smaller\n
    transistors, larger die size, and higher frequency",
    caption = "@danoehm / Source: CHIP dataset / Code: doehm/tidytuesday #rstats #tidytuesday"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = ft_text, size = 36, colour = "black"),
    strip.background = element_rect(fill = "grey20"),
    strip.text = element_text(colour = "white", angle = 270, margin = margin(2, 2, 2, 2), size = 36, face = "bold"),
    legend.position = "right",
    plot.margin = margin(t = 40, l = 40, r = 40, b = 5),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(size = 64, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 36, lineheight = 0.2, hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_markdown(size = 24, hjust = 0.5, margin = margin(t = 30))
  )

ggsave("2022/week34-chips/chips.png", height = 8, width = 8)
