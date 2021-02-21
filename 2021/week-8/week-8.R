
library(tidyverse)
library(janitor)
library(magick)
library(patchwork)
library(extrafont)
library(evoPalette) # devtools::install_github("doehm/evoPalette")
extrafont::loadfonts(device = "win", quiet = TRUE)

#### helpers ####
save_k <- function(loc, filename, format = "png") {
  k <- max(as.numeric(str_extract(list.files(loc), "[:digit:]+")) + 1)
  if(is.infinite(k)) k <- "001"
  glue("{loc}/{filename}-{str_pad(k, 3, 'left', '0')}.{format}")
}

#### data load ####

df <- tidytuesdayR::tt_load(2021, week = 8)
freed_slaves <- df$freed_slaves %>%
  clean_names()

#### fonts ####

ftc <- "Gill Sans Nova Cond"
ft <- "Gill Sans Nova"

update_geom_defaults("text", list( family = ftc, color = black))

#### palette ####

pal <- extract_palette("C:/Users/Dan/Pictures/tidy-tuesday/free-libre-sm.png", n_cols = 4)
show_palette(pal) + ggsave("./2021/week-8/palette.png", height = 4, width = 8)
col1 <- pal[2]
col2 <- pal[3]
ft_col <- pal[4]
bg <- pal[1]

#### label data ####

labels <- freed_slaves %>%
  mutate(
    y = 101.5,
    label = paste0(free, "%")
    )

lines_df <- labels %>%
  mutate(
    y1 = 100,
    yend = ifelse(slave == 0, slave, slave + 3),
    name = NA
    )

title1 <- "PROPORTION  OF  FREEMEN  AND  SLAVES  AMONG  AMERICAN  NEGROES  ."
title2 <- "PROPORTION  DES  NEGRES  LIBRES  ET  DES  ESCLAVES  EN  AMERIQUE  ."
subtitle <- "DONE  BY  ATLANTA  UNIVERSITY  ."

#### plots ####

wd <- 10
freed_slaves %>%
  mutate(free = 100 - slave) %>%
  pivot_longer(cols = -year, names_to = "name", values_to = "y") %>%
  ggplot(aes(x = year, y = y, fill = name)) +
  geom_area() +
  geom_segment(data = lines_df, mapping = aes(x = year, xend = year, y = y1, yend = yend)) +
  annotate("text", x = 1830, y = 55, label = "SLAVES\nESCLAVES", family = ftc, col = bg, fontface = "bold", size = 12, lineheight = 0.8) +
  annotate("text", x = labels$year, y = labels$y, label = labels$year, family = ftc, col = ft_col, fontface = "bold", size = 8, alpha = 0.9) +
  annotate("text", x = 1830, y = 95, label = "FREE  -  LIBRE", family = ftc, col = ft_col, fontface = "bold", size = 10, alpha = 0.9) +
  annotate("text", x = labels$year, y = ifelse(labels$slave == 0, 89, labels$slave) + 1.5, label = labels$label, family = ftc, col = ft_col, fontface = "bold", size = 7, alpha = 0.9) +
  annotate("text", x = 1830, y = 140, label = title1, family = ftc, col = ft_col, fontface = "bold", size = 7, alpha = 0.8) +
  annotate("text", x = 1830, y = 130, label = title2, family = ftc, col = ft_col, fontface = "bold", size = 7, alpha = 0.8) +
  annotate("text", x = 1830, y = 120, label = subtitle, family = ftc, col = ft_col, fontface = "bold", size = 5, alpha = 0.8) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    legend.position = "none"
  ) +
  scale_fill_manual(values = list(slave = col2, free = col1)) +
  ggsave(save_k("./2021/week-8/plots", "slave"), type = "cairo", height = 1.28*wd, width = wd)
