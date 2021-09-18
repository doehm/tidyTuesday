# week 13
library(tidyverse)
library(janitor)
library(glue)
library(extrafont)
library(patchwork)
library(ggtext)
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 15)
tt

tt$brazil_loss %>%
  pivot_longer(cols = c(-entity, -code, -year), names_to = "type", values_to = "loss") %>%
  mutate(type1 = fct_lump(type, 4, w = loss)) %>%
  group_by(year, type1) %>%
  summarise(loss = sum(loss)) %>%
  mutate(
    c_loss = cumsum(loss),
    max_c_loss = max(c_loss)
    ) %>%
  ggplot(aes(x = year, y = loss, fill = type1)) +
  # geom_bar(stat = "identity")
  # geom_bump() +
  as_reference(
    geom_text(aes(y = max_c_loss, label = year), hjust = 0.5, size = 12),
    id = "text"
  ) +
  with_blend(
    geom_bar(stat = "identity"),
    bg_layer = "text",
    blend_type = "xor"
  )

tt$forest %>%
  group_by(year) %>%
  summarise(total = sum(net_forest_conversion))

