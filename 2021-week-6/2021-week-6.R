library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(janitor)
library(glue)
library(cowplot)
library(ggforce)

df <- tt_load(2021, week = 6)

df$hs_students %>%
  clean_names() %>% View()

df$hbcu_all %>%
  clean_names() %>%
  select(year, males, females) %>%
  pivot_longer(-year, names_to = "gender", values_to = "enrolled") %>%
  group_by(year) %>%
  mutate(p = enrolled/sum(enrolled)) %>%
  ggplot(aes(x = year, y = p, colour = gender, fill = gender)) +
  geom_line()
  # facet_wrap(~gender) +
  # coord_polar("x")

# facet year
# square - number of enrolled
# random squares for proportion of females
