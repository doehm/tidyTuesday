library(tidyverse)
library(janitor)
library(evoPalette) # devtools::install_github("doehm/evoPalette")
library(survivoR)
extrafont::loadfonts(quiet = TRUE)

#### helpers ####
save_k <- function(loc, filename, format = "png") {
  k <- max(as.numeric(str_extract(list.files(loc), "[:digit:]+")) + 1)
  if(is.infinite(k)) k <- "001"
  glue("{loc}/{filename}-{str_pad(k, 3, 'left', '0')}.{format}")
}

min_max <- function(x){
  (x - min(x))/(max(x) - min(x))
}

#### data ####
df <- tidytuesdayR::tt_load(2021, week = 9)

earn <- df$earn %>%
  mutate(
    median_weekly_earn = min_max(median_weekly_earn) + 0.1,
    n_persons = min_max(n_persons) + 0.1
  )

#### fonts ####
# ftc <- "Gill Sans Nova Cond"
# ft <- "Gill Sans Nova"
ftc <- "Verdana Pro Cond Black"
ft <- "Verdana Pro Cond Light"


#### palette ####
season <- 10
sp <- season_palettes$palette[[season]]
col1 <- "#ccd5ae" # neutral colour  faedcd ccd5ae
col2 <- sp[1]
bg <- colorRampPalette(c("black", sp[2]))(8)

#### titles ####
wd <- 80
title <- str_wrap("Median Weekly Earnings", wd/3)
subtitle0 <- str_wrap(
  '"Asian women and men earned more than their White, Black, and Hispanic counterparts in 2019. Among women,
  Whites ($840) earned 82 percent as much as Asians ($1,025); Blacks ($704) earned 69 percent; and Hispanics
  ($642) earned 63 percent. Among men, these earnings differences were even larger: White men ($1,036) earned
  78 percent as much as Asian men ($1,336); Black men ($769) earned 58 percent as much; and Hispanic men ($747)
  earned 56 percent" - US Bureau of Labor Statistics',
  wd)
subtitle <- paste(subtitle0)


#### plot ####
earn %>%
  filter(
    age %in% c("16 years and over", "25 years and over", "55 years and over"),
    !str_detect(sex, "Both"),
    !str_detect(race, "All"),
    year %% 2 == 0
  ) %>%
  group_by(age, year, race, sex) %>%
  summarise(
    n_persons = mean(n_persons),
    median_weekly_earn = mean(median_weekly_earn)
  ) %>%
  ungroup() %>%
  mutate(
    race = str_wrap(race, 10),
    year = as.numeric(year)
    ) %>%
  bind_rows(expand_grid(sex = letters[1:2], race = unique(.$race), year = seq(2010, 2020, 2))) %>%
  ggplot() +
  geom_bar(mapping = aes(x = sex, y = median_weekly_earn, fill = age), stat = "identity") +
  facet_grid(year ~ race) +
  scale_x_discrete(labels = c("Men" = "Men", "Women" = "Women", "a" = " ", "b" = " ")) +
  labs(
    title = title,
    subtitle = subtitle0,
    caption = "Source: US Bureau of Labor Statistics / Graphic: @danoehm",
    fill = "Age Group"
  ) +
  theme_void() +
  coord_polar("y", clip = "off") +
  scale_fill_survivor(season) +
  theme(
    text = element_text(colour = col2, family = ft),
    strip.text = element_text(family = ft, size = 10),
    plot.background = element_rect(fill = bg[1], colour = NA),
    plot.margin = margin(t = 20, b = 50, l = 20, r = 20),
    plot.title = element_text(family = ftc, face = "bold", hjust = 0.5, margin = margin(b = 20), size = 36),
    plot.subtitle = element_text(family = ft, hjust = 0, margin = margin(b = 20), size = 16),
    plot.caption = element_text(margin = margin(t = 20), size = 12),
    legend.position = "bottom",
    legend.margin = margin(t = 20),
    legend.text = element_text(size = 10),
    axis.text.y = element_text(family = ft, hjust = 1)
  ) +
  ggsave(glue("./2021/week-9/plots/earn-{format(now(), '%Y%m%d-%H%M%S')}.png"), type = "cairo", height = 20, width = 8.72)


