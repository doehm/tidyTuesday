library(tidyverse)
library(tidytuesdayR)
library(extrafont)
library(janitor)
library(glue)
library(cowplot)
library(ggforce)
library(purrr)
library(png)

df <- tt_load(2021, week = 7)

#### helpers ####
save_k <- function(loc, filename, format = "png") {
  k <- max(as.numeric(str_extract(list.files(loc), "[:digit:]+")) + 1)
  if(is.infinite(k)) k <- "001"
  glue("{loc}/{filename}-{str_pad(k, 3, 'left', '0')}.{format}")
}

#### fonts ####
# ft <- "Verdana Pro Light"
# ftb <- "Verdana Pro Black"
ft <- "Sarabun ExtraLight"
ftb <- "Sarabun ExtraBold"

#### palette ####
col1 <- "#ccd5ae" # neutral colour  faedcd ccd5ae
col2 <- "white"
bg <- "black"

#### titles ####
wd <- 70
title <- "Income Inequality"
subtitle0 <- str_wrap(
  "The income gap between White and Black American middle income earners between
  1967 and 2019",
  wd)
subtitle1 <- str_wrap(
  '"Income is money coming into a family, while wealth is a family’s assets—things like savings,
  real estate, businesses—minus debt. Both are important sides of families’ financial security,
  but wealth cushions families against emergencies and gives them the means to move up the economic
  ladder. Also, wealth disparities are much greater than income disparities: three times as much by
  one measure."',
  wd)
subtitle2 <- str_wrap(
  '"Income inequality can worsen wealth inequality because the income people have available to save and
  invest matters. Focusing on private income, such as earnings and dividends, plus cash government
  benefits, we see that the income of families near the top increased roughly 90 percent from 1963 to
  2016, while the income of families at the bottom increased less than 10 percent."',
  wd)
subtitle <- paste(subtitle0, "\n\n", subtitle1)

#### plot ####
ann_df <- tibble(
  x = 2022,
  y = c(72471, 45356),
  label = c("White - $72k", "Black - $45k"),
  race = c("Black Alone", "White Alone"),
  difference = 1
)

main <- df$income_mean %>%
  filter(
    dollar_type == "Current Dollars",
    income_quintile == "Middle",
    race %in% c("White Alone", "Black Alone")
    ) %>%
  mutate(race = factor(race, levels = c("White Alone", "Black Alone"))) %>%
  arrange(year, race) %>%
  select(year, race, income_dollars) %>%
  # pivot_wider(names_from = race, values_from = income_dollars) %>%
  # clean_names() %>%
  # mutate(d = 100*round(white_alone/black_alone, 2)) %>%
  group_by(race) %>%
  mutate(difference = ifelse(race == "Black Alone", 1, income_dollars/max(income_dollars))) %>%
  ggplot(aes(x = year, y = income_dollars, fill = race, alpha = difference, colour = NULL)) +
  geom_bar(stat = "identity", position = "identity") +
  geom_text(data = ann_df, mapping = aes(x = x, y = y, label = label), colour = col1, family = ft, size = 5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg),
    plot.margin = margin(l = 80, t = 40, r = 40),
    legend.position = "none",
    plot.caption = element_text(family = ft, colour = col1, margin = margin(b = 20), size = 12)
  ) +
  labs(
    caption = "Source: Urban Institute and US Census / Graphic: @danoehm"
  ) +
  coord_cartesian(clip = "off") +
  # coord_polar("y", clip = "off") +
  scale_fill_manual(values = c(col1, "black"))

text <- ggplot() +
  geom_text(aes(x = 0.1, y = 0.57), label = title, family = ftb, colour = col1, size = 20, hjust = 0) +
  geom_text(aes(x = 0.1, y = 0.35), label = subtitle, family = ft, colour = col1, size = 5, hjust = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg)
  ) +
  coord_cartesian(clip = "off", xlim = c(0, 1), ylim = c(0, 1))

ggdraw() +
  draw_plot(main) +
  draw_plot(text, 0.05, 0.35, 0.3, 0.95) +
  ggsave(save_k("2021/week-7/plots", "income"), height = 10, width = 22)

