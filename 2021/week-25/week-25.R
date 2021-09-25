# week 25

library(tidyverse)
library(janitor)
library(glue)
library(extrafont)
library(ggtext)
library(forcats)
library(ggfx)
extrafont::loadfonts(quiet = TRUE)


# data load ---------------------------------------------------------------

data <- tidytuesdayR::tt_load(2021, week = 25)

df <- data$tweets

summary(df)

df |>
  count(url)
