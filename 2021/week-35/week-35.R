library(tidyverse)
library(janitor)
library(glue)
library(extrafont)
library(patchwork)
library(ggtext)
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 35)
tt

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')
