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

df$student_debt %>%
  select(-loan_debt_pct) %>%
  full_join(df$retirement, by = c("year", "race")) %>%
  full_join(
    df$income_mean %>%
      filter(
        race %in% c("Black Alone", "White Alone", "Hispanic"),
        income_quintile == "Middle",
        dollar_type == "Current Dollars"
        ) %>%
      mutate(race = str_extract(race, "[:alpha:]+"))
      group_by()
  )
