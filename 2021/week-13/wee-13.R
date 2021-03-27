# week 13
library(tidyverse)
library(janitor)
library(glue)
library(snakecase)
library(lubridate)
library(extrafont)
library(cowplot)
library(ggforce)
library(unvotes)
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 13)
tt

#### G20 ####
# leaving the EU out because I couldn't be bothered grouping them - soz
g20 <- c(
  "Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia",
  "Italy", "Japan", "South Korea", "Mexico", "Russia", "Saudi Arabia", "South Africa", "Turkey",
  "United Kingdom"
  )

countries <- g20
df_grid <- expand_grid(country = g20, issue = unique(un_roll_call_issues$issue))
correlations <- vector("numeric", nrow(df_grid))
for(k in 1:nrow(df_grid)){

  text <- glue("{df_grid$country[k]} | {k} / {nrow(df_grid)}")
  cat(text,paste0(rep(" ", 100 - str_length(text)), collapse = ""), "|\r")
  df <- un_votes %>%
    left_join(un_roll_call_issues, by = "rcid") %>%
    filter(
      country %in% c(df_grid$country[k], "United States"),
      issue == df_grid$issue[k],
      vote != "abstain"
      ) %>%
    mutate(vote = as.numeric(vote == "yes")) %>%
    pivot_wider(id_cols = rcid, names_from = country, values_from = vote) %>%
    filter(complete.cases(.))

  correlations[k] <- cor(df[,2], df[,3])

}

df_corr <- df_grid %>%
  as_tibble() %>%
  bind_cols(tibble(corr = round(correlations, 2))) %>%
  mutate(
    label = ifelse(corr > 0, paste0("+", corr), corr),
    country = fct_reorder(str_wrap(country, 10), corr, mean, na.rm = TRUE, .desc = TRUE),
    issue = str_wrap(issue, 10, ),
    issue = fct_reorder(issue, corr, mean, na.rm = TRUE)
    )

#### colours ####
bg <- "#555b6e"
high <- "#89b0ae"
low <- "#e56b6f"
text1 <- "#FFD6BA"
text2 <- "#22223b"
text3 <- "#14213d"
text2 <- "grey20"
text3 <- "grey10"

#### fonts ####
ftbl <- "Segoe UI Black"
ftsb <- "Segoe UI Semibold"
ftl <- "Segoe UI Light"

#### text ####
st1 <- "The G20 is the international forum that brings together the world's major economies. Its members account for more than 80 percent of world GDP, 75 percent of global trade and 60 percent of the population of the planet."
st2 <- "The correlation of voting direction between the United States and other 19 members of
the G20 shows the United States tend to vote similarly the countries on the top of the chart,
such as the UK and Canada, and differently to those on the bottom, such as Russia and China."
st <- str_wrap(paste(st1, st2), 80)

df_corr %>%
  ggplot(aes(fill = corr, ymax = corr, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect() +
  coord_polar(theta = "y", start = -pi, clip = "off") +
  xlim(c(0, 2)) +
  ylim(c(-1,1)) +
  geom_text(aes(x = 0, y = 0, label = label), size = 3.5, family = ftl, colour = text3) +
  facet_grid(country ~ issue) +
  theme_void() +
  scale_fill_gradient2(low = low, high = high) +
  theme(
    plot.background = element_rect(fill = bg),
    strip.background = element_blank(),
    strip.text.x = element_text(family  = ftsb, face = "italic", size = 9, colour = text3),
    strip.text.y = element_text(hjust = 0, vjust = 0.5, family  = ftsb, face = "italic", size = 10, colour = text3, margin = margin(r = 10)),
    strip.placement = "left",
    panel.spacing.x = unit(0.6, "cm"),
    panel.spacing.y = unit(-0.5, "cm"),
    plot.title = element_text(family = ftbl, size = 32, face = "italic", margin = margin(b = 20), colour = text3),
    plot.subtitle = element_text(family = ftl, size = 12, margin = margin(b = 20), colour = text3),
    plot.caption = element_text(family = ftl, face = "italic", hjust = 0.5),
    plot.margin = margin(t = 20, b = 20, l = 20, r = 20),
    legend.position = "none"
    ) +
  labs(
    title = str_wrap("U.S. Voting Patterns with Members of the G20", 25),
    subtitle = st,
    caption = "Data: Rstats {unvotes} / Graphic: @danoehm"
  ) +
  ggsave(glue("./2021/week-13/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} un-votes.png"), height = 13.9, width = 6.9)
  # ggsave(glue("./2021/week-13/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} un-votes.png"), height = 5.75, width = 14)

#### extra ####

df_grid %>%
  as_tibble() %>%
  bind_cols(tibble(corr = df_correlations)) %>%
  ggplot(aes(x = V1, y = V2, fill = corr)) +
  geom_tile(width = 0.8, height = 0.8) +
  scale_fill_gradient2()



df_votes <- un_votes %>%
  filter(country %in% c("United States", "Russia", "China")) %>%
  left_join(df_rcid_year, by = "rcid") %>%
  left_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year, issue) %>%
  summarise(
    yes = sum(vote == "yes"),
    no = sum(vote == "no"),
    abstain = sum(vote == "abstain"),
    p_yes = mean(vote == "yes"),
    p_no = mean(vote == "no"),
    p_abstain = mean(vote == "abstain")
  ) %>%
  filter(!is.na(issue))

df_votes %>%
  ggplot(aes(x = year, y = p_yes, colour = country)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~issue)

tibble(x = letters[1:3], y = c(0, 1, 0.5)) %>%
  ggplot(aes(x, y, fill = x)) +
  geom_bar(stat = "identity") +
  coord_polar("y")



un_votes %>%
  count(country, vote) %>%
  group_by(country) %>%
  mutate(p = n/sum(n)) %>%
  pivot_wider(id_cols = country, names_from = vote, values_from = p) %>%
  ggplot(aes(x = yes, y = no)) +
  geom_point()

df_rcid_year <- un_roll_calls%>%
  mutate(year = year(date)) %>%
  count(rcid, year)

df_votes <- un_votes %>%
  # filter(country %in% c("United States", "Russia", "China")) %>%
  group_by(rcid) %>%
  summarise(
    n_countries = n_distinct(country),
    yes = sum(vote == "yes"),
    no = sum(vote == "no"),
    abstain = sum(vote == "abstain"),
    p_yes = mean(vote == "yes"),
    p_no = mean(vote == "no"),
    p_abstain = mean(vote == "abstain")
  ) %>%
  left_join(df_rcid_year, by = "rcid") %>%
  left_join(un_roll_call_issues, by = "rcid") %>%
  filter(!is.na(issue))

df_votes %>%
  ggplot(aes(x = year, y = p_yes, colour = issue)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ issue)
