# week 13
library(tidyverse)
library(janitor)
library(glue)
library(evoPalette) # devtools::install_github("doehm/evoPalette")
library(snakecase)
library(lubridate)
library(extrafont)
library(cowplot)
library(magick)
library(ggforce)
library(unvotes)
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 13)
tt

un_votes
un_roll_calls
un_roll_call_issues

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

#### G20 ####
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
    country = fct_reorder(str_wrap(country, 10), corr, mean, na.rm = TRUE)
    # issue = str_wrap(issue, 15)
    )

#### colours ####
bg <- "#555b6e"
high <- "#89b0ae"
low <- "#e56b6f"

#### fonts ####
ftbl <- "Segoe UI Black"
ftsb <- "Segoe UI Semibold"
ftl <- "Segoe UI Light"

#### text ####
st1 <- "The G20 is the international forum that brings together the world's major economies. Its members account for more than 80 percent of world GDP, 75 percent of global trade and 60 percent of the population of the planet.
The forum has met every year since 1999 and since 2008 has included a yearly Leaders' Summit, with the participation of the respective Heads of State and Government."
st <- paste(st1)
subtitle <- str_wrap(st, 120)

df_corr %>%
  ggplot(aes(fill = corr, ymax = corr, ymin = 0, xmax = 2, xmin = 1)) +
  # geom_rect(aes(ymax=1, ymin=-1, xmax=2, xmin=1), fill = NA, colour = "grey50") + #"#ece8bd"
  geom_rect() +
  # coord_polar(theta = "y",start = -pi/2) +
  coord_polar(theta = "y", start = -pi) +
  xlim(c(0, 2)) +
  ylim(c(-1,1)) +
  geom_text(aes(x = 0, y = 0, label = label), size=3) +
  # geom_text(aes(x=1, y=2, label=title), size=4.2) +
  facet_grid(issue ~ country) +
  theme_void() +
  # scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  # scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  scale_fill_gradient2(low = low, high = high) +
  theme(
    plot.background = element_rect(fill = bg),
    strip.background = element_blank(),
    strip.text.x = element_text(family  = ftl, face = "bold"),
    strip.text.y = element_text(hjust = -10, vjust = 0, family  = ftl, face = "bold"),
    panel.spacing.y = unit(0.8, "cm"),
    plot.title = element_text(family = ftbl, size = 20),
    plot.subtitle = element_text(family = ftl, size = 12)
    ) +
  labs(
    title = "U.S. Voting Patterns with Members of the G20",
    subtitle = st
  ) +
  guides(fill=FALSE) +
  guides(colour=FALSE) +
  ggsave(glue("./2021/week-13/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} un-votes.png"), height = 10, width = 16)






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



df <- data.frame(matrix(nrow=5, ncol = 2))

names(df) <- c("variable", "percentage")
df$variable <- c("Carbohydrates", "Warming", "NGTnotPresent", "DrainNotPresent", "DrEaMing")
df$percentage <- c(0.67,0.33,0.86,0.78,0.58)

df <- df %>% mutate(group=ifelse(percentage <0.6, "red",
                                 ifelse(percentage>=0.6 & percentage<0.8, "orange","green")),
                    label=paste0(percentage*100, "%"),
                    title=dplyr::recode(variable, `Carbohydrates`="Preoperative\ncarbohydrate loading",
                                        `Warming`="Intraoperative\nwarming",
                                        `NGTnotPresent`="Patients without a\nnasogastric tube\non arrival in recovery",
                                        `DrainNotPresent`="Patients without an\nabdominal drain\non arrival in recovery",
                                        `DrEaMing`="Patients DrEaMing on\npostoperative day 1")) %>%
  mutate(percentage = 2*percentage - 1)
ggplot(df, aes(fill = group, ymax = percentage, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax=1, ymin=-1, xmax=2, xmin=1), fill = NA, colour = "grey50") + #"#ece8bd"
  geom_rect() +
  coord_polar(theta = "y",start=-pi/2) +
  xlim(c(0, 2)) + ylim(c(-1,3)) +
  geom_text(aes(x = 0, y = 0, label = label, colour=group), size=6.5) +
  geom_text(aes(x=1, y=2, label=title), size=4.2) +
  facet_wrap(~title, ncol = 5) +
  theme_void() +
  scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  scale_colour_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  guides(fill=FALSE) +
  guides(colour=FALSE)
