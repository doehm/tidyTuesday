# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week10-erasmus/log.txt"

# load data ---------------------------------------------------------------

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv') |>
  clean_names() |>
  time_log() |>
  rename(
    gender = participant_gender,
    sending = sending_country_code,
    receiving = receiving_country_code,
    age = participant_age
    ) |>
  mutate(
    sending = case_when(
      sending == 'EL' ~ 'GR',
      sending == 'UK' ~ 'GB',
      TRUE ~ sending
      ),
    receiving = case_when(
      receiving == 'EL' ~ 'GR',
      receiving == 'UK' ~ 'GB',
      TRUE ~ receiving
    ),
    year = as.numeric(str_extract(academic_year, "[:digit:]+")),
    sending = countrycode(sending, origin="iso2c", destination="iso.name.en"),
    receiving = countrycode(receiving, origin="iso2c", destination="iso.name.en")
    )

# fonts and palettes ------------------------------------------------------

bg <- dark

ft_text <- "incon"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

top_send <- erasmus |>
  time_log() |>
  group_by(sending) |>
  summarise(n_send = sum(participants)) |>
  # slice_max(n_send, n = 10) |>
  pull(sending)

df_send <- erasmus |>
  time_log() |>
  filter(sending %in% top_send) |>
  group_by(sending) |>
  summarise(
    n_send = sum(participants),
    age_send = sum(participants*age)/n_send,
    n_female_send = sum(participants[gender == "Female"])/n_send
    )

df_send_rec <- erasmus |>
  time_log() |>
  filter(receiving %in% top_send) |>
  group_by(receiving) |>
  summarise(
    n_rec = sum(participants),
    age_rec = sum(participants*age)/n_rec,
    n_female_rec = sum(participants[gender == "Female"])/n_rec
  )

df_base <- df_send |>
  time_log() |>
  left_join(df_send_rec, by = c("sending" = "receiving")) |>
  mutate_if(is.numeric, ~replace_na(.x, 0)) |>
  mutate(
    d = (n_rec - n_send)/n_send,
    d_age = age_rec - age_send,
    dir = ifelse(d > 0, "Up", "Down")
    )

# titles ------------------------------------------------------------------

title <- "Student Mobility"
subtitle <- str_rich_wrap(
"On average <strong>Luxemburg, Belgium</strong> and <strong>France</strong> receive more students than they typically send abroad by 10-15%. Proportionally
<strong>Latvia</strong> receives the most female students and <strong>Luxemburg</strong> proportionally the least. <strong>Lithuania, The Netherlands</strong> and <strong>Luxemburg</strong>
typically receive older cohorts than they send whereas <strong>North Macedonia</strong> and <strong>France</strong> typically recieve younger cohorts, however the
difference in the mean is almost negligible.
", 170)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = light))} @danoehm / Source: Data.Europa.eu / Code: {get_icon('github', 10, fill = list(bg = bg, img = light))} doehm/tidytuesday #rstats #tidytuesday")
fill <- "Fill"
colour <- "Proportion\nof Women"

labels <- tribble(
  ~x, ~y, ~label,
  -0.05, 0.25, "Older students\nReceived less",
  0.1, 0.25, "Older students\nReceived more",
  -0.05, -0.25, "Younger students\nReceived less",
  0.1, -0.25, "Younger students\nReceived More"
)

# plot --------------------------------------------------------------------

df_base |>
  filter(
    d_age > -1,
    d_age < 1,
    d > -1000,
    d < 2000
  ) |>
  time_log() |>
  ggplot() +
  geom_text(aes(x, y, label = label), labels, family = ft_text, size = 40, colour = "black", fontface = "bold", lineheight = 0.25) +
  geom_hline(yintercept = 0, colour = "grey80", alpha = 0.5, size = 0.1) +
  geom_vline(xintercept = 0, colour = "grey80", alpha = 0.5, size = 0.1) +
  geom_point(aes(d, d_age, colour = n_female_rec), size = 5) +
  geom_text_repel(aes(d, d_age, label = sending), size = 16, family = ft_text, colour = light, force = 2) +

  # theme and scales and labs
  scale_colour_gradientn(colours = rev(spec)) +
  scale_x_continuous(breaks = seq(-0.1, 0.15, 0.05), labels = paste0(seq(-10, 15, 5), "%")) +
  scale_size(guide = NULL) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = fill,
    colour = colour,
    x = "Net Gain in Number of Students",
    y = "Difference in Mean Age"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = light),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_text, size = 125, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, halign = 0, margin = margin(b = 50)),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.text = element_text(family = ft_text, size = 32),
    axis.title = element_text(family = ft_text, size = 48, margin = margin(t = 15), face = "bold"),
    axis.title.y = element_text(family = ft_text, size = 48, angle = 90, margin = margin(r = 15)),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.35),
    legend.text = element_text(family = ft_text, size = 36),
    legend.position = "right"
  ) +
  ggsave("2022/week10-erasmus/erasmus.png", height = 10, width = 14)
