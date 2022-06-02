# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week22-polls/log.txt"

# load data ---------------------------------------------------------------

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv("2022/week22-polls/reputation.csv")

# fonts and palettes ------------------------------------------------------

pal <- spec
bg <- "#EBF5FF"
txt_col <- dark

ft_text <- "kan"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

df_base <- reputation |>
  select(-rank) |>
  pivot_wider(names_from = name, values_from = score) |>
  clean_names()

df_facebook <- df_bse |>
  filter(company == "Facebook")

df_twitter <- df_base |>
  filter(company == "Twitter")

df_tiktok <- df_base |>
  filter(company == "TikTok")

# titles ------------------------------------------------------------------

title <- "Ethics and Trust"
subtitle <- "A company that is seen to be ethical is also seen as more trustworthy. Big tech
  <br>is trying to establish ethical practices and many companies score on par with
  <br>other industries. However, <span style='color:#1DA1F2'><strong>Twitter</strong></span>,
  <span style='color:#4267B2'>Facebook</span> and <strong>TikTok</strong> are clearly separate
  <br>from the rest"
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Axios and Harris Poll / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

df_base |>
  mutate(tech = ifelse(industry == "Tech", "Tech", "Other industries")) |>
  ggplot() +
  geom_abline(slope = 1, lty = 3) +
  geom_point(aes(ethics, trust, colour = tech), size = 5, alpha = 0.6) +

  annotate("richtext", x = 65, y = 54, label = "Facebook", family = ft_text, size = 20, colour = "#4267B2", hjust = 0, fontface = "bold", label.colour = NA, fill = bg) +
  annotate("curve", x = 65, y = 55, xend = df_facebook$ethics+0.3, yend = df_facebook$trust+0.3, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = 0.3) +

  annotate("richtext", x = 50, y = 60, label = "Twitter", family = ft_text, size = 20, colour = "#1DA1F2", hjust = 1, fontface = "bold", label.colour = NA, fill = bg) +
  annotate("curve", x = 50, y = 60, xend = df_twitter$ethics-0.3, yend = df_twitter$trust+0.3, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = -0.3) +

  annotate("richtext", x = 70, y = 58, label = "TikTok", family = ft_text, size = 20, hjust = 0, fontface = "bold", label.colour = NA, fill = bg, vjust = 1) +
  annotate("curve", x = 70, y = 58, xend = df_tiktok$ethics+0.3, yend = df_tiktok$trust+0.3, arrow = arrow(length = unit(0.02, "npc"), type = "closed"), curvature = 0.3) +

  # theme and scales and labs
  scale_y_continuous(
    breaks = c(55, 60, 65, 70, 75, 80),
    labels = c("Very poor", "Poor", "Fair", "Good", "Very good", "Excellent"),
    position = "right",
    limits = c(50, max(x$trust))
    ) +
  scale_x_continuous(
    breaks = c(55, 60, 65, 70, 75, 80),
    labels = c("Very poor", "Poor", "Fair", "Good", "Very good", "Excellent")
  ) +
  scale_colour_manual(values = c("grey50", spec[2])) +
  coord_cartesian(clip = "off") +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    colour = "",
    x = "Ethics score",
    y = "Trust\nscore"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col, family = ft_text),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(hjust = 0.5, family = ft_title, size = 128, face = "bold"),
    plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35, margin = margin(b = 20), halign = 0),
    plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 10)),
    plot.margin = margin(t = 50, r = 70, b = 50, l = 100),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.margin = margin(t = 10),
    legend.position = "bottom",
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 36, lineheight = 0.3, margin = margin(l = 10, t = 20)),
  )
ggsave("2022/week22-polls/poll.png", height = 10, width = 10)
