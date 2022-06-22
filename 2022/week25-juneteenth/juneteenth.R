# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

log_file <<- "2022/week25-juneteenth/log.txt"

# load data ---------------------------------------------------------------

slave_routes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/slave_routes.csv')

# fonts and palettes ------------------------------------------------------

pal <- spec
bg <- rgb(234, 225, 214, maxColorValue = 255)
txt_col <- "grey20"

ft_text <- "oswald"
ft_title <- ft_text

# wrangle -----------------------------------------------------------------

total_slaves <- scales::comma(sum(slave_routes$n_slaves_arrived, na.rm = TRUE))
a <- 1.4

# titles ------------------------------------------------------------------

text1 <- glue("slaves, on record were transported<br>bewteen 1514 and 1866 but given<br>the incomplete records it is<br><strong>{cg('estimated', lighten(txt_col, 0.5))}</strong> to be closer to 7.6M")
caption <- glue("Graphic: {get_icon('twitter', 20, fill = list(bg = bg, img = txt_col))} @danoehm / Source: WEB DuBois style by Anthony Starks / Code: {get_icon('github', 20, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------
slave_routes |>
  time_log() |>
  group_by(year_arrival) |>
  summarise(
    n = sum(n_slaves_arrived, na.rm = TRUE),
    n_est = n*1.5
  ) |>
  ggplot(aes(year_arrival, n)) +
  geom_area(aes(y = n_est), fill = lighten(txt_col, 0.5)) +
  geom_area(fill = txt_col) +

  annotate("text", x = 1514, y = 70000*a, label = total_slaves, size = 64, family = ft_text,
           fontface = "bold", colour = txt_col, hjust = 0) +
  annotate("richtext", x = 1514, y = 64000*a, label = text1, size = 24, family = ft_text,
           colour = txt_col, hjust = 0, lineheight = 0.35, vjust = 1,
           fill = NA, label.colour = NA) +

  annotate("segment", x = 1554, xend = 1602, y = 60500*a, yend = 60500*a, colour = txt_col, size = 2) +
  annotate("segment", x = 1516, xend = 1568, y = 49600*a, yend = 49600*a, colour = lighten(txt_col, 0.5), size = 2) +

  scale_x_continuous(
    breaks = c(1514, 1866, 2022),
    labels = c(1514, 1866, 2022),
    limits = c(1514, 2022)
  ) +
  ylim(c(0, NA)) +
  labs(caption = caption) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft_text, colour = txt_col),
    plot.background = element_rect(fill = bg),
    plot.margin = margin(t = 80, b = 0, l = 80, r = 80),
    plot.caption = element_markdown(size = 48, hjust = 0.5, margin = margin(b = 20, t = 100)),
    axis.text.x = element_text(size = 80, face = "bold")
  )

ggsave("2022/week25-juneteenth/juneteenth.png", height = 12, width = 14)
