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
extrafont::loadfonts(quiet = TRUE)

#### data ####
tt <- tidytuesdayR::tt_load(2021, week = 12)
df <- tt$games %>%
  mutate(
    avg_peak_perc = as.numeric(str_replace(avg_peak_perc, "%", "")),
    date = ymd(paste(year, month, "01"))
    )

#### wrangling ####
portal <- df %>%
  filter(gamename %in% c("Portal", "Portal 2")) %>%
  group_by(year, date) %>%
  summarise(
    avg = sum(avg),
    peak = sum(peak)
  ) %>%
  group_by(year) %>%
  summarise(
    avg = mean(avg),
    peak = max(peak)
  ) %>%
  mutate(
    avg_rank = rank(avg),
    peak_rank = rank(peak),
    to_avg_rank = lead(avg_rank),
    to_peak_rank = lead(peak_rank),
    y_dir = ifelse(to_peak_rank > peak_rank, "up", "down"),
    label_avg = paste0(round(avg/1000, 1), "k"),
    label_peak = paste0(round(peak/1000, 1), "k"),
    label_joint = as.character(glue("{label_avg}\n{label_peak}")),
    year = as.character(year)
  ) %>%
  bind_rows(
    tibble(year = "Year", avg_rank = 11.4, peak_rank = 4, label_joint = "Avg\nPeak")
  )

a <- 0.8
b <- 3
c <- 0.25

portal_size <- 1.25

start <- portal %>%
  mutate(
    x = peak_rank + a,
    y = avg_rank
  )

start2 <- portal %>%
  mutate(
    x = peak_rank + b,
    y = avg_rank
  )

# random <- tibble(year = 2012:2021, x = runif(10, 0, 12), y = runif(10, 0, 12))
middle <- portal %>%
  filter(year < 2021) %>%
  group_by(year) %>%
  mutate(
    x = rnorm(n(), (peak_rank + to_peak_rank)/2, 2),
    y = rnorm(n(), (avg_rank + to_avg_rank)/2, 2)
  )

end <- portal %>%
  mutate(
    x = to_peak_rank - a,
    y = to_avg_rank
  )

end2 <- portal %>%
  mutate(
    x = to_peak_rank - b,
    y = to_avg_rank
  )

portal_paths <- bind_rows(
  start, start2, middle, end2, end
  ) %>%
  select(year, x, y) %>%
  arrange(year)

#### portals ####
portals_df_entry <- portal %>%
  mutate(
    x = peak_rank - a,
    y = avg_rank,
    r1 = 0.1,
    r2 = 0.475
  )

portals_df_exit <- portal %>%
  mutate(
    x = peak_rank + a,
    y = avg_rank,
    r1 = 0.1,
    r2 = 0.475
  )


#### colours ####
orange <- "#FC6B06"
blue <- "#01ADEF"
cover <- extract_palette("./2021/week-12/images/cover.jpg")
# show_palette(cover)
portal_fill = cover[4]
grid_lines <- colorRampPalette(c("white", portal_fill))(16)

#### fonts ####
ftbl <- "Segoe UI Black"
ftsb <- "Segoe UI Semibold"


#### legend text ####
legend_text <- str_wrap("
  The combined player counts for Portal and Portal 2 were ranked for each year.
  The x-axis is the rank with respect to the peak number of simultaneous players.
  The y-axis is the rank with respect to the average number of players across months for the year.
  Portals link years in chronological order starting at 2012.
", 65)

#### background ####
bg <- image_read("./2021/week-12/images/bg.jpg")

#### plot ####
g_portal <- portal %>%
  ggplot() +
  geom_ellipse(data = portals_df_entry, aes(x0 = x, y0 = y, a = r1*portal_size, b = r2*portal_size, angle = 0), colour = orange, size = 2.5, fill = portal_fill) +
  geom_bspline(data = portal_paths, mapping = aes(x = x, y = y, group = as.factor(year)), linetype = 2, colour = "grey65", size = 0.5, arrow = arrow(length = unit(0.15, "inches"), type = "closed")) +
  geom_ellipse(data = portals_df_exit, aes(x0 = x, y0 = y, a = r1*portal_size, b = r2*portal_size, angle = 0), colour = blue, size = 2.5, fill = portal_fill) +
  geom_text(data = portal, mapping = aes(x = peak_rank, y = avg_rank+c+0.2, label = year), colour = "white", family = ftbl, size = 7) +
  geom_text(data = portal, mapping = aes(x = peak_rank, y = avg_rank-c+0.2, label = label_joint), colour = "white", family = ftsb, size = 6, lineheight = 0.8) +
  geom_text(mapping = aes(x = 15, y = 1, label = "f0r science...\ny0u MONSTER..."), colour = "grey90", size = 16, family = ftbl, lineheight = 0.8) +
  geom_text(mapping = aes(x = 5.3, y = 11.4, label = legend_text), colour = "grey90", family = ftsb, size = 4.5, lineheight = 0.8, hjust = 0) +
  theme_void() +
  theme(
    axis.title = element_text(colour = "white", family = ftsb, size = 16),
    axis.title.y = element_text(angle = 90, family = ftsb),
    plot.caption = element_text(hjust = 0, family = ftsb, colour = "grey90")
  ) +
  labs(
    x = "Peak (Rank)",
    y = "Average (Rank)",
    caption = "Source: Steam / Wallpaper: mocah.org / Chart: @danoehm"
  ) +
  scale_x_continuous(breaks = 1:10, labels = 10:1) +
  scale_y_continuous(breaks = 1:10, labels = 10:1) +
  coord_cartesian(clip = "off")

g_rect <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 3, ymin = 0, ymax = 11), fill = "black", alpha = 0.4) +
  theme_void()

ht <- 10
wd <- 1920/1200*ht
ggdraw(clip = "off") +
  draw_image(bg) +
  draw_plot(g_rect, x = -0.2, y = -0.2, width = 1.5, height = 1.5) +
  draw_plot(g_portal, x = 0.01, y = 0.01, width = 0.8, height = 1.01) +
  ggsave(glue("./2021/week-12/plots/{format(now(), '%Y-%m-%d %Hh-%Mm-%Ss')} portal.png"), height = ht, width = wd)


#### plot without background ####
#### plot ####
  portal %>%
    ggplot() +
    geom_ellipse(data = portals_df_entry, aes(x0 = x, y0 = y, a = r1*portal_size, b = r2*portal_size, angle = 0), colour = orange, size = 2.5, fill = cover[3]) +
    geom_bspline(data = portal_paths, mapping = aes(x = x, y = y, group = as.factor(year)), linetype = 2, colour = "grey65", size = 0.5, arrow = arrow(length = unit(0.15, "inches"), type = "closed")) +
    geom_ellipse(data = portals_df_exit, aes(x0 = x, y0 = y, a = r1*portal_size, b = r2*portal_size, angle = 0), colour = blue, size = 2.5, fill = cover[3]) +
    geom_text(data = portal, mapping = aes(x = peak_rank, y = avg_rank+c+0.2, label = year), colour = "white", family = ftbl, size = 7) +
    geom_text(data = portal, mapping = aes(x = peak_rank, y = avg_rank-c+0.2, label = label_joint), colour = "white", family = ftsb, size = 6, lineheight = 0.8) +
    # geom_text(mapping = aes(x = 15, y = 1, label = "f0r science...\ny0u MONSTER..."), colour = "grey90", size = 16, family = ftbl, lineheight = 0.8) +
    geom_text(mapping = aes(x = 5.3, y = 11.4, label = legend_text), colour = "grey90", family = ftsb, size = 4.5, lineheight = 0.8, hjust = 0) +
    theme_void() +
    theme(
      axis.title = element_text(colour = "white", family = ftsb, size = 16, margin = margin(t = 20, b = 20, l = 20, r = 20)),
      axis.title.y = element_text(angle = 90, family = ftsb),
      plot.caption = element_text(hjust = 0, family = ftsb, colour = "grey90"),
      axis.ticks = element_line(),
      axis.text = element_text(family = ftsb, colour = "white"),
      plot.background = element_rect(fill = portal_fill),
      # plot.margin = margin(r = 150),
      panel.grid.major = element_line(colour = grid_lines[14])
    ) +
    labs(
      x = "Peak No. of Players (Rank)\nHighest year: Rank 1\nLowest year: Rank 10",
      y = "Average No. of Players (Rank)",
      caption = "Source: Steam / Wallpaper: mocah.org / Chart: @danoehm"
    ) +
    scale_x_continuous(breaks = 1:10, labels = 10:1) +
    scale_y_continuous(breaks = 1:10, labels = 10:1) +
    coord_cartesian(clip = "off") +
    ggsave(glue("./2021/week-12/plots-no-bg/ portal.png"), height = ht, width = wd)


imgs <- list.files("./2021/week-12/for-gif", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(
  image = img_animated,
  path = "./2021/week-12/portal.gif"
  )


