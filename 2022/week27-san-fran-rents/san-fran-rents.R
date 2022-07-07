# https://github.com/rfordatascience/tidytuesday

source("scripts/startup.R")

library(sf)
library(scico)
library(ggforce)

log_file <<- "2022/week27-san-fran-rents/log.txt"

# load data ---------------------------------------------------------------

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')
permits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/sf_permits.csv')
new_construction <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/new_construction.csv')

nhoods_shape <- read_sf("2022/week27-san-fran-rents/geo_export_3b286e07-2c69-479a-8452-b9acb0b14cfa.shp")
counties_shape <- read_sf("2022/week27-san-fran-rents/CA_Counties/CA_Counties_TIGER2016.shp")

# fonts and palettes ------------------------------------------------------

bg <- "grey20"
txt_col <- "white"

ft_text <- "karla"
ft_title <- ft_text

palette_style <- "tokyo"

# wrangle -----------------------------------------------------------------

df_base <- rent |>
  filter(
    !is.na(lat),
    !is.na(lon),
    lon <= -121.24,
    lat >= 36.5,
    year > 2010
  ) |>
  mutate(
    lat = round(lat, 2),
    lon = round(lon, 2),
    after_2010 = year > 2010
  )

# rental price
# capping to 10k so the colours scale better
df_price <- df_base |>
  group_by(lat, lon, after_2010) |>
  summarise(price = median(price)) |>
  mutate(price = ifelse(price > 10000, 10000, price))

# Counties in the San Fran area that have rents
counties <- tribble(
  ~county,
  "Sonoma",
  "Napa",
  "Solano",
  "Marin",
  "Contra Costa",
  "San Joacquin",
  "San Francisco",
  "Alameda",
  "San Mateo",
  "Santa Cruz",
  "Santa Clara"
)

# Counties mean and coords for table
df_counties <- rent |>
  filter(
    !is.na(lat),
    !is.na(lon),
    lon <= -121.24,
    lat >= 36.5,
    year > 2010
  ) |>
  group_by(county) |>
  summarise(
    lat = mean(lat),
    lon = mean(lon),
    price = median(price)
    ) |>
  drop_na() |>
  arrange(desc(price)) |>
  mutate(
    label = paste0("$", scales::comma(price)),
    county = str_to_title(county),
    x = -125.2,
    y = seq(39, 38, length = n()),
    rank = paste0(1:n(), "."),
    x_bump = -124.8
    )

# Counties shape file
# Replace San Fran coords with the actual center
df_counties_shape  <- counties_shape |>
  semi_join(counties, by = c("NAME" = "county")) |>
  left_join(df_counties, by = c("NAME" = "county")) |>
  mutate(
    center_lat = as.numeric(str_remove(INTPTLAT, "\\+")),
    center_lon = as.numeric(INTPTLON),
    center_lat = ifelse(NAME == "San Francisco", 37.7575, center_lat),
    center_lon = ifelse(NAME == "San Francisco", -122.4488, center_lon)
  )

# Collapsing the rental prices to form a suitable grid for geom_tile
b <- 0.0025
df_base_sf <- rent |>
  mutate(nhood = str_to_title(nhood)) |>
  semi_join(nhoods_shape, by = c("nhood" = "name")) |>
  filter(
    !is.na(lat),
    !is.na(lon),
    lon <= -122.3,
    lat >= 37.7,
    year > 2010
  ) |>
  mutate(
    lat = round(round(lat, 3)/b)*b,
    lon = round(round(lon, 3)/b)*b
  ) |>
  group_by(lat, lon) |>
  summarise(price = median(price)) |>
  mutate(price = ifelse(price > 10000, 10000, price))

# df for the circle around San Fran
df_circle <- tribble(
  ~y0, ~x0, ~a, ~b,
  37.7575, -122.4488+0.01, 0.095, 0.075
)

# Bump lines
df_bump <- df_counties |>
  select(
    county,
    y_bump = y,
    x_bump
  ) |>
  bind_rows(
    df_counties_shape |>
      as_tibble() |>
      select(
        county = NAME,
        y_bump = center_lat,
        x_bump = center_lon
      )
  ) |>
  left_join(
    df_counties |>
      select(county, price),
    by = "county"
  )

# titles ------------------------------------------------------------------

title <- "Rental Prices in San Francisco"
subtitle <- str_rich_wrap("Rental prices from listing on Craigslist for San Francisco and surrounding areas from 2010", 100)
caption <- glue("Graphic: {get_icon('twitter', 10, fill = list(bg = bg, img = txt_col))} @danoehm / Source: Kate Pennington, data.sfgov.org, Vital Signs / Code: {get_icon('github', 10, fill = list(bg = bg, img = txt_col))} doehm/tidytuesday #rstats #tidytuesday")

# plot --------------------------------------------------------------------

g_cali <- ggplot() +
  geom_sf(data = df_counties_shape, fill = "black", colour = "grey50", size = 0.5) +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, angle = 0), df_circle, fill = "grey30", colour = txt_col, alpha = 0.5) +
  geom_tile(aes(lon, lat, fill = price), df_price, colour = "black") +
  geom_text(aes(x, y, label = county), df_counties, family = ft_text, size = 12, colour = txt_col, hjust = 0) +
  geom_text(aes(x, y-0.05, label = label, colour = price), df_counties |> bind_rows(tibble(price = c(327, 10000))), family = ft_text, size = 12, hjust = 0, fontface = "bold") +
  geom_text(aes(x-0.1, y, label = rank), df_counties, family = ft_text, size = 12, colour = txt_col, hjust = 0) +
  geom_bump(aes(x_bump, y_bump, group = county, colour = price), df_bump |> bind_rows(tibble(price = c(327, 10000))), size = 0.2) +
  geom_point(aes(x_bump, y), df_counties, colour = txt_col, size = 8, pch = "|") +
  geom_point(aes(center_lon, center_lat), df_counties_shape, colour = txt_col, size = 4) +
  geom_text(aes(center_lon, center_lat, label = rank), df_counties_shape, colour = bg, size = 8, family = ft_text) +
  annotate("text", x = -125.2, y = 39.1, label = "Median rental price", colour = txt_col, size = 16, family = ft_text, hjust = 0) +

  # theme and scales and labs
  coord_sf(default_crs = sf::st_crs(4326), clip = "off") +
  scale_fill_scico(
    direction = -1, palette = palette_style, trans = "pseudo_log",
    breaks = c(0, 1000, 5000, 10000),
    labels = c("0", "1k", "5k", "> 10k"),
    ) +
  scale_colour_scico(
    direction = -1, palette = palette_style, trans = "pseudo_log",
    breaks = c(0, 1000, 5000, 10000),
    labels = c("0", "1k", "5k", "> 10k"),
    guide = "none"
  ) +
  xlim(c(-125.6, NA)) +
  labs(
    fill = "Median rental\nprice"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = txt_col),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
    legend.text = element_text(family = ft_text, size = 36, face = "bold"),
    legend.position = "right"
  )


g_sanfran <- ggplot() +
  geom_sf(data = nhoods_shape, fill = "black", colour = "grey50", size = 0.5) +
  geom_ellipse(aes(x0 = x0, y0 = y0, a = a, b = b, angle = 0), df_circle, fill = "grey30", colour = txt_col, alpha = 0.5) +
  geom_tile(aes(lon, lat, fill = price), df_price_sf |> bind_rows(tibble(price = c(327, 10000))), colour = "black") +

  # theme and scales and labs
  coord_sf(default_crs = sf::st_crs(4326), clip = "off") +
  scale_fill_scico(
    direction = -1, palette = palette_style, trans = "pseudo_log",
    breaks = c(0, 1000, 5000, 10000),
    labels = c("0", "1,000", "5,000", "10,000")
    ) +
  theme_void() +
  theme(
    legend.position = "none"
  )

g_cali +
  inset_element(g_sanfran, left = 0, right = 0.5, bottom = 0, top = 0.495) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = theme(
      text = element_text(colour = txt_col),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.title = element_text(hjust = 0.5, family = ft_title, size = 128, face = "bold"),
      plot.subtitle = element_markdown(hjust = 0.5, family = ft_text, size = 48, lineheight = 0.35),
      plot.caption = element_markdown(hjust = 0.5, family = ft_text, size = 36, lineheight = 0.35, margin = margin(t = 20)),
      legend.title = element_text(family = ft_text, size = 36, lineheight = 0.25, face = "bold"),
      legend.text = element_text(family = ft_text, size = 36, face = "bold"),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    )
  )
ggsave("2022/week27-san-fran-rents/sanfran.png", height = 12, width = 16.8)

