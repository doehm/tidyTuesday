
# week 41 -----------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(showtext)
library(janitor)
library(geofacet)
library(magick)

# data --------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 41)

nurses <- tt$nurses |>
  clean_names()

states <- nurses |>
  filter(year >= 2019) |>
  mutate(
    state_lab = case_when(
      state == "District of Columbia" ~ "D.C.",
      TRUE ~ state
    ),
    salary = annual_salary_median,
    fte = total_employed_rn
    ) |>
  pivot_wider(id_cols = c(state, state_lab), names_from = year, values_from = c(salary, fte)) |>
  mutate(
    change_salary = round((salary_2020 - salary_2019)/1000),
    change_fte = round((fte_2020 - fte_2019)/1000),
    salary_lab = glue("{round(salary_2020/1000)}k ({ifelse(change_salary < 0, change_salary, paste0('+', change_salary))}k)"),
    fte_lab = glue("{round(fte_2020/1000)}k ({ifelse(change_fte < 0, change_fte, paste0('+', change_fte))}k)"),
    scaled_salary_2020 = scale(change_salary/salary_2019)[,1],
    rating = ceiling(5*pnorm(scaled_salary_2020)),
    rating_lab = map_chr(rating, ~paste0(rep("*", .x), collapse = ""))
  ) |>
  left_join(
    nurses |>
      filter(year == 2020) |>
      distinct(state, location_quotient),
    by = "state"
    )

# fonts and palettes ------------------------------------------------------

font_add_google("Inconsolata", "incon")
showtext_auto()

ft <- "incon"

cols <- list(
  pal1 = c("#586ba4", "#eee2df", "#f76c5e"),
  pal2 = c("#240046", "#eee2df", "#ff6d00"),
  pal3 = c("#9d4edd", "#eee2df", "#ff9e00"),
  pal4 = c("#414833", "#fefae0", "#e07a5f"),
  ft = "black"
)

# text --------------------------------------------------------------------

subtitle <- "2020 was quite the year for the healthcare sector. The pandemic dramatically increased the workloads<br>and demands of registered
nurses across the United States. States responded differently to the challenges.<br><br><strong>California, Washington, Wyoming, Nebraska, North Dakota</strong> and <strong>Alska</strong>
all had the largest proportional increase in average annual<br>salaries of registered nurses compared to 2019, whereas <strong>Delaware</strong> and <strong>D.C.</strong> are the only states that saw a drop."

# plot --------------------------------------------------------------------

states |>
  ggplot(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1, fill = location_quotient)) +
  geom_rect() +
  geom_text(aes(x = -0.9, y = 0.9, label = state_lab), family = ft, fontface = "bold", size = 10, lineheight = 0.3, vjust = 1, hjust = 0) +
  geom_text(aes(x = -0.7, y = 0.2, label = salary_lab), family = ft, size = 12, hjust = 0) +
  geom_text(aes(x = -0.7, y = -0.2, label = fte_lab), family = ft, size = 12, hjust = 0) +
  geom_text(aes(x = 0, y = -0.6, label = rating_lab), family = ft, fontface = "bold", size = 12) +
  facet_geo(~state, grid = us_state_grid1) +
  scale_fill_gradientn(colours = cols$pal1) +
  labs(
    title = "Change in Average Annual Salaries of Registered Nurses from 2019 to 2020",
    subtitle = subtitle,
    fill = "Location\nquotient",
    caption = "Note: not all of the changes would be attributable to the pandemic but there was likely some influence\nData: Data.World / Graphic: @danoehm "
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft),
    plot.title = element_text(size = 64, face = "bold"),
    plot.subtitle = element_markdown(size = 32, lineheight = 0.35, margin = margin(b = 20, t= 10)),
    plot.caption = element_text(family = ft, lineheight = 0.25, size = 20),
    plot.margin = margin(50, 50, 20, 50),
    legend.title = element_text(lineheight = 0.25, size = 32, vjust = 1),
    legend.text = element_text(size = 24, vjust = 1, hjust = 1),
    legend.position = "bottom",
    strip.text = element_blank()
  ) +
  ggsave("./2021/week-41/nurses.png", height = 10, width = 14)

# legend ------------------------------------------------------------------

a <- 0.9
ggplot() +
  geom_rect(aes(xmin = -1, xmax = 1, ymin = -1, ymax = 1), fill = NA, colour = "black") +
  annotate("text", x = -0.9, y = 0.8, label = "State", family = ft, fontface = "bold", size = 24, vjust = 1, hjust = 0) +
  annotate("text", x = -0.5, y = 0.3, label = "Median annual\nsalary\nfor 2020", family = ft, size = 14, lineheight = 0.25) +
  annotate("text", x = 0.5, y = 0.3, label = "(change from 2019)", family = ft, size = 14) +
  annotate("text", x = -0.5, y = -0.2, label = "Total employed\nregistered nurses\nfor 2020", family = ft, size = 14, lineheight = 0.25) +
  annotate("text", x = 0.5, y = -0.2, label = "(Change from 2019)", family = ft, size = 14) +
  annotate("text", x = 0, y = -0.7, label = "Rating based on the\nproportional change in\nannual salary from 2019", family = ft, fontface = "bold", size = 14, lineheight = 0.25) +
  theme_void() +
  ggsave("./2021/week-41/legend.png", height = 3.5*a, width = 4.5*a)

# combine plots -----------------------------------------------------------

nurses <- image_read("./2021/week-41/nurses.png")
legend <- image_read("./2021/week-41/legend.png")
final <- image_composite(nurses, image_scale(legend, "700x700"), offset = "+3400-2195")
image_write(final, "./2021/week-41/final.png")
