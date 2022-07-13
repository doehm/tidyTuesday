
# start up ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(showtext)
library(ggtext)
library(glue)
library(colorspace)
library(snakecase)
library(ggbump)
library(geofacet)
library(crayon)
library(magick)
library(ggrepel)
library(countrycode)
library(ggstream)
library(geomtextpath)
# library(rstanarm)
library(ggdist)
library(ggnewscale)
library(patchwork)
library(ggstream)

# rich wrap ---------------------------------------------------------------

str_rich_wrap <- function(x, n) {
  parts <- str_extract_all(x, "<.*?>")[[1]]
  x1 <- str_replace_all(x, "<.*?>", "XZ")
  x1 <- str_wrap(x1, n)
  for(k in parts) {
    x1 <- str_replace(x1, "XZ", k)
  }
  str_replace_all(x1, "\\n", "<br>")
}

# palettes ----------------------------------------------------------------

honey <- c("#E3D7C1", "#C8B188", "#C4952E", "#BE7C22", "#93500C")
forest <- c("#445544", "#779977", "#99AA77", "#CCCC88", "#BBBB88")
good_pal <- c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")
bad_pal <- rev(c("#ef6351", "#f38375", "#f7a399", "#fbc3bc", "#ffe3e0", "white"))
bright <- c("#540d6e", "#ee4266", "#ffd23f", "#3bceac")
dark <- "#181510"
light <- "#FFF6EC"
lakes <- c("#788FCE", "#e07a5f", "#f2cc8f", "#81b29a", "#f4f1de")
bg_lakes <- "#3d405b"
spec <- c("#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#ae2012")
pror <- c("#7b2cbf", "#ff7900", "#127475", "#a53860")
prorg <- colorRampPalette(pror[1:2])(6)
sunset <- c("#ffcdb2", "#ffb4a2", "#e5989b", "#b5838d", "#6d6875")

# fonts -------------------------------------------------------------------

font_add_google("Abel", "abel")
sysfonts::font_add(
  family = "jose",
  regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Josefin/Static/JosefinSans-Regular.ttf",
  bold = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Josefin/Static/JosefinSans-Bold.ttf",
  italic = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Josefin/Static/JosefinSans-Italic.ttf"
)
sysfonts::font_add(
  family = "jose-thin",
  regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Josefin/Static/JosefinSans-Thin.ttf",
  italic = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Josefin/Static/JosefinSans-ThinItalic.ttf"
)
sysfonts::font_add(
  family = "incon",
  regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Inconsolata/static/Inconsolata/Inconsolata-ExtraLight.ttf",
  bold = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Inconsolata/static/Inconsolata/Inconsolata-ExtraBold.ttf"
)
sysfonts::font_add(
  family = "inconr",
  regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Inconsolata/static/Inconsolata/Inconsolata-Regular.ttf",
  bold = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Inconsolata/static/Inconsolata/Inconsolata-Bold.ttf"
)
sysfonts::font_add(
  family = "nyt",
  regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/Chomsky-font/Chomsky-8MOa2.otf"
)
font_add_google("Barlow Condensed", "barlow")
font_add_google("Oswald", "oswald")
font_add_google("Secular One", "sec")
font_add_google("Patrick Hand", "hand")
font_add_google("Palette Mosaic", "pm")
font_add_google("Crete Round", "crete")
font_add_google("Orelega One", "oreg")
font_add_google("Zen Loop", "zen")
font_add_google("Kanit", "kan")
font_add_google("Lobster", "lobster")
font_add_google("Pacifico", "pacifico")
font_add_google("Indie Flower", "indie")
font_add_google("Karla", "karla")
showtext_auto()

# time log ----------------------------------------------------------------

# an unconventional way to log the time I spend on tidytuesday
time_log <- function(df, ...) {
  write_lines(Sys.time(), file = log_file, append = TRUE)
  df
}

get_time <- function(.duration = 5, ...) {

  stamp <- str_replace(log_file, ".txt", "-posted.txt")
  if(file.exists(stamp)) {
    posted <- read_table(str_replace(log_file, ".txt", "-posted.txt"), col_names = c("date", "time"), col_types = cols())
    posted <- ymd_hms(paste(posted$date, posted$time))

    cat(cyan(glue("\n\nPosted at {posted}\n\n\n")))

  } else {
    posted <- ymd_hms("2999-01-01 00:00:00")
  }

  read_table(log_file, col_names = c("date", "time"), col_types = cols()) |>
    mutate(date_time = ymd_hms(paste(date, time))) |>
    filter(date_time <= posted) |>
    mutate(
      duration = difftime(date_time, lag(date_time), units = "mins"),
      duration = ifelse(is.na(duration), 0, duration),
      duration = pmin(duration, .duration)
    ) |>
    summarise(total = sum(duration)) |>
    mutate(hours = glue("{floor(total/60)}h{round(60*(total/60 - floor(total/60)))}m"))
}

posted <- function(...) {
  stamp <- str_replace(log_file, ".txt", "-posted.txt")
  write_lines(Sys.time(), file = stamp, append = TRUE)
}

# min max -----------------------------------------------------------------

min_max <- function(x, a, b) {
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}


# get icon ----------------------------------------------------------------

get_icon <- function(name, size = 50, fill = NULL) {
  img <- glue('C:/Users/Dan/Downloads/fontawesome/{name}.png')

  if(!is.null(fill)) {

    if(name == "github") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        img = list(col = fill$img, point = "+900+200")
      )
    }

    if(name == "twitter") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        img = list(col = fill$img, point = "+1200+200")
      )
    }

    if(name == "lightning-bolt") {
      pars <- list(
        bg = list(col = fill$bg, point = "+1+1"),
        bg2 = list(col = fill$bg2, point = "+1400+100"),
        img = list(col = fill$img, point = "+700+900")
      )
    }

    x <- image_read(img)
    if(!is.null(fill$bg)) x <- image_fill(x, color = pars$bg$col, point = pars$bg$point, fuzz = 0)
    if(!is.null(fill$bg2)) x <- image_fill(x, color = pars$bg2$col, point = pars$bg2$point, fuzz = 0)
    if(!is.null(fill$img)) x <- image_fill(x, color = pars$img$col, point = pars$img$point, fuzz = 0)
    img <- str_replace(img, ".png", "-fill.png")
    image_write(x, img)
  }
  glue("<img src='{img}' height = {size} width = {size}>")
}



# ggtext colour -----------------------------------------------------------

cg <- function(text, colour = NULL) {
  if(is.null(colour)) {
    colour <- pal[names(pal) == text]
  }
  glue("<span style='color:{colour}'>{text}</span>")
}

b <- function(text) {
  paste0("<strong>", text, "</strong>")
}



# geom_rrect --------------------------------------------------------------

geom_rrect <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRrect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRrect <- ggplot2::ggproto("GeomRrect", ggplot2::Geom,

                              default_aes = ggplot2::aes(
                                colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
                              ),

                              required_aes = c("xmin", "xmax", "ymin", "ymax"),

                              draw_panel = function(self, data, panel_params, coord,
                                                    radius = grid::unit(6, "pt")) {

                                coords <- coord$transform(data, panel_params)

                                lapply(1:length(coords$xmin), function(i) {

                                  grid::roundrectGrob(
                                    coords$xmin[i], coords$ymax[i],
                                    width = (coords$xmax[i] - coords$xmin[i]),
                                    height = (coords$ymax[i] - coords$ymin)[i],
                                    r = radius,
                                    default.units = "native",
                                    just = c("left", "top"),
                                    gp = grid::gpar(
                                      col = coords$colour[i],
                                      fill = alpha(coords$fill[i], coords$alpha[i]),
                                      lwd = coords$size[i] * .pt,
                                      lty = coords$linetype[i],
                                      lineend = "butt"
                                    )
                                  )

                                }) -> gl

                                grobs <- do.call(grid::gList, gl)

                                ggname("geom_rrect", grid::grobTree(children = grobs))

                              },

                              draw_key = ggplot2::draw_key_polygon

)


ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

