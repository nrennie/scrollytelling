# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggforce)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2026-05-26")
energy_cleaned <- tuesdata$energy_cleaned


# Load fonts --------------------------------------------------------------

font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#151C28"
text_col <- "#F2F4F8"
non_col <- "grey20"
rural_col <- "#75DBCD"
urban_col <- "#FBB13C"


# Data wrangling ----------------------------------------------------------

plot_data <- energy_cleaned |>
  select(
    yr, country_name,
    access_non_solid_fuel_rural_pop_pct, access_non_solid_fuel_urban_pop_pct
  ) |>
  drop_na(starts_with("access")) |>
  filter_out(str_detect(country_name, "income|Europe|Asia|Oceania|World|America")) |>
  filter_out(str_detect(country_name, "\\(")) |>
  mutate(
    across(
      starts_with("access"), ~ .x / 100
    )
  ) |>
  mutate(
    tooltip = glue("<b>{country_name}</b><br>Rural: {round(100 * access_non_solid_fuel_rural_pop_pct)}%<br>Urban: {round(100 * access_non_solid_fuel_urban_pop_pct)}%")
  ) |>
  mutate(diff = access_non_solid_fuel_urban_pop_pct - access_non_solid_fuel_rural_pop_pct)


# Plot functions ----------------------------------------------------------

make_chart <- function(category = "Australia", type = c("urban", "rural"), offset = 0.03) {
  g <- ggplot(data = plot_data) +
    # Background arcs
    geom_path(
      mapping = aes(
        x0 = access_non_solid_fuel_rural_pop_pct / 2,
        y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
        start = -pi / 2, end = pi / 2
      ),
      stat = ggforce::StatArc,
      colour = non_col
    ) +
    geom_path(
      mapping = aes(
        x0 = access_non_solid_fuel_urban_pop_pct / 2,
        y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
        start = pi / 2, end = 3 * pi / 2
      ),
      stat = ggforce::StatArc,
      colour = non_col
    ) +
    # Background points
    geom_point(
      mapping = aes(
        x = access_non_solid_fuel_rural_pop_pct,
        y = offset
      ),
      colour = non_col
    ) +
    geom_point(
      mapping = aes(
        x = access_non_solid_fuel_urban_pop_pct,
        y = -offset
      ),
      colour = non_col
    ) +
    # axis
    geom_text(
      data = data.frame(x = seq(0, 1, 0.2), y = 0),
      mapping = aes(
        x = x, y = y,
        label = paste0(100 * x, "%")
      ),
      family = body_font,
      colour = text_col
    ) +
    geom_text(
      data = data.frame(
        x = c(-0.05, -0.05),
        y = c(offset, -offset),
        label = c("Rural", "Urban"),
        colour = c(rural_col, urban_col)
      ),
      mapping = aes(x = x, y = y, label = label, colour = colour),
      family = body_font,
      fontface = "bold",
      hjust = 1,
      size = 4
    ) +
    scale_colour_identity() +
    scale_x_continuous(limits = c(-0.07, 1.01)) +
    coord_fixed(expand = FALSE, clip = "off") +
    theme_void(base_size = 10, base_family = body_font) +
    theme(
      plot.margin = margin(5, 10, 5, 25),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        family = title_font,
        face = "bold",
        size = rel(1.6)
      ),
      plot.subtitle = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 5, t = 5),
        family = body_font
      ),
      plot.caption = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 0, t = 10),
        family = body_font
      ),
      strip.text = element_textbox_simple(
        face = "bold",
        margin = margin(t = 10),
        size = rel(0.9)
      ),
      panel.grid.minor = element_blank()
    )
  if ("rural" %in% type) {
    g <- g +
      # arcs
      geom_path(
        data = filter(plot_data, country_name %in% category),
        mapping = aes(
          x0 = access_non_solid_fuel_rural_pop_pct / 2,
          y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
          start = -pi / 2, end = pi / 2
        ),
        stat = ggforce::StatArc,
        colour = rural_col,
        linewidth = 1.5
      ) +
      geom_point(
        data = filter(plot_data, country_name %in% category),
        mapping = aes(
          x = access_non_solid_fuel_rural_pop_pct,
          y = offset
        ),
        colour = rural_col,
        size = 2
      )
  }
  if ("urban" %in% type) {
    g <- g +
      geom_path(
        data = filter(plot_data, country_name %in% category),
        mapping = aes(
          x0 = access_non_solid_fuel_urban_pop_pct / 2,
          y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
          start = pi / 2, end = 3 * pi / 2
        ),
        stat = ggforce::StatArc,
        colour = urban_col,
        linewidth = 1.5
      ) +
      geom_point(
        data = filter(plot_data, country_name %in% category),
        mapping = aes(
          x = access_non_solid_fuel_urban_pop_pct,
          y = -offset
        ),
        colour = urban_col,
        size = 2
      )
  }


  return(g)
}


# Save charts -------------------------------------------------------------

# Biggest difference
biggest_diff <- plot_data |>
  slice_max(diff, n = 1) |>
  pull(country_name)
make_chart(biggest_diff) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> biggest_diff
save_ggplot(
  plot = biggest_diff,
  file = file.path("posts", "sustainable-energy", "images", "biggest_diff.png")
)

# Smallest diff
max_urban <- plot_data |>
  slice_max(access_non_solid_fuel_urban_pop_pct, n = 1) |>
  pull(country_name)
make_chart(max_urban, type = "urban") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> max_urban
save_ggplot(
  plot = max_urban,
  file = file.path("posts", "sustainable-energy", "images", "max_urban.png")
)

# Minimum urban
min_urban <- plot_data |>
  slice_min(access_non_solid_fuel_urban_pop_pct, n = 1) |>
  pull(country_name)
make_chart(min_urban, type = "urban") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> min_urban
save_ggplot(
  plot = min_urban,
  file = file.path("posts", "sustainable-energy", "images", "min_urban.png")
)

# Maximum rural
max_rural <- plot_data |>
  slice_max(access_non_solid_fuel_rural_pop_pct, n = 1) |>
  pull(country_name)
make_chart(max_rural, type = "rural") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> max_rural
save_ggplot(
  plot = max_rural,
  file = file.path("posts", "sustainable-energy", "images", "max_rural.png")
)

# Minimum rural
min_rural <- plot_data |>
  slice_min(access_non_solid_fuel_rural_pop_pct, n = 1) |>
  pull(country_name)
make_chart(min_rural, type = "rural") +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> min_rural
save_ggplot(
  plot = min_rural,
  file = file.path("posts", "sustainable-energy", "images", "min_rural.png")
)


