# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(ggview)
library(ggforce)
library(lemon)


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
  mutate(diff = access_non_solid_fuel_urban_pop_pct - access_non_solid_fuel_rural_pop_pct) |>
  group_by(access_non_solid_fuel_rural_pop_pct) |>
  arrange(access_non_solid_fuel_rural_pop_pct) |>
  mutate(rural_n = row_number()) |>
  ungroup() |>
  group_by(access_non_solid_fuel_urban_pop_pct) |>
  arrange(access_non_solid_fuel_urban_pop_pct) |>
  mutate(urban_n = row_number()) |>
  ungroup()

med_data <- plot_data |> 
  summarise(
    med_rural = median(access_non_solid_fuel_rural_pop_pct),
    med_urban = median(access_non_solid_fuel_urban_pop_pct)
  )


# Explainer 1 -------------------------------------------------------------

offset <- 5
ggplot(data = plot_data) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset + rural_n
    ),
    colour = rural_col,
    size = 0.5
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset - urban_n
    ),
    colour = urban_col,
    size = 0.5
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
  scale_y_symmetric() +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 30, 5, 45),
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> ex1
save_ggplot(
  plot = ex1,
  file = file.path("posts", "sustainable-energy", "images", "ex1.png")
)



# Explainer 2 -------------------------------------------------------------

offset <- 5
ggplot(data = plot_data) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset
    ),
    colour = rural_col
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset
    ),
    colour = urban_col
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
  scale_y_continuous(limits = c(-max(c(plot_data$rural_n, plot_data$urban_n)), max(c(plot_data$rural_n, plot_data$urban_n)))) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.margin = margin(5, 30, 5, 45),
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> ex2
save_ggplot(
  plot = ex2,
  file = file.path("posts", "sustainable-energy", "images", "ex2.png")
)



# Explainer 3 -------------------------------------------------------------

offset <- 0.03
ggplot(data = plot_data) +
  # arcs
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_rural_pop_pct / 2,
      y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
      start = -pi / 2, end = pi / 2
    ),
    stat = ggforce::StatArc,
    colour = rural_col
  ) +
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_urban_pop_pct / 2,
      y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
      start = pi / 2, end = 3 * pi / 2
    ),
    stat = ggforce::StatArc,
    colour = urban_col
  ) +
  # points
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset
    ),
    colour = rural_col
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset
    ),
    colour = urban_col
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> ex3
save_ggplot(
  plot = ex3,
  file = file.path("posts", "sustainable-energy", "images", "ex3.png")
)



# Explainer 4 -------------------------------------------------------------

offset <- 0.03
ggplot(data = plot_data) +
  # arcs
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_rural_pop_pct / 2,
      y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
      start = -pi / 2, end = pi / 2
    ),
    stat = ggforce::StatArc,
    colour = alpha(rural_col, 0.4)
  ) +
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_urban_pop_pct / 2,
      y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
      start = pi / 2, end = 3 * pi / 2
    ),
    stat = ggforce::StatArc,
    colour = alpha(urban_col, 0.4)
  ) +
  # points
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset
    ),
    colour = alpha(rural_col, 0.4)
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset
    ),
    colour = alpha(urban_col, 0.4)
  ) +
  # median
  geom_path(
    data = med_data,
    mapping = aes(
      x0 = med_rural / 2,
      y0 = offset, r = med_rural / 2,
      start = -pi / 2, end = pi / 2
    ),
    linewidth = 1.3,
    stat = ggforce::StatArc,
    colour = text_col
  ) +
  geom_point(
    data = med_data,
    mapping = aes(
      x = med_rural,
      y = offset
    ),
    colour = text_col,
    size = 2
  ) +
  geom_path(
    data = med_data,
    mapping = aes(
      x0 = med_urban / 2,
      y0 = -offset, r = med_urban / 2,
      start = pi / 2, end = 3 * pi / 2
    ),
    linewidth = 1.3,
    stat = ggforce::StatArc,
    colour = text_col
  ) +
  geom_point(
    data = med_data,
    mapping = aes(
      x = med_urban,
      y = -offset
    ),
    colour = text_col,
    size = 2
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> ex4
save_ggplot(
  plot = ex4,
  file = file.path("posts", "sustainable-energy", "images", "ex4.png")
)



# All rural ---------------------------------------------------------------

offset <- 0.03
ggplot(data = plot_data) +
  # arcs
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_rural_pop_pct / 2,
      y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
      start = -pi / 2, end = pi / 2
    ),
    stat = ggforce::StatArc,
    colour = alpha(rural_col, 0.4)
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
  # points
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset
    ),
    colour = alpha(rural_col, 0.4)
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset
    ),
    colour = non_col
  ) +
  # median
  geom_path(
    data = med_data,
    mapping = aes(
      x0 = med_rural / 2,
      y0 = offset, r = med_rural / 2,
      start = -pi / 2, end = pi / 2
    ),
    linewidth = 1.3,
    stat = ggforce::StatArc,
    colour = text_col
  ) +
  geom_point(
    data = med_data,
    mapping = aes(
      x = med_rural,
      y = offset
    ),
    colour = text_col,
    size = 2
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> all_rural
save_ggplot(
  plot = all_rural,
  file = file.path("posts", "sustainable-energy", "images", "all_rural.png")
)




# All urban ---------------------------------------------------------------

offset <- 0.03
ggplot(data = plot_data) +
  # arcs
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_urban_pop_pct / 2,
      y0 = -offset, r = access_non_solid_fuel_urban_pop_pct / 2,
      start = pi / 2, end = 3 * pi / 2
    ),
    stat = ggforce::StatArc,
    colour = alpha(urban_col, 0.4)
  ) +
  geom_path(
    mapping = aes(
      x0 = access_non_solid_fuel_rural_pop_pct / 2,
      y0 = offset, r = access_non_solid_fuel_rural_pop_pct / 2,
      start = -pi / 2, end = pi / 2
    ),
    stat = ggforce::StatArc,
    colour = non_col
  ) +
  # points
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_urban_pop_pct,
      y = -offset
    ),
    colour = alpha(urban_col, 0.4)
  ) +
  geom_point(
    mapping = aes(
      x = access_non_solid_fuel_rural_pop_pct,
      y = offset
    ),
    colour = non_col
  ) +
  # median
  geom_path(
    data = med_data,
    mapping = aes(
      x0 = med_urban / 2,
      y0 = -offset, r = med_urban / 2,
      start = pi / 2, end = 3 * pi / 2
    ),
    linewidth = 1.3,
    stat = ggforce::StatArc,
    colour = text_col
  ) +
  geom_point(
    data = med_data,
    mapping = aes(
      x = med_urban,
      y = -offset
    ),
    colour = text_col,
    size = 2
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
  ) +
  canvas(
    width = 7, height = 6,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> all_urban
save_ggplot(
  plot = all_urban,
  file = file.path("posts", "sustainable-energy", "images", "all_urban.png")
)

