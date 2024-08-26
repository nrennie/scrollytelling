# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-08-20")
english_monarchs_marriages_df <- tuesdata$english_monarchs_marriages_df


# Load fonts --------------------------------------------------------------

font_add_google("Fraunces")
font_add_google("Ubuntu")
showtext_auto()


# Define colours and fonts-------------------------------------------------

bg_col <- "white"
text_col <- "#2F4F4F"
highlight_col <- "#508080"
highlight_col2 <- "#E30B5C"

body_font <- "Ubuntu"
title_font <- "Fraunces"


# Data wrangling ----------------------------------------------------------

monarch_data <- english_monarchs_marriages_df |>
  filter(
    str_detect(year_of_marriage, c("[–?]"), negate = TRUE),
    str_detect(consort_age, c("[–?]"), negate = TRUE),
    str_detect(king_age, c("[–?]"), negate = TRUE)
  ) |>
  mutate(across(contains("age"), as.numeric))

period_data <- tibble::tribble(
  ~Period, ~Start_Year, ~End_Year,
  "Anglo-Saxon Period", 802, 1066,
  "House of Normandy", 1066, 1154,
  "Angevins", 1154, 1216,
  "Plantagenets", 1216, 1399,
  "House of Lancaster", 1399, 1461,
  "House of York", 1461, 1485,
  "Tudors", 1485, 1603,
  "Stuart Period", 1603, 1714,
  "Hanoverians", 1714, 1901,
  "House of Saxe-Coburg and Gotha", 1901, 1917,
  "House of Windsor", 1917, 2020
) |>
  mutate(
    alpha = rep(c(0.1, 0.2), 6)[1:11],
    y_pos = rep(c(0.1, 0.2), 6)[1:11]
  )


# Plot --------------------------------------------------------------------

ggplot() +
  # Period data
  geom_rect(
    data = period_data,
    mapping = aes(
      ymin = 0, ymax = 100,
      xmin = Start_Year, xmax = End_Year,
      alpha = alpha
    ),
    fill = text_col
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      y = mean(c(65, 100)),
      x = 0.5 * (Start_Year + End_Year),
      label = Period
    ),
    colour = text_col,
    family = title_font,
    lineheight = 0.5,
    size = 9,
    angle = 90
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      y = 101,
      x = Start_Year,
      label = Start_Year
    ),
    colour = text_col,
    family = body_font,
    hjust = 0,
    vjust = 1,
    size = 9,
    angle = 90
  ) +
  # Marriage data
  geom_segment(
    data = monarch_data,
    mapping = aes(
      y = king_age, yend = consort_age,
      x = year_of_marriage, xend = year_of_marriage
    ),
    colour = alpha(text_col, 0.5),
    linewidth = 0.7
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(y = king_age, x = year_of_marriage),
    colour = highlight_col,
    size = 2,
    pch = 17
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(y = consort_age, x = year_of_marriage),
    colour = highlight_col2,
    size = 2
  ) +
  # Annotations
  geom_textbox(
    data = data.frame(
      y = 7, x = 1000,
      label = "Henry the Young King (age 5) marries Margaret of France (age 3) in 1160."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(1.5, "inch")
  ) +
  annotate(
    "curve",
    y = 11, x = 1130,
    yend = 7, xend = 1160,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      y = 52, x = 1650,
      label = "The weddings of Henry the VIII."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(1, "inch")
  ) +
  annotate(
    "curve",
    y = 43, x = 1600,
    yend = 39, xend = 1580,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      y = 50, x = 1420,
      label = "40 year age gap between Edward I (age 60) and Margaret of France (age 20)."
    ),
    mapping = aes(x = x, y = y, label = label),
    colour = text_col,
    family = body_font,
    size = 9,
    hjust = 0.5,
    halign = 0.5,
    lineheight = 0.5,
    fill = alpha(bg_col, 0.3),
    width = unit(1, "inch")
  ) +
  annotate(
    "curve",
    y = 37, x = 1370,
    yend = 33, xend = 1330,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  # Styling
  scale_y_continuous(
    limits = c(0, 106),
    breaks = seq(0, 65, 5),
    minor_breaks = NULL
  ) +
  scale_x_continuous(limits = c(802, 2020)) +
  scale_alpha_identity() +
  coord_cartesian(expand = FALSE) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 26, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.4
    )
  )

ggsave("images/main-plot.png", width = 8, height = 6)


# Period plot function ----------------------------------------------------

period_plot <- function(period) {
  # subset data
  period_subset <- period_data |>
    filter(Period == period)
  monarch_subset <- monarch_data |>
    filter(
      year_of_marriage >= period_subset$Start_Year,
      year_of_marriage <= period_subset$End_Year
    )
  # extra processing for labels
  if (period == "Angevins") {
    monarch_subset <- monarch_subset |>
      mutate(consort_name = if_else(
        consort_age < 5,
        str_replace(str_wrap(consort_name, 5), "\n", "<br>"),
        consort_name
      ))
  }
  if (period == "Tudors") {
    monarch_subset <- monarch_subset |>
      mutate(consort_name = if_else(
        consort_name == "Anne of Cleves",
        "Anne of Cleves / Catherine Howard",
        consort_name
      )) |> 
      filter(consort_name != "Catherine Howard")
  }
  ggplot() +
    # Period data
    geom_rect(
      data = period_subset,
      mapping = aes(
        xmin = 0, xmax = 65,
        ymin = Start_Year, ymax = End_Year
      ),
      fill = text_col,
      alpha = 0.1
    ) +
    # Marriage data
    geom_segment(
      data = monarch_subset,
      mapping = aes(
        x = king_age, xend = consort_age,
        y = year_of_marriage, yend = year_of_marriage
      ),
      colour = alpha(text_col, 0.5),
      linewidth = 0.7
    ) +
    # Monarch
    geom_point(
      data = monarch_subset,
      mapping = aes(x = king_age, y = year_of_marriage),
      colour = highlight_col,
      size = 2,
      pch = 17
    ) +
    geom_textbox(
      data = monarch_subset,
      mapping = aes(
        x = king_age, y = year_of_marriage, label = king_name,
        hjust = dplyr::case_when(
          consort_age < king_age ~ 0,
          TRUE ~ 1
        ),
        halign = dplyr::case_when(
          consort_age < king_age ~ 0,
          TRUE ~ 1
        )
      ),
      family = body_font,
      size = 9,
      colour = text_col,
      lineheight = 0.5,
      fill = "transparent",
      box.colour = "transparent"
    ) +
    # Consort
    geom_point(
      data = monarch_subset,
      mapping = aes(x = consort_age, y = year_of_marriage),
      colour = highlight_col2,
      size = 2
    ) +
    geom_textbox(
      data = monarch_subset,
      mapping = aes(
        x = consort_age, y = year_of_marriage, label = consort_name,
        hjust = dplyr::case_when(
          consort_age < king_age ~ 1,
          TRUE ~ 0
        ),
        halign = dplyr::case_when(
          consort_age < king_age ~ 1,
          TRUE ~ 0
        )
      ),
      family = body_font,
      size = 9,
      lineheight = 0.5,
      colour = text_col,
      fill = "transparent",
      box.colour = "transparent"
    ) +
    # Styling
    scale_x_continuous(
      limits = c(0, 65),
      breaks = seq(0, 65, 5),
      minor_breaks = NULL
    ) +
    scale_y_reverse(limits = c(period_subset$End_Year, period_subset$Start_Year)) +
    scale_alpha_identity() +
    coord_cartesian(expand = FALSE, clip = "off") +
    labs(
      x = "Age at marriage",
      y = NULL,
      title = period
    ) +
    theme_minimal(base_size = 26, base_family = body_font) +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_line(
        colour = alpha(text_col, 0.2),
        linewidth = 0.4
      ),
      plot.title = element_textbox_simple(
        colour = text_col,
        hjust = 0,
        halign = 0,
        margin = margin(b = 10, t = 10),
        lineheight = 0.5,
        family = title_font,
        size = rel(2),
        face = "bold"
      )
    )
}

period_plot("Anglo-Saxon Period")
ggsave("images/anglo-saxons.png", width = 8, height = 6)

period_plot("House of Normandy")
ggsave("images/normandy.png", width = 8, height = 6)

period_plot("Angevins")
ggsave("images/angevins.png", width = 8, height = 6)

period_plot("Plantagenets") # fix overlap
ggsave("images/plantagenets.png", width = 8, height = 6)

period_plot("House of Lancaster")
ggsave("images/lancaster.png", width = 8, height = 6)

period_plot("House of York")
ggsave("images/york.png", width = 8, height = 6)

period_plot("Tudors")
ggsave("images/tudors.png", width = 8, height = 6)

period_plot("Stuart Period") # fix overlap
ggsave("images/stuart.png", width = 8, height = 6)

period_plot("Hanoverians")
ggsave("images/hanoverians.png", width = 8, height = 6)

period_plot("House of Saxe-Coburg and Gotha")
ggsave("images/saxe-coburg.png", width = 8, height = 6)

period_plot("House of Windsor")
ggsave("images/windsor.png", width = 8, height = 6)
