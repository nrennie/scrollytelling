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
    alpha = rep(c(0.1, 0.2), 6)[1:11]
  )


# Define text -------------------------------------------------------------

social <- nrBrand::social_caption(
  twitter = NA,
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)

# Plot --------------------------------------------------------------------

ggplot() +
  # Period data
  geom_rect(
    data = period_data,
    mapping = aes(
      xmin = 0, xmax = 100,
      ymin = Start_Year, ymax = End_Year,
      alpha = alpha
    ),
    fill = text_col
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      x = mean(c(65, 100)),
      y = 0.5 * (Start_Year + End_Year),
      label = Period
    ),
    colour = text_col,
    family = title_font,
    size = 9
  ) +
  geom_text(
    data = period_data,
    mapping = aes(
      x = 101,
      y = Start_Year,
      label = Start_Year
    ),
    colour = text_col,
    family = body_font,
    hjust = 0,
    vjust = 1,
    size = 9
  ) +
  # Marriage data
  geom_segment(
    data = monarch_data,
    mapping = aes(
      x = king_age, xend = consort_age,
      y = year_of_marriage, yend = year_of_marriage
    ),
    colour = alpha(text_col, 0.5),
    linewidth = 0.7
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(x = king_age, y = year_of_marriage),
    colour = highlight_col,
    size = 2,
    pch = 17
  ) +
  geom_point(
    data = monarch_data,
    mapping = aes(x = consort_age, y = year_of_marriage),
    colour = highlight_col2,
    size = 2
  ) +
  # Annotations
  geom_textbox(
    data = data.frame(
      x = 7, y = 1000,
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
    width = unit(0.75, "inch")
  ) +
  annotate(
    "curve", x = 11, y = 1130, 
    xend = 7, yend = 1160,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      x = 53, y = 1600,
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
    "curve", x = 43, y = 1600, 
    xend = 39, yend = 1580,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  geom_textbox(
    data = data.frame(
      x = 50, y = 1420,
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
    "curve", x = 40, y = 1370, 
    xend = 36, yend = 1330,
    arrow = arrow(length = unit(0.2,"cm"), type = "closed"),
    colour = text_col,
    curvature = -0.5
  ) +
  # Styling
  scale_x_continuous(
    limits = c(0, 106),
    breaks = seq(0, 65, 5),
    minor_breaks = NULL
  ) +
  scale_y_reverse(limits = c(2020, 802)) +
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
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = alpha(text_col, 0.2),
      linewidth = 0.4
    )
  )

ggsave("images/main-plot.png", width = 7, height = 9)