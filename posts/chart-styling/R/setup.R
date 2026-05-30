library(tidyverse)
library(ggtext)
library(ggnewscale)
library(gghighlight)


# Colours -----------------------------------------------------------------

okabeito_colors_list <- c(
  orange = "#E69F00",
  `light blue` = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  red = "#D55E00",
  purple = "#CC79A7",
  grey = "#999999",
  black = "#000000",
  `sky blue` = "#56B4E9",
  `bluish green` = "#009E73",
  vermillion = "#D55E00",
  `reddish purple` = "#CC79A7",
  `dark yellow` = "#F5C710",
  amber = "#F5C710"
)
okabeito <- unname(okabeito_colors_list)[1:4]
names(okabeito) <- paste("Statement", LETTERS[1:4])

new_palette <- c(okabeito[1:3], okabeito_colors_list["amber"])
names(new_palette) <- paste("Statement", LETTERS[1:4])

# Nice data ---------------------------------------------------------------

set.seed(1234)
raw_data <-
  data.frame(
    x = seq(
      as.Date("01-01-2024", tryFormats = "%d-%m-%Y"),
      as.Date("01-12-2024", tryFormats = "%d-%m-%Y"),
      by = "months"
    ),
    y1 = rnorm(12, 40) / 100,
    y2 = rnorm(12, 30) / 100,
    y3 = rnorm(12, 10) / 100
  ) |>
  mutate(y0 = seq(0.5, 0.8, length.out = 12) + rnorm(12, 0, 0.05))
colnames(raw_data) <- c("x", paste("Statement", LETTERS[1:4]))
plot_data <- raw_data |>
  pivot_longer(
    -x,
    names_to = "category", values_to = "y"
  )
plot_data <- raw_data |>
  pivot_longer(
    -x,
    names_to = "category", values_to = "y"
  )


# Messy data --------------------------------------------------------------

set.seed(1234)
raw_data2 <-
  data.frame(
    x = seq(
      as.Date("01-01-2024", tryFormats = "%d-%m-%Y"),
      as.Date("01-12-2024", tryFormats = "%d-%m-%Y"),
      by = "months"
    ),
    y1 = runif(12),
    y2 = runif(12),
    y3 = runif(12)
  ) |>
  mutate(y0 = rnorm(12, 0, 0.1) + cumsum(y1) / max(cumsum(y1)))
colnames(raw_data2) <- c("x", paste("Statement", LETTERS[1:4]))
plot_data2 <- raw_data2 |>
  pivot_longer(
    -x,
    names_to = "category", values_to = "y"
  )
