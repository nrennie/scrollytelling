library(gganimate)
library(scales)
library(gapminder)
library(ggplot2)
library(ggtext)
library(glue)
library(showtext)

# data
gapminder2007 <- gapminder |>
  filter(year == 2007)
# colours
col_palette <- c("#A053A1", "#DB778F", "#E69F52", "#09A39A", "#5869C7")
names(col_palette) <- unique(gapminder$continent)
bg_col <- "white"
highlight_col <- "#DB778F"
text_col <- "#40111C"
# fonts
font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Ubuntu"

p <- ggplot(
  data = gapminder,
  mapping = aes(
    x = gdpPercap, y = lifeExp,
    size = pop, colour = continent
  )
) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(
    values = col_palette
  ) +
  scale_size(range = c(2, 12)) +
  scale_x_log10(labels = label_comma()) +
  labs(
    title = "What does the world look like in {frame_time}?",
    x = "GDP per capita (inflation-adjusted US dollars)",
    y = "Life expectancy at birth (years)",
    subtitle = glue("Though every area of the world  has seen changes in life expectancy and GDP over time, there are still disparities between <span style='color:{col_palette[1]}'>**{names(col_palette)[1]}**</span>, <span style='color:{col_palette[2]}'>**{names(col_palette)[2]}**</span>, <span style='color:{col_palette[3]}'>**{names(col_palette)[3]}**</span>, <span style='color:{col_palette[4]}'>**{names(col_palette)[4]}**</span>, and <span style='color:{col_palette[5]}'>**{names(col_palette)[5]}**</span>.")
  ) +
  theme_bw(base_family = body_font, base_size = 13.2) +
  theme(
    legend.position = "none",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      family = body_font,
      face = "bold",
      margin = margin(b = 0, t = 5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      family = body_font,
      margin = margin(b = 10, t = 10)
    )
  ) +
  transition_time(year) +
  ease_aes("linear")
p_anim <- animate(p, height = 4, width = 6, units = "in", res = 300)

anim_save("posts/gapminder/images/animation.gif", animation = p_anim)
