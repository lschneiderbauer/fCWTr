library(hexSticker)
library(tidyverse)
library(showtext)
library(svglite) # to produce an svg

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Tiny5", db_cache = FALSE)
font_add_google("Micro 5", db_cache = FALSE)


morlet <- function(x, sigma) {
  exp(-x^2) * sin(2 * pi * x * sigma)
}

p <-
  tibble(x = seq(-2.5, 2.5, by = 0.025)) |>
  mutate(
    y = morlet(x, 1),
    c = abs(x)
  ) |>
  ggplot(aes(x = x, y = y)) +
  # geom_line(linewidth = 1.5) +
  geom_point(size = 0.8, shape = "square") +
  # scale_color_viridis() +
  theme_void() +
  theme(
    legend.position = "none"
  )
p

# hexSticker conventions (by experiment)
# (x,y) = (1,1) seems to be the center of hex
# (x,y) = (0,0) is left bottom corner

sticker(
  p,
  package = "",
  s_x = 1, s_y = 1,
  s_width = 2, s_height = 1.6,
  h_fill = "white",
  h_color = "black",
  url = "fCWTr",
  #u_size = 26, # 26 for png
  u_size = 9, # for svg
  u_family = "Micro 5",
  u_x = 1.15,
  u_y = 0.23,
  filename = "inst/figures/hex-logo.svg"
) |> plot()
