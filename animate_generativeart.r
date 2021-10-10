# load packages needed to run this code
library(generativeart)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)
library(gifski)
library(stringr)

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
seed <- strtoi(args[2])
seed
# this function makes generative art
make_frame <- function(seed = 4182, state = 1, formula) {
  # set seed
  set.seed(seed)

  # making a data frame with the visualization
  df <- seq(from = -pi, to = pi, by = 0.02) %>%
    expand.grid(x_i = ., y_i = .) %>%
    dplyr::mutate(!!!formula)

  # split up the data frame to extract the final state
  df %>% select(x, y) %>%
    mutate(state = state)
}

# set different formulas that will be used in the animation

x1 = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2))
x2 = quote(runif(1, -1, 1) * x_i^3 - sin(y_i^2))
x3 = quote(runif(1, -1, 1) * x_i^3 - sin(y_i^3))

y1 = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
y2 = quote(runif(1, -1, 1) * y_i^4 - cos(x_i^4))
y3 = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^3))

my_formula1 <- list(x = x1, y = y1)
my_formula2 <- list(x = x1, y = y2)
my_formula3 <- list(x = x1, y = y3)
my_formula4 <- list(x = x2, y = y1)
my_formula5 <- list(x = x2, y = y2)
my_formula6 <- list(x = x2, y = y3)
my_formula7 <- list(x = x3, y = y1)
my_formula8 <- list(x = x3, y = y2)
my_formula9 <- list(x = x3, y = y3)
formulas = list(my_formula1, my_formula2, my_formula3, my_formula4, my_formula5, my_formula6, my_formula7, my_formula8, my_formula9)

game_expanded <- function(color) {
  if (color <= 0.03928) {
    return(color / 12.92)
  } else {
    return(`^`((color + 0.055) / 1.055,2.4))
  }
}

# backgrounds = c("#aa33aa", "#4400ee", "#ffffee", "#338899", "#bb0022")
# backgrounds = c("#66bb22", "#442233", "#333311", "#007799", "#ffbb44")
source("palettes.r")
backgrounds = get_palette(seed)
whites = c();
blacks = c();
winner = "black"

for (i in 1:5) {
  value = backgrounds[i]
  rgb = col2rgb(value)
  r = rgb[1] / 255
  g = rgb[2] / 255
  b = rgb[3] / 255

  luminance = (0.2121 * game_expanded(r)) + (0.7154 * game_expanded(g)) + (0.0721 * game_expanded(b))
  if (luminance > 0.5) {
    blacks <- c(blacks, value[[1]]);
  } else {
    whites <- c(whites, value[[1]]);
  }
}

if (length(blacks) > length(whites)) {
  backgrounds = blacks
  winner = "black"
} else {
  backgrounds = whites
  winner = "white"
}


# make generative art for different seeds
selected_formulas = list()

for(i in sample(1:9, 5)) {
  selected_formulas <- c(selected_formulas, formulas[i])
}

frame1 = make_frame(seed, 1, selected_formulas[[1]])
frame2 = make_frame(seed, 2, selected_formulas[[2]])
frame3 = make_frame(seed, 3, selected_formulas[[3]])
frame4 = make_frame(seed, 4, selected_formulas[[4]])
frame5 = make_frame(seed, 5, selected_formulas[[5]])
df_animate = c(frame1, frame2, frame3, frame4, frame5)


if (length(backgrounds) == 3) {
  rbind(frame1, frame2, frame3) -> df_animate
} else {
  if (length(backgrounds) == 4) {
    rbind(frame1, frame2, frame3, frame4) -> df_animate
  } else {
    rbind(frame1, frame2, frame3, frame4, frame5) -> df_animate
  }
}

backgrounds
# make a data frame for background colors
df_back <- data.frame(
  xmin = -1.3, ymin = -1.3, xmax = 1.3, ymax = 1.3, x = 0, y = 0,
  fill = backgrounds,
  state = 1:length(backgrounds)
)

# make an animation
# we convert x and y from cartesian to polar coordinates
# we wanted to keep visualization in polar coordinates and we wanted the background to change colors using `gganimate`
# `gganimate` only works on geoms, so the background is drawn with geom_rect()
df_animate %>%
  group_by(state) %>%
  mutate(
    r = (y-min(y))/diff(range(y)), # convert x and y to polar coordinates
    theta = 2*pi*(x-min(x))/diff(range(x))
  ) %>%
  ggplot(ggplot2::aes(x = r*sin(theta), y = r*cos(theta))) +
  geom_rect(
    data = df_back,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill, group = 1, x = 1, y = 1),
    color = NA
  ) +
  scale_fill_identity() +
  geom_point(alpha = 0.2, size = 0, shape = 20, color = winner) +
  # labs(title = "{closest_state}") +
  theme_void() +
  coord_fixed(expand = FALSE) +
  transition_states(state, transition_length = 10, state_length = 0, wrap = TRUE) -> p_genart

# animate
animate(p_genart,
        device = "png",
        width = 600,
        height = 600,
	      renderer = gifski_renderer())

# save as gif
anim_save(str_interp("samples/${seed}.gif"))
