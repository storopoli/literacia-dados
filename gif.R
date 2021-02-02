# retirado de https://stackoverflow.com/a/61500224
library(ggplot2)
library(magrittr)
library(ggforce)
library(gifski)
library(gganimate)
library(patchwork)

samples <- rnorm(100)
index <- seq(1:length(samples))
df <- tibble::tibble(value = samples, index = index)

bin_width <- 0.25

count_data <- # some minor data transformation
  df %>%
  dplyr::mutate(x = plyr::round_any(value, bin_width)) %>%
  dplyr::group_by(x) %>%
  dplyr::mutate(y = seq_along(x))

plot <- 
  ggplot(count_data) +
  geom_ellipse(aes(group = index, x0 = x, y0 = y, a = bin_width/2, b = 0.5, angle = 0), fill = 'black') +
  theme_bw() +
  ylab("Frequência") +
  coord_equal(bin_width) # to make the dots look nice and round

p_anim <- 
  plot +
  transition_states(states = index, transition_length = 100, state_length = 1) +
  shadow_mark() +
  enter_fly(y_loc = 12) 

p1 <- animate(p_anim, fps = 40, duration = 20)
save_animation(p1, "images/dist_anim.gif")
p2 <- ggplot(count_data, aes(x)) +
  geom_histogram(binwidth = bin_width, fill = "steelblue") +
  geom_density(aes(x, bin_width * ..count..), color = "black", size = 3) +
  ylab(NULL) +
  theme_bw()

ggsave(plot = p2, "images/dist.png", dpi = 300)
