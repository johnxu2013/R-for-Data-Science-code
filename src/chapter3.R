1 / 200 * 30
(59 + 73 + 2) / 3
sin(pi / 2)
x <- 3 * 4
primes <- c(2, 3, 5, 7, 11, 13)
primes * 2
primes - 1

x
this_is_a_really_long_name <- 2.5

r_rocks <- 2^3
r_rocks

seq(from = 1, to = 10)
seq(1, 10)
x <- "hello world"

my_variable <- 10
my_variable

library(tidyverse)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm")
?mpg
ls("package:tidyverse")
tidyverse_packages()

my_bar_plot <- ggplot(mpg, aes(x = class)) +
  geom_bar()
my_scatter_plot <- ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
my_bar_plot
my_scatter_plot
ggsave(filename = "mpg-plot.png", plot = my_bar_plot)
