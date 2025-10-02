library(tidyverse)

#1. define the possible outcomes
foo <- tibble(
  x = seq(0, 1, length.out = 100)
)

#2. choose our parameters
shape1 <- 8
shape2 <- 2

#3. calculate density
foo <- mutate(foo, density = dbeta(x, shape1, shape2))

#4. plot it
ggplot(foo, aes(x, density)) +
  geom_line()



