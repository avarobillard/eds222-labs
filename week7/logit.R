library(ggplot2)
library(tidyverse)

p <- seq(0.001, 0.999, length.out = 1e4)

logit_p <- log(p/(1-p))
ggplot(tibble(p, logit_p),aes(p, logit_p)) +
  geom_line()


x <- seq(0, 100, length.out = 1e4)
beta0 <- -5
beta1 <- 0.1
logit_p <- beta0 + beta1*x
ggplot(tibble(x, logit_p),
       aes(x, logit_p)) +
  geom_line()

p <- exp(logit_p) /
  (1 + exp(logit_p))
ggplot(tibble(x,p), aes(x,p)) +
  geom_line()

PctHisp <- seq(0, 100, length.out = 1e3)
beta0 <- -12
beta1 <- 0.5
logit_p <- beta0 + beta1 * PctHisp
p <- exp(logit_p) / (1 + exp(logit_p))
ggplot(tibble(PctHisp, p),
       aes(PctHisp, p)) +
  geom_line()