library(tidyverse)
library(palmerpenguins)

# bill length ~ interaction of body mass and sex
penguin_int_mod <- lm(bill_length_mm ~ body_mass_g + sex + body_mass_g:sex,
                      data = penguins)
summary(penguin_int_mod)

# short hand for interactions
penguin_int_mod2 <- lm(bill_length_mm ~ body_mass_g * sex, penguins)
