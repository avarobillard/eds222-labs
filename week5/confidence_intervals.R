library(tidyverse)
theme_set(theme_classic(14))
set.seed(123)

# 1. Define your population- note difference in p1 & p2- assuming there is a difference!
p1 <- 0.42
p2 <- 0.55
n1 <- 60
n2 <- 40

# 2. Simulate many samples
n_samples <- 1e4
x1 <- rbinom(n = n_samples,
             size = n1,
             prob = p1)
x2 <- rbinom(n = n_samples,
             size = n2,
             prob = p2)

# 3. Calculate statistic for each sample (this is same as hyp. test stat)
p1_hat <- x1 / n1
p2_hat <- x2 / n2
diff_prop <- p2_hat - p1_hat

# Here's the distribution of our samples
tibble(p1_hat,
       p2_hat) %>% 
  pivot_longer(c(p1_hat, p2_hat),
               names_to = "treatment",
               values_to = "prop_success") %>% 
  ggplot(aes(prop_success)) +
  geom_histogram() +
  facet_wrap(~ treatment) 

# 4. Calculate the SE for the CI
se_diff_prop <- sqrt(p1_hat * (1 - p1_hat) / n1 +
                       p2_hat * (1 - p2_hat) / n2)

# 5. Calculate the CI for each sample- note mean (centered at difference) and sd
ci_95_lwr <- qnorm(0.025, mean = diff_prop, sd = se_diff_prop)
ci_95_upr <- qnorm(0.975, mean = diff_prop, sd = se_diff_prop)

# 6. Calculate coverage- is true diff in CI?
is_covered <- ci_95_lwr <= p2 - p1 & p2 - p1 <= ci_95_upr
coverage <- mean(is_covered)
coverage

# Visualize the CI's
tibble(sample = 1:n_samples,
       diff_prop,
       ci_95_lwr,
       ci_95_upr,
       is_covered) %>% 
  sample_n(100) %>% 
  ggplot(aes(diff_prop, sample, xmin = ci_95_lwr, xmax = ci_95_upr, color = is_covered)) +
  geom_pointrange()
