library(tidyverse)
theme_set(theme_classic(4))
set.seed(123)

# Hypothesis test -> starting from null distribution (H0)
# H0 assumes NO DIFFERENCE between p_hat1 and p_hat2
# If there is no difference, toss them all in the same bag

# 1. Let's simulate what a null distribution looks like- choose these!
# Larger sample size (n) = lower se = narrower histogram = less extreme vals 
p <- 0.5
n1 <- 60
n2 <- 40

# 2. Simulate samples from that null distribution- same p!!! 
# rbinom is for a simulation to create new data- for bootstrap or permutation use sample function
n_samples <- 1e4
x1 <- rbinom(n = n_samples,
             size = n1,
             prob = p)
x2 <- rbinom(n = n_samples,
             size = n2,
             prob = p)

# 3. Calculate statistic for each sample
p1_hat <- x1/n1
p2_hat <- x2/n2
diff_prop <- p2_hat - p1_hat

# what does the null distribution look like?
tibble(diff_prop) %>% 
  ggplot(aes(diff_prop)) +
  geom_histogram()

# 4. Standard error for the null hypothesis
# SE(p_hat), pooled proportions
p_hat <- (x1 + x2) / (n1 + n2)

se_null <- sqrt(p_hat * (1- p_hat) * (1 / n1 + 1 / n2))

# 5. Calculate p value
# pnorm() is the area under the curve
# why is mean 0- bc null dist, why is sd se_null- by def, why is first argument diff_prop
# what is probability of getting this value with this mean and se, at least this extreme
pval <- pnorm(diff_prop, mean = 0, sd = se_null, lower.tail = FALSE)
mean(pval <= 0.05)

# Visualize result
tibble(diff_prop,
       reject = pval <= 0.05) %>% 
  ggplot(aes(diff_prop, fill = reject)) +
  geom_histogram(binwidth = 0.02) +
  scale_fill_manual(values = c(`FALSE` = "cornflowerblue", `TRUE` = "firebrick")) +
  labs(x = "Difference in proportions") +
  theme(legend.position = "none") 
                
                