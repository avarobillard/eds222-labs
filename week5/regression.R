library(tidyverse)
set.seed(123)

# Let's define our population
beta0 <- -10
beta1 <- 2
sigma <- 3

# x values
temperature <- rnorm(200, mean = 25, sd = 2)
# y values (mean is equation)
bleaching <- rnorm(200, mean = beta0 + beta1 * temperature, sd = sigma)

# scatter plot to visualize data
bleaching_df <- tibble(temperature, bleaching)
ggplot(bleaching_df, aes(temperature, bleaching)) +
  geom_point()

# model bleaching based on temp, look at summary
bleaching_lm <- lm(bleaching ~ temperature, bleaching_df)
summary(bleaching_lm)

# TO SIMULATE UNDER THE NULL HYPOTHESIS 
# WE HAVE TO ASSUME THERE'S NO RELATIONSHIP
# WE ASSUME BETA1 IS 0
beta0 <- -10
beta1 <- 0
sigma <- 3

# generate new y vals based on beta1 = 0
bleaching_null <- rnorm(200, mean = beta0 + beta1*temperature, sd = sigma)

# create null data based on this!
null_df <- tibble(temperature, bleaching_null)
ggplot(null_df, aes(temperature, bleaching_null)) +
  geom_point()

# fit model
null_lm <- lm(bleaching_null ~ temperature, null_df)
summary(null_lm)

# create null distribution by getting the beta1 value from null data x 1000
null_dist <- map_dbl(1:1000, \(i) {
  bleaching_null <- rnorm(200, mean = beta0 + beta1 * temperature, sd = sigma)
  null_df <- tibble(temperature, bleaching_null)
  null_lm <- lm(bleaching_null ~ temperature, null_df)
  coef(null_lm)[2]
})
hist(null_dist)


# Now let's permute
set.seed(123)


# Let's define our population- note we did not set beta1 to 0
beta0 <- -10
beta1 <- 2
sigma <- 22

temperature <- rnorm(200, mean = 25, sd = 2)
bleaching <- rnorm(200, mean = beta0 + beta1 * temperature, sd = sigma)

# create data frame and plot
bleaching_df <- tibble(temperature, bleaching)
ggplot(bleaching_df, aes(temperature, bleaching)) + 
  geom_point() +
  geom_smooth(method = "lm")

# fit model
bleaching_lm <- lm(bleaching ~ temperature, bleaching_df)
summary(bleaching_lm)

# do one permutation- randomly shuffle temperature- and get estimate of beta1
# we shuffle temp bc this is the independent variable, and bleaching is the response- but could do either
one_permutation <- mutate(bleaching_df,
                          temperature = sample(temperature))
permutation_est <- coef(lm(bleaching ~ temperature, one_permutation))[2]

# create null distribution by doing 1000 permutations for beta1
permutation_null <- map_dbl(1:10000, \(i) {
  one_permutation <- mutate(bleaching_df,
                            temperature = sample(temperature))
  coef(lm(bleaching ~ temperature, one_permutation))[2]
})

# plot null distribution
tibble(permutation_null) %>% 
  ggplot(aes(permutation_null)) +
  geom_histogram() +
  geom_vline(xintercept = coef(bleaching_lm)[2], color = "firebrick", linewidth = 2)

# get p value- where null dist is more extreme than the actual beta 1 value
perm_pval <- mean(abs(permutation_null) > coef(bleaching_lm)[2])
perm_pval
