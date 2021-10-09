
library(tidyverse)
library(rstan)

# simple lm ---------------------------------------------------------------

df <- mtcars
df <- map_dfr(1:10, ~mtcars)

glm1 <- glm(mpg ~ wt, data = df)
summary(glm1)

new_data <- tibble(
  wt = c(4.8, 2, 3.5)
)

pred <- predict(glm1, newdata = new_data, se.fit = TRUE)
pred

tibble(
  lower_95 = pred$fit - qnorm(0.975)*sqrt(pred$se.fit^2 + pred$residual.scale^2),
  lower_50 = pred$fit - qnorm(0.75)*sqrt(pred$se.fit^2 + pred$residual.scale^2),
  median = pred$fit,
  upper_50 = pred$fit + qnorm(0.75)*sqrt(pred$se.fit^2 + pred$residual.scale^2),
  upper_95 = pred$fit + qnorm(0.975)*sqrt(pred$se.fit^2 + pred$residual.scale^2)
)

# stan --------------------------------------------------------------------

dat <- list(
  N = nrow(df),
  x = df$wt,
  y = df$mpg,
  x_new = new_data$wt
)

stan1 <- stan("stan-regression.stan", data = dat)
plot(stan1)
stan1
