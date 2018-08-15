## First load the packages needed for the assignment, using the pacman package to load them all at once

library(pacman)
p_load(tidyverse, car, broom, tidy, knitr, kableExtra, psych)


## Next load the data from the car package

data("Prestige")
prestige.dat <- Prestige

str(prestige.dat)
head(prestige.dat)


## Create a training set of 75 % of the observations and a test set of the remaining 25 % observations by randomly selecting training rows

set.seed(12)

prestige.train <- sample_frac(prestige.dat, 0.75)
prestige.test  <- setdiff(prestige.dat, prestige.train)

model1 <- lm(prestige ~ income + education + women, data = prestige.train)
summary(model1)

model2 <- lm(prestige ~ income + education + women, data = prestige.test)
summary(model2)

mse.function <- function(x, y) {
  mean(x$residuals^2)
  mean(y$residuals^2)
}
mse.function(model1, model2)

mse.f <-  function(x, y) {
  return(list("train" = mean(x$residuals^2),
       "test"= mean(y$residuals^2)))
}
mse.f(model1, model2)
