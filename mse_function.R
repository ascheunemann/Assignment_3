

mean_squared_error <- function(x, y) {
  return(list("training" = mean(resid(x)^2), "test" = mean(resid(y)^2)))
}
