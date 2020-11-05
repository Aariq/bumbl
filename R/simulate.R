#' Simulate worker mass data with switchpoint
#'
#' This is the generative model assumed by the GLM implemented in `bumbl()` parameterized using estimates from the `bombus` dataset.
#'
#' @param seed numeric; passed to `set.seed()`. For reproducibility.
#' @importFrom stats rnorm
#' @noRd
#' @return a vector of colony growth with parameters used as attributes.
#'
#' @examples
#' sim_colony()
sim_colony <- function(seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }
  tau <- rnorm(1, 6.5, 1.87) #weeks
  # tau = rnorm(1, 6.5, 1.87) * 7 #days
  n0 <- abs(round(exp(rnorm(1, 3.25, 1.22))))
  lambda <- exp(rnorm(1, 0.39, 0.28))
  delta <- rnorm(1, 0.57, 0.17)
  tmax <- 20 #weeks
  # tmax = 20*7 #days

  #initialize worker number
  n <- c(n0, rep(NA_integer_, tmax-1))

  for (t in 2:tmax) {
    if (t <= tau) {
      n[t] <- lambda^t * n[1]
    } else {
      x <- t-tau
      n[t] <- lambda^tau * n[1] * delta ^ (t-tau)
    }
    n[n<0] <- 1 #no negative bees
  }
  attributes(n) <- list(tau = tau,
                        n0 = n0,
                        lambda = lambda,
                        delta = delta,
                        tmax = tmax)
  return(n)
}
