#' Integral Projection Model for Bumblebee Colony Growth
#'
#' A description I have yet to write...
#'
#' @param larv_surv larval survival rate
#' @param dev_time_mean mean development time from egg to adult, in days
#' @param wkr_size_min minimum observed worker intertegular span (ITS) in mm.
#' @param wkr_size_max maximum observed worker ITS
#' @param wkr_size_mean mean observed worker ITS
#' @param wkr_size_sd standard deviation of worker ITS
#' @param wkr_surv_f worker survival as a function of worker ITS
#' @param p_poln_ret_f probability of returning with pollen as a function of
#'   worker ITS
#' @param p_forage_f probability of making a foraging trip as a function of
#'   worker ITS
#' @param trips_f number of trips per day as a function of worker ITS
#' @param poln_mass_f the mass of pollen returned per trip as a function of ITS,
#'   in grams
#' @param wkr_mass_f a function relating worker mass to worker ITS
#' @param poln_per_cell mean mass of pollen per cell, in grams
#' @param prop_foraging proportion of workers allowed to forage.  When less than
#'   1, the smallest `1 - prop_foraging` workers do not contribute resources to
#'   recruitment of new larvae.
#'
#' @return a list containing the full integral projection model and the colony
#'   growth rate, lambda.
#'
#' @importFrom popbio lambda
#' @importFrom stats plogis dpois pnorm
#' @export
#'
#' @examples
#' # Original model:
#' bipm()
#' # Effect of reducing larval survival:
#' bipm(larv_surv = 0.97)
bipm <- function(larv_surv = 0.9804193,
                 dev_time_mean = 23.56705,
                 wkr_size_min = 2.5,
                 wkr_size_max = 5.81,
                 wkr_size_mean = 3.600687,
                 wkr_size_sd = 0.4276681,
                 wkr_surv_f = function(wkr_size) plogis(4.5000243 - 0.4956151 * wkr_size),
                 p_poln_ret_f = function(wkr_size) plogis(7.9804084 + (-4.9121457 * wkr_size) + (0.6533555 * wkr_size^2)),
                 p_forage_f = function(wkr_size) plogis(-2.392183 + 1.364705 * wkr_size),
                 trips_f = function(wkr_size) exp(-10.4418003 + (5.6902411 * wkr_size) + (-0.6896561 * wkr_size^2)),
                 poln_mass_f = function(wkr_size) exp(-5.7240368 + 0.2914442 * wkr_size),
                 wkr_mass_f = function(wkr_size) -0.03631937 + 0.04433529 * wkr_size,
                 poln_per_cell = 0.016,
                 prop_foraging = 1
) {
  # Larva to larva
  dev_time <- dpois(1:50, dev_time_mean)
  n_larv <- length(dev_time)

  larv_larv_mat <- array(0, dim = c(n_larv, n_larv))

  for (i in 1:(n_larv - 1)) {
    larv_larv_mat[i + 1, i] <- (1 - dev_time[i]) * larv_surv
  }

  # Larva to worker
  wkr_size <- seq(wkr_size_min, wkr_size_max, 0.01)
  n_wkr <- length(wkr_size) - 1
  n_larv <- length(dev_time)
  wkr_pdist <- pnorm(wkr_size, mean = wkr_size_mean, sd = wkr_size_sd)
  prop_wkr_size <- wkr_pdist[2:length(wkr_pdist)] - wkr_pdist[1:(length(wkr_pdist) - 1)]

  larv_wkr_mat <- array(0, dim = c(n_wkr, n_larv))

  for (i in 1:length(prop_wkr_size)) {
    larv_wkr_mat[i, ] <- dev_time * larv_surv * prop_wkr_size[i]
  }

  # Worker to worker
  wkr_size_1 <- wkr_size[1:length(wkr_size) - 1]
  wkr_surv <- wkr_surv_f(wkr_size_1)

  wkr_wkr_mat <- array(0, dim = c(n_wkr, n_wkr))
  diag(wkr_wkr_mat) <- wkr_surv

  # Worker to larva
  p_poln_return <- p_poln_ret_f(wkr_size_1)
  p_forage <- p_forage_f(wkr_size_1)
  trips_per_day <- trips_f(wkr_size_1)
  poln_mass <- poln_mass_f(wkr_size_1)
  prop_nforaging <- 1 - prop_foraging
  cum_prop <- cumsum(prop_wkr_size)
  foraging_index <- which.min(cum_prop < prop_nforaging) #position in vector for cutoff for foraging workers

  daily_poln_return <- p_poln_return * p_forage * trips_per_day * poln_mass

  wkr_mass <- wkr_mass_f(wkr_size_1)
  poln_per_wkrmass <- poln_per_cell / mean(wkr_mass)

  wkr_larv <- daily_poln_return / (poln_per_wkrmass *  wkr_mass)

  if (foraging_index > 1) {
    wkr_larv[1:foraging_index - 1] <- 0
    border_case_correction <-
      1 - (prop_nforaging - max(cum_prop[1:foraging_index - 1])) / prop_wkr_size[foraging_index]
  } else {
    border_case_correction <-
      1 - (prop_nforaging / prop_wkr_size[1]) #if prop_foraging = 1 this equals 1
  }
  wkr_larv[foraging_index] <- wkr_larv[foraging_index] * border_case_correction
  wkr_larv_mat <- array(0, dim = c(n_larv, n_wkr))
  wkr_larv_mat[1, ] <- wkr_larv

  larv_mat <- rbind(larv_larv_mat, larv_wkr_mat)
  wkr_mat <- rbind(wkr_larv_mat, wkr_wkr_mat)
  full_mat <- cbind(larv_mat, wkr_mat)

  growth_rate <- popbio::lambda(full_mat)
  print(growth_rate)

  invisible(list(lambda = growth_rate, ipm = full_mat))
}
