#' Bumblebee worker mass as a function of intertegular span (ITS)
#'
#' A relationship between bumblebee worker intertegular span and mass, described
#' in Kerr et al. 2020. Specifically, mass = -0.03631937362579  +
#' (0.0443352875958048 * wkr_size)
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of worker mass in grams
#' @export
#'
#' @references Kerr, N. Z., Williams, N. M., and Crone, E. E. (2020). Optimal
#'   life history strategies in a eusocial insect: the contribution of worker
#'   size polymorphism during colony establishment and growth phases in
#'   bumblebee colonies. Journal of Animal Ecology (*in prep*).
#'
#' @examples
#' mass_func(4.1)
mass_func <- function(wkr_size) {
  return(-0.03631937362579  + (0.0443352875958048 * wkr_size))
}


#' Bumblebee worker survival as a function of size
#'
#' Survival probabilities as a function of body size (intertegular span) for
#' bumblebee workers described in Kerr et al. 2019. Specifically, p =
#' plogis(4.50002431428571 - 0.495615057142857 * wkr_size)
#'
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of survival probabilities
#' @export
#'
#' @references Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates
#'   explains optimal worker size for resource return by bumblebee workers.
#'   Funct Ecol 33:467–478. doi: 10.1111/1365-2435.13251
#'
#' @examples
#' surv_func(4)
surv_func <- function(wkr_size) {
  return(plogis(4.50002431428571 - 0.495615057142857 * wkr_size))
}


#' Probability of foraging as a function of bumblebee worker size
#'
#' Foraging probability as a function of bumblebee worker size (intertegular
#' span), described in Kerr et al. 2019.
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of foraging probabilities
#' @export
#'
#' @references Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates
#'   explains optimal worker size for resource return by bumblebee workers.
#'   Funct Ecol 33:467–478. doi: 10.1111/1365-2435.13251
#'
#' @examples
#' forage_func(4)
forage_func <- function(wkr_size) {
  m1 <- -10.262929037574773 + (5.929037608743619 * wkr_size) + (-0.650980787040497 * wkr_size^2)
  m2 <- -2.39218349485354 + (1.36470460119029 * wkr_size)

  wts <- c(0.613097385000364, 0.386902614999636)

  m_avg <- plogis(m1) * wts[1] + plogis(m2) * wts[2]

  return(m_avg)
}


#' Probability of pollen return as a function of bumblebee worker size
#'
#' Probability of pollen return by a foraging worker bumblebee as a function of
#' worker size (intertegular span), described in Kerr et al. 2019.
#'
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of pollen return probabilities
#' @export
#'
#' @references Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates
#'   explains optimal worker size for resource return by bumblebee workers.
#'   Funct Ecol 33:467–478. doi: 10.1111/1365-2435.13251
#'
#' @examples
#' poln_ret_func(4)
poln_ret_func <- function(wkr_size) {
  m1 <- 7.98040838924227 + (-4.91214574943515 * wkr_size) + (0.653355474683423 * wkr_size^2)
  m2 <- -2.1959139376487 + (0.277436710524086  * wkr_size)
  m3 <- -1.12292287816589 + (0 * wkr_size)

  wts <- c(0.545262160009476, 0.253848107890376, 0.200889732100148)

  m_avg <- plogis(m1) * wts[1] + plogis(m2) * wts[2] + plogis(m3) * wts[3]

  return(m_avg)

}


#' Number of foraging trips per day as a function of bumblebee worker size
#'
#' Estimates the number of foraging trips per day of a worker bumblebee as a
#' function of body size (intertegular span) as described in Kerr et al. 2019.
#' Specifically, trips = exp(-10.441800289970 + (5.690241104687 * wkr_size) +
#' (-0.689656110476 * wkr_size^2))
#'
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of mean number of trips per day
#' @export
#'
#' @references Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates
#'   explains optimal worker size for resource return by bumblebee workers.
#'   Funct Ecol 33:467–478. doi: 10.1111/1365-2435.13251
#'
#' @examples
#' trips_func(4)
trips_func <- function(wkr_size) {
  return(exp(-10.441800289970 + (5.690241104687 * wkr_size) + (-0.689656110476 * wkr_size^2)))
}


#' Pollen mass as a function of bumblebee worker size
#'
#' The estimated pollen mass returned per foraging trip by a foraging worker
#' bumblebee as a function of body size (intertegular span) as described in Kerr
#' et al. 2019.
#'
#' @param wkr_size a vector of worker ITS values in mm
#'
#' @return a vector of pollen mass in grams
#' @export
#'
#' @references Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates
#'   explains optimal worker size for resource return by bumblebee workers.
#'   Funct Ecol 33:467–478. doi: 10.1111/1365-2435.13251
#'
#' @examples
#' poln_mass_func(4)
poln_mass_func <- function(wkr_size) {
  m1 <- -9.322439490493416 + (2.099089029256157 * wkr_size) + (-0.223761025727937 * wkr_size^2)
  m2 <- -5.724036797919059 + (0.291444174297121 * wkr_size)
  m3 <- -4.58078536341549 + (0 * wkr_size)

  wts <- c(0.508702819088624, 0.293356371483592, 0.197940809427784)

  m_avg <- exp(m1) * wts[1] + exp(m2) * wts[2] + exp(m3) * wts[3]

  return(m_avg)

}


#' Integral Projection Model for Bumblebee Colony Growth
#'
#' Builds an size- and age-based integral projection model to estimate bumblebee
#' colony growth using vital rates estimated from previously published empirical
#' work, as described in Kerr, et al. (*in prep*).
#'
#' @param larv_surv Larval survival rate.
#' @param dev_time_mean Mean development time from egg to adult, in days.
#' @param wkr_size_min Minimum observed worker intertegular span (ITS), in mm.
#' @param wkr_size_max Maximum observed worker ITS, in mm.
#' @param wkr_size_mean Mean observed worker ITS, in mm.
#' @param wkr_size_sd Observed standard deviation of worker ITS.
#' @param poln_cost Grams of pollen needed to make a gram of new worker.  The
#'   default was estimated as the mean mass of pollen per cell / mean worker
#'   mass.
#' @param prop_foraging Proportion of workers allowed to forage.  When less than
#'   1, the smallest `1 - prop_foraging` workers do not contribute resources to
#'   recruitment of new larvae.
#' @param wkr_mass_f A function relating worker mass to worker ITS.
#' @param wkr_surv_f Worker survival as a function of worker ITS.
#' @param p_forage_f Probability of a worker making a foraging trip as a
#'   function of ITS.
#' @param p_poln_ret_f Probability of a worker returning from a foraging trip
#'   with pollen, as a function of ITS.
#' @param trips_f The average number of foraging trips per day as a function of
#'   worker ITS.
#' @param poln_mass_f The mass of pollen returned per trip as a function of ITS,
#'   in grams
#'
#' @return A list containing the full integral projection model and the colony
#'   growth rate, lambda.
#'
#' @details Defaults for all functions are estimated from data and published in
#'   Kerr et al. (*in prep*). Parameters ending in `_f` expect the name of a
#'   function or an anonymous function, for example, \code{bipm(wkr_mass_f =
#'   function(x) -0.4 + 0.05 * x)}. You can supply a constant as an anonymous
#'   function that returns a constant, e.g., \code{bipm(poln_mass_f = function(x)
#'   0.1)}. The default functions provided are derived from Kerr et al. 2019 and
#'   2020 and can be viewed in their help files or using \code{View()}.
#'
#' @seealso \code{\link{mass_func}}, \code{\link{surv_func}},
#'   \code{\link{forage_func}}, \code{\link{poln_ret_func}},
#'   \code{\link{trips_func}}, \code{\link{poln_mass_func}}
#'
#' @references Kerr, N. Z., Williams, N. M., and Crone, E. E. (2020). Optimal
#'   life history strategies in a eusocial insect: the contribution of worker
#'   size polymorphism during colony establishment and growth phases in
#'   bumblebee colonies. Journal of Animal Ecology (*in prep*).
#'
#'   Kerr NZ, Crone EE, Williams NM (2019) Integrating vital rates explains
#'   optimal worker size for resource return by bumblebee workers. Funct Ecol
#'   33:467–478. doi: 10.1111/1365-2435.13251
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
bipm <- function(larv_surv = 0.980419283573345,
                 dev_time_mean = 23.5670474993181,
                 wkr_size_min = 2.5,
                 wkr_size_max = 5.81,
                 # wkr_size_mean = 3.60068671381606,#Do I need to back-calculate from observed worker mass to get default size?  Observed mean mass is 0.1254111
                 wkr_size_mean = 3.64789499281592,
                 wkr_size_sd = 0.427668095606862,
                 poln_cost = 0.1275805,
                 prop_foraging = 1,
                 wkr_mass_f = mass_func,
                 wkr_surv_f = surv_func,
                 p_forage_f = forage_func,
                 p_poln_ret_f = poln_ret_func,
                 trips_f = trips_func,
                 poln_mass_f = poln_mass_func
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
  #(matches natalie's results)

  # Worker to worker
  wkr_size_1 <- wkr_size[1:length(wkr_size) - 1]
  wkr_surv <- wkr_surv_f(wkr_size_1)

  wkr_wkr_mat <- array(0, dim = c(n_wkr, n_wkr))
  diag(wkr_wkr_mat) <- wkr_surv
  #(matches natalie's results)

  # Worker to larva
  p_poln_return <- p_poln_ret_f(wkr_size_1)
  p_forage <- p_forage_f(wkr_size_1) #might need to set this to zero if prop_foraging < 1. Check manuscript.
  trips_per_day <- trips_f(wkr_size_1)
  poln_mass <- poln_mass_f(wkr_size_1)
  prop_nforaging <- 1 - prop_foraging

  daily_poln_return <- p_poln_return * p_forage * trips_per_day * poln_mass

  wkr_mass_mean <- wkr_mass_f(wkr_size_mean)
  #according to Natalie, the wkr_mass_mean should be the mass of the average worker, since only average sized workers are being produced.

  wkr_larv <- daily_poln_return / (poln_cost * wkr_mass_mean)

  #position in vector for cutoff for foraging workers.
  #When prop_nforaging = 1, foraging_index can be NA
  cum_prop <- cumsum(prop_wkr_size)
  #find first worker size class that exists and has a cumulative proportion of workers that exceeds proportion not foraging
  foraging_index <- match(TRUE, cum_prop > 0 & cum_prop >= prop_nforaging)
  min_wkr_index <- match(TRUE, prop_wkr_size > 0)

  if (prop_nforaging == 1) { #i.e., no foraging workers
    wkr_larv[] <- 0 #no recruitment
  } else if (foraging_index > min_wkr_index) { #some workers left behind
    wkr_larv[1:foraging_index - 1] <- 0
    border_case_correction <-
      1 - (prop_nforaging - cum_prop[foraging_index - 1]) / prop_wkr_size[foraging_index]
  } else if (foraging_index == min_wkr_index) { #i.e only part of the smallest size class doesn't forage
    border_case_correction <-
      1 - (prop_nforaging / prop_wkr_size[min_wkr_index]) #if prop_foraging = 1 this equals 1
  }

  if (prop_nforaging < 1) {
    wkr_larv[foraging_index] <-
      wkr_larv[foraging_index] * border_case_correction
  }

  wkr_larv_mat <- array(0, dim = c(n_larv, n_wkr))
  wkr_larv_mat[1, ] <- wkr_larv
  #(this is NOT the same as natalie's results)

  larv_mat <- rbind(larv_larv_mat, larv_wkr_mat)
  wkr_mat <- rbind(wkr_larv_mat, wkr_wkr_mat)
  full_mat <- cbind(larv_mat, wkr_mat)

  growth_rate <- c("lambda" = popbio::lambda(full_mat))
  print(growth_rate)

  invisible(list(lambda = growth_rate, ipm = full_mat))
}
