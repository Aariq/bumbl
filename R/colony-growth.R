#' Fit breakpoint model to individual colony
#'
#' Fits models using a range of taus and picks the best one using maximum
#' liklihood
#'
#' @param data a dataframe or tibble
#' @param t the unquoted column name for the time variable in `data`
#' @param formula a formula passed to `glm()` or `glm.nb()`.  This should
#'   include the time variable supplied to `t`
#' @param family passed to `glm()`
#' @param taus an optional vector of taus to test. If not supplied, `seq(min(t),
#'   max(t), length.out = 50)` will be used
#' @param ... additional arguments passed to `glm()` or `glm.nb()`
#' @return a tibble with a column for the winning tau and a column for the
#'   winning model
#'
#' @import dplyr
#' @import rlang
#' @importFrom stats update logLik terms glm poisson as.formula gaussian
#'
#' @keywords internal
#'
#' @examples
#' testbees <- bombus[bombus$colony == 9, ]
#' # Using dates
#' \dontrun{
#' brkpt(testbees, t = date, formula = mass ~ date)
#' # Using weeks
#' brkpt(testbees, t = week, formula = mass ~ week)
#' }
brkpt <-
  function(data,
           t,
           formula,
           family = gaussian(link = "log"),
           taus = NULL,
           ...) {
    #TODO: make sure none of the variables are called '.post'?
  t <- enquo(t)
  tvar <- as_name(t)
  more_args <- list2(...)

  if (is.null(taus)) {
    tvec <- data[[tvar]]
    taus <- seq(min(tvec), max(tvec), length.out = 50)
  }

  #Check that at least some taus are in range of t
  if (all(taus > max(data[[tvar]])) | all(taus < min(data[[tvar]]))) {
    abort(paste0("At least one tau must be in range of '", tvar, "'"))
  }
  #If some taus are out of range of t, drop them
  if (any(taus > max(data[[tvar]])) |
      any(taus < min(data[[tvar]]))) {
    warning(paste0(
      "Some taus were not used because they were outside of range of '",
      tvar,
      "'"
    ))
    taus <-
      taus[taus <= max(data[[tvar]]) & taus >= min(data[[tvar]])]
  }

  # adds `.post` to formula.
  f <- update(formula, ~. + .post)
  LLs <- numeric()

  for (i in 1:length(taus)) {
    usetau <- taus[i]
    data2 <- mutate(data, .post = ifelse(!!t <= usetau, 0, !!t - usetau))
    m0 <- try(exec("glm", formula = f, family = family, data = data2, !!!more_args), silent = TRUE)
    if (inherits(m0, "try-error")) {
      LLs[i] <- NA
    } else {
      LLs[i] <- logLik(m0)
    }
  }

  if (all(is.na(LLs))) {
    abort("No valid values for tau found. \n Check for problems with the GLM specification or underlying data (e.g. impossible negative values)")
  }

  tau_win <- taus[which(LLs == max(LLs, na.rm = TRUE))]

  # if multiple equivalent taus are found, this should fail
  if (length(tau_win) > 1) {
    abort("More than one equivalent tau found")
  }

  #TODO: I don't really like that it re-fits the model.  I could have it save
  #them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))

  m_win <-
    exec(
      "glm",
      formula = f,
      family = family,
      data = data_win,
      !!!more_args
    )

  return(tibble(tau = tau_win, model = list(m_win)))
}


#' @describeIn brkpt passes arguments to `MASS::glm.nb()`
#' @param link passed to `glm.nb()`
#'
#' @import dplyr
#' @import rlang
#' @importFrom stats update logLik terms as.formula
#' @importFrom MASS glm.nb
#'
#' @keywords internal
#'
brkpt.nb <- function(data, taus = NULL, t, formula, link = "log", ...) {
  #TODO: make sure none of the variables are called '.post'?
  t <- enquo(t)
  tvar <- as_name(t)
  more_args <- list2(...)

  if (is.null(taus)) {
    tvec <- data[[tvar]]
    taus <- seq(min(tvec), max(tvec), length.out = 50)
  }

  #Check that at least some taus are in range of t
  if (all(taus > max(data[[tvar]])) | all(taus < min(data[[tvar]]))) {
    abort(paste0("At least one tau must be in range of '", tvar, "'"))
  }
  #If some taus are out of range of t, drop them
  if (any(taus > max(data[[tvar]])) |
      any(taus < min(data[[tvar]]))) {
    warning(paste0(
      "Some taus were not used because they were outside of range of '",
      tvar,
      "'"
    ))
    taus <-
      taus[taus <= max(data[[tvar]]) & taus >= min(data[[tvar]])]
  }

  # adds `.post` to formula.
  f <- update(formula, ~. + .post)
  LLs <- c()

    for (i in 1:length(taus)) {
      usetau <- taus[i]
      data2 <- mutate(data, .post = ifelse(!!t <= usetau, 0, !!t - usetau))
      m0 <- try(exec("glm.nb", formula = f, data = data2, link = link, !!!more_args))
      if (inherits(m0, "try-error")) {
        LLs[i] <- NA
      } else {
        LLs[i] <- logLik(m0)
      }
    }

  if (all(is.na(LLs))) {
    abort("No valid values for tau found. \n Check for problems with the GLM specification or underlying data (e.g. impossible negative values)")
  }

  tau_win <- taus[which(LLs == max(LLs, na.rm = TRUE))]

  # if multiple equivalent taus are found, this should fail
  if (length(tau_win) > 1) {
    abort("More than one equivalent tau found")
  }
  #TODO: I don't really like that it re-fits the model.  I could have it save
  #them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))
  m_win <- exec("glm.nb", formula = f, data = data_win, !!!more_args)
  return(tibble(tau = tau_win, model = list(m_win)))
}



#' Estimate colony growth, switch point, and decay parameters
#'
#' Fits models that assume bumblebee colonies will switch from growth to gyne
#' production at some point, \eqn{\tau}. This allows for a different
#' switchpoint (\eqn{\tau}) for each colony, chosen by maximum liklihood
#' methods.
#'
#' @param data a dataframe or tibble
#' @param t the unquoted column name of the time variable in (units???)
#' @param formula a formula with the form `response ~ time + covariates` where
#'   response is your measure of colony growth, time is whatever measure of time
#'   you have (date, number of weeks, etc.) and covariates are any optional
#'   co-variates you want to fit at the colony level.
#' @param family passed to `glm()`.
#' @param colonyID the unquoted column name of the colony ID variable
#' @param augment when FALSE, `bumbl` returns a summary dataframe with one row
#'   for each colonyID.  When TRUE, it returns the original data with additional
#'   columns containing model coefficients.
#' @param taus an optional vector of taus to test. If not supplied, `seq(min(t),
#'   max(t), length.out = 50)` will be used.
#' @param ... additional arguments passed to `glm()` or `glm.nb()`.
#'
#' @details Colony growth is modeled as increasing exponentialy until the colony
#'   switches to gyne production, at which time the workers die and gynes leave
#'   the colony, causing the colony to decline. The switch point, \eqn{\tau},
#'   may vary among colonies. The model is described further in Crone and Williams (2016).
#'
#' @return The original dataframe augmented with the following columns:
#'   \itemize{
#'   \item{`tau` is the switchpoint, in the same units as `t`, for
#'   each `colonyID`.  The colony grows for \eqn{\tau} weeks, then begins to
#'   decline in week \eqn{\tau + 1}.}
#'   \item{`logN0` is the intercept of the
#'   growth function.  It reflects actual initial colony size, if the colony
#'   initially grows exponentially.  It would also be lower if there were a few
#'   weeks lag before growth started in the field.}
#'   \item{`logLam` is the
#'   average (log-scale) colony growth rate (e.g., rate of weight gain per unit
#'   `t`) during the growth period.}
#'   \item{`decay` reflects the rate of decline
#'   during the decline period. In fact, the way this model is set up, the
#'   actual rate of decline per unit `t` is calculated as `decay` - `logLam`.}
#'   \item{`logNmax` is the maximum weight (or count) reached by each colony.  It is a
#'   function of `tau`, `logN0` and `logLam`}
#'   \item{Additional columns are
#'   coefficients for any covariates supplied in the `formula`}
#'   }
#' @references Crone, E. E., and Williams, N. M. (2016). Bumble bee colony
#'   dynamics: quantifying the importance of land use and floral resources for
#'   colony growth and queen production. Ecol. Lett. 19, 460–468.
#'   https://doi.org/10.1111/ele.12581
#' @import tidyr
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map_dbl map2_df
#' @import broom
#' @importFrom glue glue
#' @export
#'
#' @examples
#' # Colony 67 doesn't ever switch to reproduction and results in an error
#' \dontrun{
#' bumbl(bombus, colonyID = colony, t = week, formula = mass ~ week)
#'}
#'
#' bombus2 <- bombus[bombus$colony != 67, ]
#' bumbl(bombus2, colonyID = colony, t = week, formula = mass ~ week)
bumbl <-
  function(data,
           t,
           formula,
           family = gaussian(link = "log"),
           colonyID = NULL,
           augment = FALSE,
           taus = NULL,
           ...) {

  colonyID <- enquo(colonyID)
  t <- enquo(t)
  tvar <- as_name(t)
  more_args <- list2(...)
  fterms <- terms(formula)

  #Check that time variable is in the formula
  if (!tvar %in% attr(fterms, "term.labels")) {
    abort(paste0("'", tvar, "' is missing from the model formula"))
  }

  if (quo_is_null(colonyID)) {
    df <- data
    dflist <- list("NA" = data)
    colonyID <- "colony"

  } else {
    df <-
      data %>%
      # make sure colonyID is a character vector
      mutate(!!colonyID := as.character(!!colonyID)) %>%
      group_by(!!colonyID)

    dflist <- group_split(df)
    names(dflist) <- group_keys(group_by(data, !!colonyID))[[1]]
  }

  model_list <- vector("list", length(dflist))
  names(model_list) <- names(dflist)

  brkpt_w_err <- function(code, colonyID) {
    tryCatch(
      code,
      error = function(c) {
        message(
          glue::glue(
            "Warning: {c$message} for colonyID '{colonyID}'. Omitting from results."
          )
        )
      }
    )
  }

  resultdf <-
    purrr::map2_df(dflist,
                   names(dflist),
                   ~brkpt_w_err(brkpt(data = .x,
                                      taus = {{taus}},
                                      t = !!t,
                                      formula = formula,
                                      family = family,
                                      !!!more_args),
                                .y),
                   .id = as_name(colonyID))

  if(nrow(resultdf) == 0) {
    abort("Model fitting failed for all colonies.")
  }

  predictdf <-
    resultdf %>%
    dplyr::select(-"tau") %>%
    mutate(aug = purrr::map(.data$model, ~broom::augment(.x, se_fit = TRUE))) %>%
    tidyr::unnest(.data$aug) %>%
    dplyr::select(!!colonyID, !!t, ".fitted", ".se.fit", ".resid")

  modeldf <-
    resultdf %>%
    mutate(coefs = purrr::map(.data$model, broom::tidy)) %>%
    tidyr::unnest(.data$coefs) %>%
    dplyr::select(!!colonyID, "tau", "model", "term", "estimate") %>%
    spread(key = "term", value = "estimate") %>%
    mutate(logNmax = purrr::map_dbl(.data$model,
                                    ~ max(predict(.),
                                          na.rm = TRUE))) %>%
    dplyr::select(-"model") %>%
    dplyr::select(
      !!colonyID,
      "tau",
      logN0 = "(Intercept)",
      logLam = !!t,
      decay = ".post",
      "logNmax",
      everything()
    )

  augmented_df <-
    left_join(df, modeldf, by = as_name(colonyID))

  full_df <-
    left_join(augmented_df, predictdf, by = c(as_name(colonyID), as_name(t)))
  if (augment == TRUE) {
    out <- full_df
  } else {
    out <- modeldf
    attr(out, "predict") <- full_df
  }

  attr(out, "colonyID") <- as_name(colonyID)
  attr(out, "t") <- as_name(t)
  attr(out, "formula") <- formula

  class(out) <- c("bumbldf", class(out))
  return(out)
}



#' @describeIn bumbl passes arguments to `MASS::glm.nb()` rather than `glm()`
#'
#' @param link passed to `glm.nb()` from the `MASS` package
#'
#' @import tidyr
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map_dbl map2_df
#' @import broom
#' @importFrom glue glue
#' @export
#'
bumbl.nb <-
  function(data,
           colonyID = NULL,
           t,
           formula,
           link = "log",
           augment = FALSE,
           taus = NULL,
           ...) {

  colonyID <- enquo(colonyID)
  t <- enquo(t)
  tvar <- as_name(t)
  more_args <- list2(...)
  fterms <- terms(formula)

  #Check that time variable is in the formula
  if (!tvar %in% attr(fterms, "term.labels")) {
    abort(paste0("'", tvar, "' is missing from the model formula"))
  }

  if (quo_is_null(colonyID)) {
    df <- data
    dflist <- list("NA" = data)
    colonyID <- "colony"

  } else {
    df <-
      data %>%
      # make sure colonyID is a character vector
      mutate(!!colonyID := as.character(!!colonyID)) %>%
      group_by(!!colonyID)

    dflist <- group_split(df)
    names(dflist) <- group_keys(group_by(data, !!colonyID))[[1]]
  }

  model_list <- vector("list", length(dflist))
  names(model_list) <- names(dflist)

  brkpt_w_err <- function(code, colonyID) {
    tryCatch(
      code,
      error = function(c) {
        message(
          glue::glue(
            "Warning: {c$message} for colonyID '{colonyID}'. Omitting from results."
          )
        )
      }
    )
  }

  resultdf <-
    purrr::map2_df(dflist,
                   names(dflist),
                   ~brkpt_w_err(brkpt.nb(data = .x,
                                      taus = {{taus}},
                                      t = !!t,
                                      formula = formula,
                                      link = link,
                                      !!!more_args),
                                .y),
                   .id = as_name(colonyID))

  if(nrow(resultdf) == 0) {
    abort("Model fitting failed for all colonies.")
  }

  predictdf <-
    resultdf %>%
    dplyr::select(-"tau") %>%
    mutate(aug = purrr::map(.data$model, ~broom::augment(.x, se_fit = TRUE))) %>%
    tidyr::unnest(.data$aug) %>%
    dplyr::select(!!colonyID, !!t, ".fitted", ".se.fit", ".resid")

  modeldf <-
    resultdf %>%
    mutate(coefs = purrr::map(.data$model, broom::tidy)) %>%
    tidyr::unnest(.data$coefs) %>%
    dplyr::select(!!colonyID, "tau", "model", "term", "estimate") %>%
    spread(key = "term", value = "estimate") %>%
    mutate(logNmax = purrr::map_dbl(.data$model,
                                    ~ max(predict(.),
                                          na.rm = TRUE))) %>%
    dplyr::select(-"model") %>%
    dplyr::select(
      !!colonyID,
      "tau",
      logN0 = "(Intercept)",
      logLam = !!t,
      decay = ".post",
      "logNmax",
      everything()
    )

  augmented_df <-
    left_join(df, modeldf, by = as_name(colonyID))

  full_df <-
    left_join(augmented_df, predictdf, by = c(as_name(colonyID), as_name(t)))
  if (augment == TRUE) {
    out <- full_df
  } else {
    out <- modeldf
    attr(out, "predict") <- full_df
  }

  attr(out, "colonyID") <- as_name(colonyID)
  attr(out, "t") <- as_name(t)
  attr(out, "formula") <- formula

  class(out) <- c("bumbldf", class(out))
  return(out)
}
