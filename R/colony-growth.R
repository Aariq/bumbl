#' Fit breakpoint model to individual colony
#'
#' Fits models using a range of taus and picks the best one using maximum
#' likelihood. Typically only used internally by [bumbl()].
#'
#' @param data a dataframe or tibble
#' @param t the unquoted column name for the time variable in `data`
#' @param formula a formula passed to [glm()] or [MASS::glm.nb()].  This should
#'   include the time variable supplied to `t`
#' @param family a description of the error distribution and link function.
#'   This is passed to [glm()] except in the case of `family = "negbin"`, which
#'   causes [MASS::glm.nb()] to be used to fit a negative binomial GLM.
#' @param tau_optim_maxit passed to `optim()` which is used to find the optimal
#'   change point, tau.  Mostly used for testing purposes (to force convergence
#'   errors), but could be increased if optimal switchpoint doesn't converge.
#' @param ... additional arguments passed to [glm()] or [glm.nb()]
#' @return a tibble with a column for the winning tau and a column for the
#'   winning model
#' @seealso [bumbl()]
#' @import dplyr
#' @import rlang
#' @importFrom stats update logLik terms glm poisson as.formula gaussian
#' @importFrom MASS glm.nb
#' @keywords internal
brkpt <-
  function(data,
           t,
           formula,
           family = gaussian(link = "log"),
           tau_optim_maxit = 100,
           ...) {

    #low-level function to modify data and formula and fit breakpoint model
    brkpt_glm <- function(data, formula, t, tau, family, ...) {
      data2 <- mutate(data, .post = ifelse({{t}} <= tau, 0, {{t}} - tau))
      f <- update(formula, ~ . + .post)

      if(is.character(family) && family == "negbin") {
        #create new call in order to pass ... to glm()
        new_call <-
          as.call(c(list(
            sym("glm.nb"), formula = f, data = data2
          ), exprs(...)))
      } else {
        #create new call in order to pass ... to glm()
        new_call <-
          as.call(c(list(
            sym("glm"),
            formula = f,
            family = family,
            data = data2
          ), exprs(...)))
      }
      m <- eval(new_call)
      #TODO: capture warnings from eval(new_call)
      return(m)
    }

    #wrap so optim() can work on it.
    brkpt_wrap <- function(tau, data, formula, t, family, ...) {
      m <- suppressWarnings(brkpt_glm(
        data = data,
        formula = formula,
        t = {{t}},
        tau = tau,
        family = family,
        ...
      ))
      return(logLik(m))
    }

    m_optim <- stats::optim(
      par = mean(pull(data, {{t}})),
      fn = brkpt_wrap,
      data = data,
      formula = formula,
      t = {{t}},
      family = family,
      method = "L-BFGS-B",
      lower = min(pull(data, {{t}})),
      upper = max(pull(data, {{t}})),
      control = list(fnscale = -1, maxit = tau_optim_maxit)
    )
    if (m_optim$convergence != 0) {
      abort(message = "Search for optimal switchpoint did not converge")
    }
    tau_win <- m_optim$par
    m_win <-
      brkpt_glm(
        data = data,
        formula = formula,
        t = {{t}},
        tau = tau_win,
        family = family,
        ...
      )
    return(tibble(tau = tau_win, model = list(m_win)))
  }


#' Estimate colony growth, switch point, and decay parameters
#'
#' Fits generalized linear models that assume bumblebee colonies will switch
#' from growth to gyne production at some point, \eqn{\tau}. This allows for a
#' different switchpoint (\eqn{\tau}) for each colony, chosen by maximum
#' likelihood methods.
#'
#' @param data a dataframe or tibble with a column for colony ID (as a
#'   `character` or `factor`), a column for some measure of time (`numeric`),
#'   and a column for some measure of colony growth (`numeric`), at minimum.
#' @param t the unquoted column name of the time variable.
#' @param formula a formula with the form `response ~ time + covariates` where
#'   response is your measure of colony growth, time is whatever measure of time
#'   you have (date, number of weeks, etc.) and covariates are any optional
#'   co-variates you want to fit at the colony level.
#' @param family a description of the error distribution and link function.
#'   This is passed to [glm()] except in the case of `family = "negbin"`, which
#'   causes [MASS::glm.nb()] to be used to fit a negative binomial GLM.
#' @param colonyID the unquoted column name of the colony ID variable. This is
#'   required, so to run `bumbl()` on a single colony, you must create a dummy
#'   column with a colony ID.
#' @param augment when FALSE, `bumbl` returns a summary dataframe with one row
#'   for each colonyID.  When TRUE, it returns the original data with additional
#'   columns containing model coefficients. Cannot be used in conjunction with
#'   `keep.model = TRUE`.
#' @param keep.model If TRUE, then the output will contain a list-column with
#'   the models for each colony. This may be useful for extracting statistics
#'   and performing model diagnostics not provided by `bumbl()`. Learn more
#'   about working with list columns with `vignette("nest", package = "tidyr")`.
#' @param tau_optim_maxit passed to `optim()` which is used to find the optimal
#'   change point, tau.  Mostly used for testing purposes (to force convergence
#'   errors), but could be increased if optimal switchpoint doesn't converge.
#' @param ... additional arguments passed to [glm()] or [MASS::glm.nb()].
#'
#' @details Colony growth is modeled as increasing exponentially until the
#'   colony switches from producing workers to producing reproductive
#'   individuals (drones and gynes), at which time the workers die and gynes
#'   leave the colony, causing the colony to decline. The switch point,
#'   \eqn{\tau}, may vary among colonies. `bumbl()` finds the value of
#'   \eqn{\tau} that maximizes likelihood and this "winning" model is used to
#'   calculate statistics returned in the output. This function works by fitting
#'   generalized linear models (GLMs) to modified colony growth data. Because of
#'   this, the assumptions for GLMs apply, namely independence and homogeneity
#'   of variance. See `vignette("bumbl", package = "bumbl")` for more details on
#'   the underlying math of the model.
#' @note This function *assumes* there is a switchpoint and does not test
#'   whether the switchpoint model is significantly better than a log-linear
#'   model. As a result, it may estimate a switchpoint even if the data do not
#'   represent a true switchpoint. See the vignette for an example of how to
#'   extract the GLMs---you could compare them to a simpler log-linear model
#'   without the switchpoint by AIC or a likelihood ratio test to test the
#'   significance of the switchpoint.
#' @seealso [plot.bumbldf()]
#'
#' @return A `data.frame` with the additional class `bumbldf` containing a
#'   summary of the data with a row for every colony and the following columns:
#'   \itemize{
#'   \item{`converged` indicates whether the winning model converged.}
#'   \item{`tau` is the switchpoint, in the same units as `t`, for
#'   each `colonyID`.  The colony grows for \eqn{\tau} weeks, then begins to
#'   decline in week \eqn{\tau + 1}.}
#'   \item{`logN0` is the intercept of the
#'   growth function.  It reflects actual initial colony size, if the colony
#'   initially grows exponentially.  It would also be lower if there were a few
#'   weeks lag before growth started in the field.}
#'   \item{`logLam` is the
#'   average (log-scale) colony growth rate (i.e., rate of weight gain per unit
#'   `t`) during the growth period.}
#'   \item{`decay` reflects the rate of decline during the decline period.
#'   Equivalent to ln(\eqn{\delta}) - ln(\eqn{\lambda}) (see vignette for more
#'   in-depth explanation).}
#'   \item{`logNmax` is the maximum weight reached by each colony.  It is a
#'   function of `tau`, `logN0` and `logLam`}
#'   \item{Additional columns are
#'   coefficients for any covariates supplied in the `formula`}
#'   }
#'   When `augment = TRUE`, the original data are returned with these columns as
#'   well as fitted values (`.fitted`) residuals (`.resid`) and standard error
#'   (`.se.fit`).  When `keep.model = TRUE` a list-column with the `glm` models
#'   for each colony is returned as well.
#'
#' @references Crone EE, Williams NM (2016) Bumble bee colony dynamics:
#'   quantifying the importance of land use and floral resources for colony
#'   growth and queen production. Ecology Letters 19:460–468.
#'   https://doi.org/10.1111/ele.12581
#'
#' @import tidyr
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map_dbl map2_df
#' @import broom
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \donttest{
#' bumbl(bombus, colonyID = colony, t = week, formula = d.mass ~ week)
#' }
bumbl <-
  function(data,
           t,
           formula,
           family = gaussian(link = "log"),
           colonyID = NULL,
           augment = FALSE,
           keep.model = FALSE,
           tau_optim_maxit = 100,
           ...) {

    if (!inherits(data, "data.frame")) {
      abort("`data` must be a data frame or tibble.")
    }
    if (!is.logical(augment) | length(augment) > 1) {
      abort("`augment` must be logical (TRUE or FALSE).")
    }
    colonyID <- enquo(colonyID)
    t <- enquo(t)
    tvar <- as_name(t)
    more_args <- list2(...)
    formula <- formula(formula)
    fterms <- terms(formula)

    # Check that colonyID was input
    if (quo_is_null(colonyID) || !as_name(colonyID) %in% colnames(data)) {
      abort("The name of the colony ID column must be supplied to 'colonyID'.")
    }


    # Check that at most only one of augment and keep.model are TRUE
    if (augment == TRUE & keep.model == TRUE){
      abort("'augment' and 'keep.model' cannot both be TRUE.")
    }

    # Check types of variables
    if (!is.numeric(data[[tvar]])){
      abort(paste0("Time variable,", tvar, "must be numeric."))
    }

    #Check for missing values in time variable
    if (any(!is.finite(data[[tvar]]))) {
      abort("There must not be missing or undefined values in the time variable")
    }

    #Check that time variable is in the formula
    if (!tvar %in% attr(fterms, "term.labels")) {
      abort(paste0("'", tvar, "' is missing from the model formula"))
    }
    df <-
      data %>%
      # make sure colonyID is a character vector
      mutate(!!colonyID := as.character(!!colonyID)) %>%
      group_by(!!colonyID)

    dflist <- group_split(df)
    names(dflist) <- group_keys(group_by(data, !!colonyID))[[1]]

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
                                        t = !!t,
                                        formula = formula,
                                        family = family,
                                        tau_optim_maxit = tau_optim_maxit,
                                        !!!more_args),
                                  .y),
                     .id = as_name(colonyID))

    if (nrow(resultdf) == 0) {
      abort("Model fitting failed for all colonies.")
    }

    predictdf <-
      resultdf %>%
      dplyr::select(-"tau") %>%
      mutate(aug = purrr::map(.data$model, ~broom::augment(.x, se_fit = TRUE))) %>%
      tidyr::unnest("aug") %>%
      dplyr::select(!!colonyID, !!t, ".fitted", ".se.fit", ".resid")

    modeldf_long <-
      resultdf %>%
      mutate(coefs = purrr::map(.data$model, broom::tidy)) %>%
      tidyr::unnest("coefs") %>%
      # prepend coefs with "beta_" so colnames aren't duplicated if joined to original data.
      mutate(term = ifelse(
        !.data$term %in% c("(Intercept)", ".post", tvar),
        paste0("beta_", .data$term),
        .data$term
      ))

    modeldf <-
      modeldf_long %>%
      dplyr::select(!!colonyID, "tau", "model", "term", "estimate") %>%
      spread(key = "term", value = "estimate") %>%
      mutate(logNmax = purrr::map_dbl(.data$model,
                                      ~ max(predict(.))),
             converged = purrr::map_lgl(.data$model, ~.x$converged)) %>%
      dplyr::select(
        !!colonyID,
        "model",
        "converged",
        "tau",
        logN0 = "(Intercept)",
        logLam = !!t,
        decay = ".post",
        "logNmax",
        everything()
      )

    if (keep.model == FALSE) {
      modeldf <- select(modeldf, -"model")
    }
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
