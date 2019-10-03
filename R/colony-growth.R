#' Fit breakpoint model to individual colony
#'
#' Fits models using a range of taus and picks the best one using maximum liklihood
#'
#' @param data a dataframe or tibble
#' @param taus an optional vector of taus to test. If not supplied, `seq(min(t), max(t), length.out = 50)` will be used
#' @param t the unquoted column name for the time variable in `data`
#' @param formula a formula passed to `lm`.  This should include the time variable supplied to `t`
#'
#' @return a tibble with a column for the winning tau and a column for the winning model
#'
#' @import dplyr
#' @import rlang
#' @importFrom stats lm update logLik terms
#' @export
#'
#' @examples
#' testbees <- bombus[bombus$colony == 9, ]
#' # Using dates
#' brkpt(testbees, t = date, formula = log(mass) ~ date)
#' # Using weeks
#' brkpt(testbees, t = week, formula = log(mass) ~ week)
brkpt <- function(data, taus = NULL, t, formula, count_data = FALSE){
  #TODO: make sure none of the variables are called '.post'?
  fterms <- terms(formula)
  t <- enquo(t)
  tvar <-as_name(t)

  #reminder that you don't need to log transform when using count_data = TRUE
  if(count_data == TRUE & any(grepl("log", all.names(update(formula, . ~ 0 ))))){
    warning(
      "Are you sure you meant to log-transform the response variable? If count_data = TRUE, a log-link poisson glm is used."
    )
  }

  if(is.null(taus)){
    tvec <- data[[tvar]]
    taus <- seq(min(tvec), max(tvec), length.out = 50)
  }

  #Check that time variable is in the formula
  if(!tvar %in% attr(fterms, "term.labels")) {
    # abort(glue::glue("'{tvar}' is missing from the model formula"))
    abort(paste0("'",tvar,"' is missing from the model formula"))
    }

  #Check that at least some taus are in range of t
  if(all(taus > max(data[[tvar]])) | all(taus < min(data[[tvar]]))) {
    # abort(glue::glue("At least one tau must be in range of '{tvar}'"))
    abort(paste0("At least one tau must be in range of '", tvar, "'"))
  }
  #If some taus are out of range of t, drop them
  if(any(taus > max(data[[tvar]])) | any(taus < min(data[[tvar]]))){
    # warning(glue::glue("Some taus were not used because they were outside of range of '{tvar}'"))
    warning(paste0("Some taus were not used because they were outside of range of '", tvar, "'"))
    taus <- taus[taus <= max(data[[tvar]]) & taus >= min(data[[tvar]])]
  }

  # adds `.post` to formula. Would not be difficult to modify for other interactions
  f <- update(formula, ~. + .post)
  LLs <- c()
  if(count_data == TRUE){
    for(i in 1:length(taus)){
      usetau = taus[i]
      data2 <- mutate(data, .post = ifelse(!!t <= usetau, 0, !!t - usetau))

      m0 = try(glm(f, family = poisson(link = "log"), data = data2))
      if(!inherits(m0, "try-error")){
        LLs[i] = logLik(m0)
      } #else?
      #TODO: what if there is an error?
      # LLs
    }
  } else {
    for(i in 1:length(taus)){
      usetau = taus[i]
      data2 <- mutate(data, .post = ifelse(!!t <= usetau, 0, !!t - usetau))

      m0 = try(lm(f, data = data2))
      if(!inherits(m0, "try-error")){
        LLs[i] = logLik(m0)
      } #else?
      #TODO: what if there is an error?
      # LLs
    }
  }

  tau_win <- taus[which(LLs == max(LLs))]

  # if multiple equivalent taus are found, this should fail
  if(length(tau_win) > 1){
    abort("More than one equivalent tau found")
  }
  #TODO: I don't really like that it re-fits the model.  I could have it save them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))

  if(count_data == TRUE){
    m_win <- glm(f, family = poisson(link = "log"), data = data_win)
  } else {
    m_win <- lm(f, data = data_win)
  }
  return(tibble(tau = tau_win, model = list(m_win)))
}



#' Fit breakpoint growth models to many colonies.
#'
#' Fits a model that assumes bumblebee colonies will switch from growth to gyne production at some point, \eqn{\tau}.  This allows for a different switchpoint (\eqn{\tau}) for each colony, chosen by maximum liklihood methods.  The function returns the original dataframe augmented with model statistics.  See **Details** for more information.
#'
#' @param data a dataframe or tibble
#' @param colonyID the unquoted column name of the colony ID variable
#' @param taus an optional vector of taus to test. If not supplied, `seq(min(t), max(t), length.out = 50)` will be used.
#' @param t the unquoted column name of the time variable in (units???)
#' @param formula a formula passed to `lm()`
#' @param augment when FALSE, `bumbl` returns a summary dataframe with one row for each colonyID.  When TRUE, it returns the original data with additional columns containing model coefficients.
#'
#' @details Colony growth is modeled as increasing exponentialy until the colony switches to gyne production, at which time the workers die and gynes leave the colony, causing the colony to decline. The switch point, \eqn{\tau}, may vary among colonies.
#'
#' @return The original dataframe augmented with the following columns:
#' \itemize{
#' \item{`tau` is the switchpoint, in the same units as `t`, for each `colonyID`.  The colony grows for \eqn{\tau} weeks, then begins to decline in week \eqn{\tau + 1}.}
#' \item{`logN0` is the intercept of the growth function.  It reflects actual initial colony size, if the colony initially grows exponentially.  It would also be lower if there were a few weeks lag before growth started in the field.}
#' \item{`logLam` is the average (log-scale) colony growth rate (i.e., rate of weight gain per unit `t`) during the growth period.}
#' \item{`decay` reflects the rate of decline during the decline period. In fact, the way this model is set up, the actual rate of decline per unit `t` is calculated as `decay` - `logLam`.}
#' \item{`logNmax` is the maximum weight reached by each colony.  It is a function of `tau`, `logN0` and `logLam`}
#' \item{Additional columns are coefficients for any covariates supplied in the `formula`}
#' }
#'
#' @import tidyr
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map_dbl map2_df
#' @import broom
#' @importFrom glue glue
#' @importFrom utils packageVersion
#' @export
#'
#' @examples
#' # Colony 67 doesn't seem to ever switch to reproduction and results in an error
#' \dontrun{
#' bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week)
#'}
#'
#' bombus2 <- bombus[bombus$colony != 67, ]
#' bumbl(bombus2, colonyID = colony, t = week, formula = log(mass) ~ week)
bumbl <- function(data, colonyID, t, formula, augment = FALSE, taus = NULL){
  #TODO: scoop up all the warnings from brkpt() and present a summary at the end.
  # There was a change in the vehavior of unnest with version 1.0.0 of tidyr.  I dont' want to require tidyr 1.0.0 at this point because binaries aren't available for all platforms.  So this checks for the version the user has and implements the legacy version of unnest() if appropriate.

  if(packageVersion("tidyr") >= package_version("1.0.0")) {
    unnest <- tidyr::unnest_legacy
  }
  #TODO: Once tidyr 1.0.0 binaries are available for windows, require tidyr 1.0.0 or greater

  colonyID <- enquo(colonyID)
  t <- enquo(t)

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
    tryCatch(code,
             error = function(c){
               message(glue::glue("Warning: {c$message} for colonyID '{colonyID}'. Omitting from results."))
             })
  }

  resultdf <-
    map2_df(dflist,
            names(dflist),
           ~brkpt_w_err(brkpt(.x, taus = {{taus}}, t = !!t, formula = formula), .y),
           .id = as_name(colonyID))

  predictdf <-
    resultdf %>%
    dplyr::select(-"tau") %>%
    mutate(aug = map(.data$model, broom::augment)) %>%
    unnest(.data$aug) %>%
    dplyr::select(!!colonyID, !!t, ".fitted", ".se.fit", ".resid")

  modeldf <-
    resultdf %>%
    mutate(coefs = map(.data$model, broom::tidy)) %>%
    unnest(.data$coefs, .preserve = .data$model) %>%
    dplyr::select(!!colonyID, "tau", "model", "term", "estimate") %>%
    spread(key = "term", value = "estimate") %>%
    mutate(logNmax = map_dbl(.data$model, ~max(predict(.), na.rm = TRUE))) %>%
    dplyr::select(-"model") %>%
    dplyr::select(!!colonyID, "tau", logN0 = '(Intercept)', logLam = !!t, decay = '.post', "logNmax",
           everything())

  if(augment == TRUE){
    augmented_df <-
      left_join(df, modeldf, by = as_name(colonyID))

    full_augmented_df <-
      left_join(augmented_df, predictdf, by = c(as_name(colonyID), as_name(t)))

    #add attributes
    attr(full_augmented_df, "colonyID") <- as_name(colonyID)
    attr(full_augmented_df, "t") <- as_name(t)
    attr(full_augmented_df, "formula") <- formula
    attributes(full_augmented_df)

    class(full_augmented_df) <- c(class(full_augmented_df), "bumbldf")

    return(full_augmented_df)

  } else {
    return(modeldf)
  }

}
