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
#' testbees <- colony_weights[(colony_weights$ColonyID == 18), ]
#' brkpt(testbees, t = Round, formula = log(TrueColonyWt_g) ~ Round)
brkpt <- function(data, taus = NULL, t, formula){
  #TODO: make sure none of the variables are called '.post'
  fterms <- terms(formula)
  t <- enquo(t)
  tvar <-as_name(t)

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
  tau_win <- taus[which(LLs == max(LLs))]

  # if multiple equivalent taus are found, this should fail
  if(length(tau_win) > 1){
    abort("More than one equivalent tau found")
  }
  #TODO: I don't really like that it re-fits the model.  I could have it save them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))
  m_win <- lm(f, data = data_win)
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
#'
#' @details Colony growth is modeled as increasing exponentialy until the colony switches to gyne production, at which time the workers die and gynes leave the colony, causing the colony to decline. The switch point, \eqn{\tau}, may vary among colonies.
#'
#' @return The original dataframe augmented with the following columns:
#' \itemize{
#' \item{`tau` is the switchpoint, in the same units as `t`, for each `colonyID`.  The colony grows for \eqn{\tau} weeks, then begins to decline in week \eqn{\tau + 1}.}
#' \item{`logNo` is the intercept of the growth function.  It reflects actual initial colony size, if the colony initially grows exponentially.  It would also be lower if there were a few weeks lag before growth started in the field.}
#' \item{`loglam` is the average (log-scale) colony growth rate (i.e., rate of weight gain per unit `t`) during the growth period.}
#' \item{`decay` reflects the rate of decline during the decline period. In fact, the way this model is set up, the actual rate of decline per unit `t` is calculated as `decay` - `loglam`.}
#' \item{`logNmax` is the maximum weight reached by each colony.  It is a function of `tau`, `logNo` and `loglam`}
#' \item{Additional columns are coefficients for any covariates supplied in the `formula`}
#' }
#'
#' @import tidyr
#' @import rlang
#' @import dplyr
#' @importFrom purrr map map_dbl
#' @import broom
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' mydata <- filter(colony_weights, !ColonyID %in% c("68", "97"))
#' bumbl(mydata,
#'       colonyID = ColonyID,
#'       t = Round,
#'       formula = log(TrueColonyWt_g) ~ Round)

bumbl <- function(data, colonyID, taus = NULL, t, formula){
  #TODO: create a sensible default for tau that's like seq(min(t), max(t), length.out = 100)
  #TODO: scoop up all the errors and warnings from brkpt() and present a summary at the end.
  colonyID <- enquo(colonyID)
  df <-
    data %>%
    # make sure colonyID is a factor, drop any unused levels
    mutate(!!colonyID := as.factor(as.character(!!colonyID))) %>%
    group_by(!!colonyID)

  dflist <- group_split(df)

  names(dflist) <- group_keys(group_by(data, !!colonyID))[[1]]

  # for loop style:
  model_list <- vector("list", length(dflist))

  for(i in 1:length(dflist)){
    model_list[[i]] <-
      tryCatch(brkpt(dflist[[i]], taus = {{taus}}, t = {{t}}, formula = formula),
               error = function(c){
                 c$message <- paste0("For Colony ID '", names(dflist)[i], "': ", c$message)
                 stop(c)
               })
    names(model_list) <- names(dflist)
  }

  modeldf <-
    bind_rows(model_list, .id = rlang::as_name(colonyID)) %>%
    mutate(coefs = map(model, broom::tidy)) %>%
    unnest(coefs, .preserve = "model") %>%
    select(!!colonyID, "tau", "model", "term", "estimate") %>%
    spread(key = "term", value = "estimate") %>%
    mutate(logNmax = map_dbl(model, ~max(predict(.), na.rm = TRUE))) %>%
    select(-"model") %>%
    select(!!colonyID, "tau", logNo = '(Intercept)', loglam = {{t}}, decay = '.post', everything())

    augmented_df <- full_join(data, modeldf, by = as_name(colonyID))
    return(augmented_df)

  # Full-on {purrr} style.  This is probably faster, but I don't know how to get helpful error messages from brkpt.  Like, I don't know how to embed tryCatch into a map() function correctly.
  # modeldf <-
  #   map_dfr(dflist,
  #           ~brkpt(data = .x,
  #                  taus = {{taus}},
  #                  t ={{t}},
  #                  formula = formula),
  #           .id = as_name(colonyID)) %>%
  #   mutate(coefs = map(model, broom::tidy)) %>%
  #   unnest(coefs, .preserve = "model") %>%
  #   select(!!colonyID, tau, model, term, estimate) %>%
  #   spread(key = term, value = estimate) %>%
  #   mutate(logNmax = map_dbl(model, ~max(predict(.), na.rm = TRUE))) %>%
  #   select(-model) %>%
  #   select(!!colonyID, tau, logNo = `(Intercept)`, loglam = Round, decay = .post, everything())
  #
  # augmented_df <- full_join(data, modeldf, by = as_name(colonyID))
  # return(augmented_df)
}
