#' Fit breakpoint model to individual colony
#'
#' Fits models using a range of taus and picks the best one using maximum liklihood
#'
#' @param data a dataframe or tibble
#' @param colonyID unquoted name of colony ID column.  Only used for generating error messages.  Can be NULL if you're trying to call this function directly and not through `bumbl()`
#' @param taus a vector of taus to test
#' @param t the unquoted variable representing time in ___units?
#' @param formula a formula passed to `lm`
#'
#' @return dataframe with two columns
#'
#' @import dplyr
#' @import rlang
#'
#' @export
#'
#' @examples
#' testbees <- colony_weights[(colony_weights$ColonyID == 68), ]
#' mytaus <- (seq(2,8,0.1))
#' brkpt(testbees, ColonyID, mytaus, Round, log(TrueColonyWt_g) ~ Round)
brkpt <- function(data, colonyID = NULL, taus, t, formula){
  #TODO: make sure none of the variables are called '.post'
  fterms <- terms(formula)
  t <- enquo(t)
  colonyID <- enquo(colonyID)

  #Check that time variable is in the formula
  if(!quo_name(t) %in% attr(fterms, "term.labels")) {
    stop(paste0("'",quo_name(t),"'", " is missing from the model formula"))
    }

  #Check that at least some taus are in range of t
  if(all(taus > max(data[[quo_name(t)]]))) {
    stop("at least one tau must be in range of 't'")
  }
  #If some taus are out of range of t, drop them
  if(any(taus > max(data[[quo_name(t)]])) | any(taus < min(data[[quo_name(t)]]))){
    #TODO: does this need a warning message? If so, could be more specific
    # e.g. "For colony {colonyID} taus {unused taus} were not tested because they are outside of the range of {t}"
    # warn("Some taus were not used because they were outside of range of t")
    taus <- taus[taus <= max(data[[quo_name(t)]]) & taus >= min(data[[quo_name(t)]])]
  }

  # adds `.post` to formula. Would not be difficult to modify for other interactions
  f <- update(formula, ~. + .post)
  LLs <- c()

  for(i in 1:length(taus)){
    usetau = taus[i]
    data2 <- mutate(data, .post = ifelse(!!t <= usetau, 0, !!t - usetau))

    m0 = try(lm(f, data = data2))
    if(class(m0) != "try-error") LLs[i] = logLik(m0)
    #TODO: what if there is an error?
    #TODO: change to inherits()
    # LLs
  }
  tau_win <- taus[which(LLs == max(LLs))]

  # if multiple equivalent taus are found, this should fail
  if(length(tau_win) > 1){
    stop(paste0("For colony ", unique(data[[quo_name(colonyID)]]), " more than one equivalent tau found"))
  }
  #TODO: I don't really like that it re-fits the model.  I could have it save them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))
  m_win <- lm(f, data = data_win)
  return(tibble(tau = tau_win, model = list(m_win)))
}



#' Fit breakpoint growth models to many colonies.
#'
#' Fits a model that assumes bumblebee colonies will switch from growth to gyne production at some point, $\tau$.  This allows for a different switchpoint ($\tau$) for each colony, chosen by maximum liklihood methods.  The function returns the original dataframe augmented with model statistics.  See **Details** for more information.
#'
#' @param data a dataframe or tibble
#' @param colonyID the unquoted column name of the colony ID variable
#' @param taus vector of taus to test
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
#' @import purrr
#' @import broom
#'
#' @export
#'
#' @examples
#' data(colony_weights)
#' mytaus <- (seq(2,8,0.1))
#' bumbl(colony_weights,
#'       colonyID = ColonyID,
#'       taus = mytaus,
#'       t = Round,
#'       formula = log(TrueColonyWt_g) ~ Round)

bumbl <- function(data, colonyID, taus, t, formula){
  #TODO: create a sensible default for tau that's like seq(min(t), max(t), length.out = 100)

  models <-
    data %>%
    group_by({{colonyID}}) %>%
    tidyr::nest() %>%
    mutate(model = purrr::map(data, {
      ~(brkpt(., colonyID = {{colonyID}}, taus = taus, t = {{t}}, formula = formula))
      }))

  summary_data <-
    models %>%
    unnest(model) %>%
    mutate(coefs = map(model, broom::tidy)) %>%
    unnest(coefs, .preserve = "model") %>%
    select({{colonyID}}, tau, model, term, estimate) %>%
    spread(key = term, value = estimate) %>%
    mutate(logNmax = map_dbl(model, ~max(predict(.), na.rm = TRUE))) %>%
    select(-model) %>%
    select({{colonyID}}, tau, logNo = `(Intercept)`, loglam = {{t}}, decay = .post, everything())
  full_join(data, summary_data) #add a by= to get rid of warnings
}
