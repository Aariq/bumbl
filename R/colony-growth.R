#' Fit breakpoint model to individual colony
#' Fits models using a range of taus and picks the best one using maximum liklihood
#'
#' @param data a dataframe or tibble
#' @param taus a vector of taus to test
#' @param t the unquoted variable representing time in ___units?
#' @param formula a formula passed to `lm`
#'
#' @return dataframe with two columns
#'
#' @import dplyr
#' @import rlang
#' @export
#'
#' @examples
#' testbees <- colony_weights %>% filter(ColonyID == 68)
#' mytaus <- (seq(2,8,0.1))
#' brkpt(testbees, mytaus, Round, log(TrueColonyWt_g) ~ Round)
brkpt <- function(data, taus, t, formula){
  #TODO: make sure none of the variables are called '.post'
  fterms <- terms(formula)
  t <- enquo(t)

  #Check that time variable is in the formula
  if(!quo_name(t) %in% attr(fterms, "term.labels")) {
    stop("'t=' should specify the time variable in the formula")
    }
  #Check that at least some taus are in range of t
  if(all(taus > max(data[[quo_name(t)]]))) {
    stop("at least one tau must be in range of 't'")
  }
  #If some taus are out of range of t, drop them
  if(any(taus > max(data[[quo_name(t)]])) | any(taus < min(data[[quo_name(t)]]))){
    warn("Some taus were not used because they were outside of range of t")
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
    # LLs
  }
  tau_win <- taus[which(LLs == max(LLs))]

  # This should be documented in "Details":
  # If there is more than one equivalent tau (all have same LL), then re-fit model with median of those taus
  if(length(tau_win) > 1){
    tau_win <- median(tau_win)
  }
  #TODO: I don't really like that it re-fits the model.  I could have it save them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))
  m_win <- lm(f, data = data_win)
  return(tibble(tau = tau_win, model = list(m_win)))
}



#' Fit breakpoint growth models to many colonies.
#' Fits breakpoint growth models to each colony in a dataset and returns the estimated breakpoint (tau) and some other things...
#'
#' @param data a dataframe or tibble
#' @param colonyID the unquoted column name of the colony ID variable
#' @param taus vector of taus to test
#' @param t the unquoted column name of the time variable in (units???)
#' @param formula a formula passed to `lm()`
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' data(colony_weights)
#' mytaus <- (seq(2,8,0.1))
#' bumbl(colony_weights, ColonyID, mytaus, Round, log(TrueColonyWt_g) ~ Round)

bumbl <- function(data, colonyID, taus, t, formula){
  summary_data <-
    data %>%
    group_by({{colonyID}}) %>%
    nest() %>%
    mutate(model = map(data, ~brkpt(., taus, {{t}}, formula))) %>%
    unnest(model) %>%
    mutate(coefs = map(model, tidy)) %>%
    unnest(coefs, .preserve = "model") %>%
    select({{colonyID}}, tau, model, term, estimate) %>%
    spread(key = term, value = estimate) %>%
    mutate(logNmax = map_dbl(model, ~max(predict(.), na.rm = TRUE))) %>%
    select(-model) %>%
    select({{colonyID}}, tau, logNo = `(Intercept)`, loglam = {{t}}, decay = .post, everything())
  full_join(data, summary_data)
}
