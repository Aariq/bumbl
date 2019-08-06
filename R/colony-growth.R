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
    #what if there is an error?
    # LLs
  }
  tau_win <- taus[which(LLs == max(LLs))]
  #TODO: ensure that only one winner is chosen in the case of multiple equivalently good Tau's
  # Possibly, if length(winner)> 1, then take median(winner) and re-fit model with that

  if(length(tau_win) > 1){
    tau_win <- median(tau_win)
  }
  # I don't really like that it re-fits the model.  I could have it save them all and only re-fit in the case of a tau tie.
  data_win <- mutate(data, .post = ifelse(!!t <= tau_win, 0, !!t - tau_win))
  m_win <- lm(f, data = data_win)
  return(tibble(tau = tau_win, model = list(m_win)))
}
