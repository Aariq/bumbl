

#' Fit breakpoint model to individual colony
#' Fits models using a range of taus and picks the best one using maximum liklihood
#'
#' @param data
#' @param taus
#' @param t
#' @param formula
#'
#' @return dataframe with two columns
#' @export
#'
#' @examples
brkpt <- function(data, taus, t, formula){
  #TODO: make sure none of the variables are called '.post'
  #TODO: check that at least one of the formula elements is the same as `t`?
  #TODO: don't bother if Tau's extend past last t.  Don't even fit those models.  Warn user maybe (verbose = TRUE??)

  # adds `.post` to formula. Would not be difficult to modify for other interactions
  f <- update(formula, ~. + .post)
  LLs <- c()
  taus <- {{taus}}

  for(i in 1:length(taus)){
    usetau = taus[i]
    data2 <- mutate({{data}}, .post = ifelse({{t}} <= usetau, 0, {{t}} - usetau))

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
  data_win <- mutate({{data}}, .post = ifelse({{t}} <= tau_win, 0, {{t}} - tau_win))
  m_win <- lm(f, data = data_win)
  return(tibble(tau = tau_win, model = list(m_win)))
}
