#' Plot observed and fitted results from bumbl()
#'
#' @param x A dataframe produced by `bumbl()` with `augment = TRUE`
#'
#' @return currently doesn't return an object, just prints plots
#'
#' @import rlang
#' @import dplyr
#' @importFrom purrr walk2
#'
#' @export
#'
#' @examples
#' colonyID_subset <- sample(bombus$colony, 10)
#' colony_subset <- bombus[bombus$colony %in% colonyID_subset, ]
#' results <- bumbl(colony_subset, colonyID = colony, t = week,
#'                  formula = log(mass) ~ week, augment = TRUE)
#' plot(results)
plot.bumbldf <- function(x) {
  if(!inherits(x, "bumbldf")) {
    abort("plot.bumbldf() only works on dataframes output by bumbl() with augment = TRUE")
  }
  colonyID <- attr(x, "colonyID")
  t <- attr(x, "t")
  formula <- attr(x, "formula")

  yvar <- all.vars(formula)[1]

  gdf <-
    x %>%
    group_by(!!sym(colonyID))  #might be able to replace with base::split()

  gdf %>%
    group_split() %>% #might be able to replace with base::split()
    purrr::walk2(.y = group_keys(gdf)[[1]],
                 ~{
                   ylims <- c(min(c(.x[[yvar]], exp(.x[[".fitted"]])), na.rm = TRUE),
                              max(c(.x[[yvar]], exp(.x[[".fitted"]])), na.rm = TRUE))
                   plot(.x[[t]], .x[[yvar]], main = .y, xlab = t, ylab = yvar, ylim = ylims)
                   points(.x[[t]], exp(.x[[".fitted"]]), type = "l", col = "red")

                 })

}
