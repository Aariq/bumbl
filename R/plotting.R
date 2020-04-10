#' Plot observed and fitted results from bumbl()
#'
#' @param bumbldf A dataframe produced by `bumbl()`
#' @param gg use `ggplot2` for plotting
#'
#' @return if `gg = TRUE`, returns a \code{\link[ggplot2:ggplot]{ggplot}}
#'   object, otherwise returns (invisibly) a list of data frames used for
#'   creating the plots.
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
#'                  formula = log(mass) ~ week)
#' bumbl_plot(results)
bumbl_plot <- function(bumbldf, gg = FALSE) {
  if(!inherits(bumbldf, "bumbldf")) {
    abort("bumbl_plot() only works on dataframes output by bumbl()")
  }
  colonyID <- attr(bumbldf, "colonyID", exact = TRUE)
  t <- attr(bumbldf, "t", exact = TRUE)
  formula <- attr(bumbldf, "formula", exact = TRUE)
  predict <- attr(bumbldf, "predict", exact = TRUE)
  yvar <- all.vars(formula)[1]

  if(is.null(predict)) {
    x <- bumbldf
  } else {
    x <- predict
  }

  if(gg == TRUE){
    requireNamespace("ggplot2", quietly = TRUE)
    p <- ggplot2::ggplot(x, ggplot2::aes_string(x = t)) +
      ggplot2::geom_point(ggplot2::aes_string(y = yvar)) +
      ggplot2::geom_line(ggplot2::aes(y = exp(.data$.fitted)), color = "red") +
      ggplot2::facet_wrap(colonyID)
    print(p)
    invisible(p)
  } else {

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
}
