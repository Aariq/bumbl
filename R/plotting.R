#' Plot observed and fitted results from bumbl()
#'
#' Creates one plot per level of colonyID showing the observed (points) and fitted (red line) values from the model implemented by `bumbl()`.
#'
#' @param bumbldf a dataframe produced by [bumbl()].
#' @method plot bumbldf
#' @return invisibly returns a list of dataframes used for building the plots.
#' @export
#' @seealso [bumbl()] [autoplot.bumbldf()]
#' @examples
#' set.seed(687)
#' colonyID_subset <- sample(bombus$colony, 10)
#' colony_subset <- bombus[bombus$colony %in% colonyID_subset, ]
#' results <- bumbl(colony_subset, colonyID = colony, t = week,
#'                  formula = mass ~ week)
#' plot(results)
plot.bumbldf <- function(bumbldf) {
  colonyID <- attr(bumbldf, "colonyID", exact = TRUE)
  t <- attr(bumbldf, "t", exact = TRUE)
  formula <- attr(bumbldf, "formula", exact = TRUE)
  predict <- attr(bumbldf, "predict", exact = TRUE)
  yvar <- all.vars(formula)[1]

  if (is.null(predict)) {
    x <- bumbldf
  } else {
    x <- predict
  }

  plot_data <- split(x, x$colony)

  message(paste0("Creating plots for ", length(plot_data), " colonies..."))
  purrr::walk2(.x = plot_data, .y = names(plot_data), ~{
    ylims <- c(min(c(.x[[yvar]], exp(.x[[".fitted"]])), na.rm = TRUE),
               max(c(.x[[yvar]], exp(.x[[".fitted"]])), na.rm = TRUE))
    plot(.x[[t]], .x[[yvar]], main = .y, xlab = t, ylab = yvar, ylim = ylims)
    points(.x[[t]], exp(.x[[".fitted"]]), type = "l", col = "red")
  })
}


#' Plot observed and fitted results from bumbl()
#'
#' Plots observed (points) and fitted (red line) values from the model implemented by `bumbl()`, faceted by colony.
#'
#' @param bumbldf a dataframe produced by [bumbl()].
#' @method autoplot bumbldf
#'
#' @importFrom ggplot2 autoplot
#' @return invisibly returns a ggplot object
#' @export
#'
#' @examples
autoplot.bumbldf <- function(bumbldf) {
  colonyID <- attr(bumbldf, "colonyID", exact = TRUE)
  t <- attr(bumbldf, "t", exact = TRUE)
  formula <- attr(bumbldf, "formula", exact = TRUE)
  predict <- attr(bumbldf, "predict", exact = TRUE)
  yvar <- all.vars(formula)[1]

  if (is.null(predict)) {
    x <- bumbldf
  } else {
    x <- predict
  }

  plot_data <- split(x, x$colony)

  p <-
    ggplot2::ggplot(x, ggplot2::aes_string(x = t)) +
    ggplot2::geom_point(ggplot2::aes_string(y = yvar)) +
    ggplot2::geom_line(ggplot2::aes(y = exp(.data$.fitted)), color = "red") +
    ggplot2::facet_wrap(colonyID)
  print(p)
  invisible(p)
}
