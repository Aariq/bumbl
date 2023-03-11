#' Plot observed and fitted results from bumbl()
#'
#' Creates one plot per level of colonyID showing the observed (points) and fitted (red line) values from the model implemented by `bumbl()`.
#' @rdname plot.bumbldf
#' @param x a dataframe produced by [bumbl()].
#' @param ... other arguments not used by this method.
#' @param colony optional vector of colony ID's (character) or indexes (numeric) to plot.  If not supplied, all colonies will be plotted.
#' @method plot bumbldf
#' @return invisibly returns a list of dataframes used for building the plots.
#' @export
#' @seealso [bumbl()], [autoplot.bumbldf()]
#' @examples
#' set.seed(687)
#' colonyID_subset <- sample(bombus$colony, 10)
#' colony_subset <- bombus[bombus$colony %in% colonyID_subset, ]
#' results <- bumbl(colony_subset, colonyID = colony, t = week,
#'                  formula = mass ~ week)
#' plot(results)
plot.bumbldf <- function(x, ..., colony = NULL) {
  colonyID <- attr(x, "colonyID", exact = TRUE)
  t <- attr(x, "t", exact = TRUE)
  formula <- attr(x, "formula", exact = TRUE)
  predict <- attr(x, "predict", exact = TRUE)
  yvar <- all.vars(formula)[1]

  if (is.null(predict)) {
    x <- x
  } else {
    x <- predict
  }

  plot_data <- split(x, x$colony)

  if (!is.null(colony)) {
    plot_data <- plot_data[colony]
  }

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
#' @param object a dataframe produced by [bumbl()]
#' @param ... other arguments passed to `autoplot()`
#' @param colony a character vector of colony IDs to plot
#' @param x `r lifecycle::badge("deprecated")` `x` has been re-named to `object`
#'   for consistency with other `autoplot()` functions
#' @method autoplot bumbldf
#' @importFrom ggplot2 autoplot
#' @export
#' @return invisibly returns a ggplot object
#' @examples
#' bombus_subset <- bombus[bombus$colony %in% c("17", "104", "20", "24"), ]
#' results <- bumbl(bombus_subset, colonyID = colony, t = week,
#'                  formula = mass ~ week)
#' library(ggplot2)
#' autoplot(results)
autoplot.bumbldf <- function(object, ..., colony = NULL, x = deprecated()) {
  if(!requireNamespace("ggplot2", quietly = TRUE)) {
    abort("The ggplot2 package must be installed to use autoplot.bumbldf()")
  }
  if (lifecycle::is_present(x)) {
    lifecycle::deprecate_warn("1.0.3", "autoplot.bumbldf(x)", "autoplot.bumbldf(object)")
    object <- x
  }
  colonyID <- attr(object, "colonyID", exact = TRUE)
  t <- attr(object, "t", exact = TRUE)
  formula <- attr(object, "formula", exact = TRUE)
  predict <- attr(object, "predict", exact = TRUE)
  yvar <- all.vars(formula)[1]

  if (is.null(predict)) {
    df <- object
  } else {
    df <- predict
  }

  if (!is.null(colony)) {
    df <- df[df[[colonyID]] %in% colony, ]
  }

  p <-
    ggplot2::ggplot(df, ggplot2::aes_string(x = t)) +
    ggplot2::geom_point(ggplot2::aes_string(y = yvar)) +
    ggplot2::geom_line(ggplot2::aes(y = exp(.data$.fitted)), color = "red") +
    ggplot2::facet_wrap(colonyID)
  print(p)
  invisible(p)
}
