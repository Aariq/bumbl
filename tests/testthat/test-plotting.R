library(dplyr)
bombus_sub <-
  bombus %>%
  filter(colony %in% c(9, 14, 82, 83, 46, 92)) %>%
  group_by(colony) %>%
  mutate(count = as.integer(mass) - min(as.integer(mass))) %>%
  ungroup()

test_that("plotting function displays something?", {
  results <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = d.mass ~ week,
      augment = FALSE
    ))
  expect_invisible(bumbl_plot(results))
})

test_that("plotting function errors when not a bumbl object", {
  expect_error(
    bumbl_plot(mtcars),
    "bumbl_plot() only works on dataframes output by bumbl()",
    fixed = TRUE
  )
})

test_that("plotting works with count data", {
  results_count <-
    bumbl(lildf, colonyID = colony, t = week, formula = count ~ week,
          family = "poisson", augment = TRUE)
  expect_invisible(bumbl_plot(results_count))
})

test_that("plotting works with bumbl.nb", {
  results_overdisp <-
    bumbl.nb(lildf, colonyID = colony, t = week, formula = count ~ week, augment = TRUE)
  expect_invisible(bumbl_plot(results_overdisp))
})
