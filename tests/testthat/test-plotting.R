library(dplyr)
bombus_sub <-
  bombus %>%
  filter(colony %in% c(9, 14, 82, 83, 46, 92)) %>%
  group_by(colony) %>%
  mutate(count = as.integer(mass) - min(as.integer(mass))) %>%
  ungroup()

detach("package:dplyr")

test_that("plot() works and returns data invisibly", {
  results <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = d.mass ~ week
    ))
  expect_invisible(plot(results))
})

test_that("autoplot() works and returns data invisibly", {
  results <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = d.mass ~ week
    ))
  expect_invisible(ggplot2::autoplot(results))
})

test_that("plotting works with augment = TRUE", {
  results <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = d.mass ~ week,
      augment = TRUE
    ))
  expect_invisible(plot(results))
  expect_invisible(ggplot2::autoplot(results))
})


test_that("plotting works with count data", {
  results_count <-
    bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = count ~ week,
      family = "poisson",
      augment = TRUE
    )
  expect_invisible(plot(results_count))
  expect_invisible(ggplot2::autoplot(results_count))
})


