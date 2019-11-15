library(dplyr)
colony_subset <- sample(bombus$colony, 10)
lildf <-
  bombus %>%
  filter(colony %in% colony_subset) %>%
  #make fake count data
  group_by(colony) %>%
  mutate(count = as.integer(d.mass)) %>%
  ungroup()

test_that("plotting function displays something?", {
  results <-
    bumbl(lildf, colonyID = colony, t = week, formula = d.mass ~ week,
          augment = TRUE)
  expect_invisible(bumbl_plot(results))
})

test_that("plotting function errors when not a bumbl(augment = TRUE) object", {
  expect_error(bumbl_plot(mtcars),
               "bumbl_plot() only works on dataframes output by bumbl() with augment = TRUE",
               fixed = TRUE)
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
