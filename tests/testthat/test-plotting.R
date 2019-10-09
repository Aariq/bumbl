library(dplyr)
colony_subset <- sample(bombus$colony, 10)
lildf <-
  bombus %>%
  filter(colony %in% colony_subset) %>%
  #make fake count data
  group_by(colony) %>%
  mutate(count = as.integer(d.mass)) %>%
  ungroup()

results <-
  bumbl(lildf, colonyID = colony, t = week, formula = d.mass ~ week,
        augment = TRUE)
results_count <-
  bumbl(lildf, colonyID = colony, t = week, formula = count ~ week,
        family = "poisson", augment = TRUE)
results_overdisp <-
  bumbl(lildf, colonyID = colony, t = week, formula = count ~ week,
        family = "negbin", augment = TRUE)

test_that("plotting function displays something?", {
  expect_invisible(bumbl_plot(results))
})

test_that("plotting function errors when not a bumbl(augment = TRUE) object", {
  expect_error(bumbl_plot(mtcars),
               "bumbl_plot() only works on dataframes output by bumbl() with augment = TRUE",
               fixed = TRUE)
})

test_that("plotting works with all families", {
  expect_invisible(bumbl_plot(results_count))
  expect_invisible(bumbl_plot(results_overdisp))
})
