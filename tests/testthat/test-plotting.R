colony_subset <- sample(bombus$colony, 10)
lildf <- bombus[bombus$colony %in% colony_subset, ]
results <- bumbl(lildf, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE)

test_that("plotting function displays something?", {
  expect_visible(bumbl_plot(results))
})

test_that("plotting function errors when not a bumbl(augment = TRUE) object", {
  expect_error(bumbl_plot(mtcars),
               "bumbl_plot() only works on dataframes output by bumbl() with augment = TRUE",
               fixed = TRUE)
})
