testdf <- bombus[bombus$colony != 67, ]

test_that("bumbl works", {
  expect_s3_class(
    bumbl(testdf, colonyID = colony, t = week, formula = log(mass) ~ week),
    "data.frame"
  )
})

test_that("bumbl works with custom taus", {
  expect_s3_class(
    suppressWarnings(bumbl(testdf, colonyID = colony, taus = seq(3, 18, 0.5), t = week, formula = log(mass) ~ week)),
    "data.frame"
  )
})

test_that("bumbl drops colonies that produce errors", {
  expect_message({
    out <- bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(out), length(unique(bombus$colony)) - 1)
})

test_that("bumbl returns NAs for colonies that produce errors when augment = TRUE", {
  expect_message({
    out <- bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(bombus), nrow(out))
})

