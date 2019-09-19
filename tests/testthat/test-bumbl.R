testdf <- bombus[bombus$colony != 67, ]

test_that("bumbl works", {
  expect_s3_class(
    bumbl(testdf, colonyID = colony, t = week, formula = log(mass) ~ week),
    "data.frame"
  )
})

test_that("bumbl returns DF same size as data when augment = TRUE", {
  out <- bumbl(testdf, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE)
  outfull <- bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE)
  expect_equal(nrow(out), nrow(testdf))
  expect_equal(nrow(outfull), nrow(bombus))
})

test_that("bumbl returns NAs for colonies that produce errors", {
  out <- bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week)
  expect_equal(nrow(out), length(unique(bombus$colony)))
})

test_that("error messages propgate correctly from brkpt to bumbl", {
  expect_message(bumbl(bombus, colonyID = colony, t = week, formula = log(mass) ~ week),
  "For Colony ID '67': More than one equivalent tau found"
  )
})

test_that("bumbl works with custom taus", {
  expect_s3_class(
    suppressWarnings(bumbl(testdf, colonyID = colony, taus = seq(3, 18, 0.5), t = week, formula = log(mass) ~ week)),
    "data.frame"
  )
})

