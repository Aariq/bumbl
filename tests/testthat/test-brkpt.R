testbees <- colony_weights %>% dplyr::filter(ColonyID == 18)

test_that("brkpt errors if time variable is missing from formula", {
  expect_error(
    brkpt(testbees, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ 1),
    "'Round' is missing from the model formula"
  )
})


test_that("brkpt errors if taus don't match t", {
  expect_error(
    brkpt(testbees, taus = seq(8.1, 10, 0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round),
    "At least one tau must be in range of 'Round'"
  )
})

test_that("brkpt uses only taus in range of t", {
  expect_warning(
    brkpt(testbees, taus = seq(2, 10, 0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round),
    "Some taus were not used because they were outside of range of 'Round'"
  )
})

test_that("brkpt works", {
  expect_s3_class(
    brkpt(testbees, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round),
    "data.frame"
  )
})

test_that("brkpt works with more complicated formulas", {
  expect_s3_class(
    brkpt(testbees, taus = seq(2, 8, 0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round + Condition),
    "data.frame"
  )
})

test_that("brkpt errors when multiple equivalent taus are found", {
  expect_error({
    testbees <- colony_weights %>% dplyr::filter(ColonyID == 68)
    brkpt(testbees, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round)},
    "More than one equivalent tau found"
  )
})
