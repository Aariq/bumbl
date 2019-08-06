testbees <- colony_weights[colony_weights$ColonyID=="18"]

test_that("brkpt errors if time variable is missing from formula", {
  expect_error(
    brkpt(data = testbees, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ 1), "'t=' should specify the time variable in the formula"
  )
})

test_that("brkpt works", {
  expect_is(
    brkpt(data = testbees, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round), "data.frame"
  )
})
