library(dplyr)
testbees <- bombus %>% filter(colony == 9)
noswitch <- bombus %>% filter(colony == 67)
testcount <-
  testbees %>%
  #fake count data
  mutate(count = as.integer(mass) - min(as.integer(mass)))

test_that("brkpt errors if time variable is missing from formula", {
  expect_error(
    brkpt(testbees, t = week, formula = mass ~ 1),
    "'week' is missing from the model formula"
  )
})

test_that("brkpt errors if taus don't match t", {
  expect_error(
    brkpt(testbees, taus = seq(16, 20, 0.1), t = week, formula = mass ~ week),
    "At least one tau must be in range of 'week'"
  )
})

test_that("brkpt uses only taus in range of t", {
  expect_warning(
    brkpt(testbees, taus = seq(2, 20, 0.1), t = week, formula = mass ~ week),
    "Some taus were not used because they were outside of range of 'week'"
  )
})

test_that("brkpt works", {
  expect_s3_class(
    brkpt(testbees, t = week, formula = mass ~ week),
    "data.frame"
  )
})

test_that("brkpt works with more complicated formulas", {
  expect_s3_class(
    brkpt(testbees, t = week, formula = mass ~ week + floral_resources),
    "data.frame"
  )
})

test_that("brkpt errors when multiple equivalent taus are found", {
  expect_error({
    brkpt(noswitch, t = week, formula = mass ~ week)},
    "More than one equivalent tau found"
  )
})

test_that("brkpt works with dates", {
  date.model <- brkpt(testbees, t = date, formula = mass ~ date)
  expect_s3_class(date.model, "data.frame")
  expect_is(date.model$tau, "Date")

  testbees2 <- testbees %>% mutate(date = as.POSIXct(date))
  date.model2 <- brkpt(testbees2, t = date, formula = mass ~ date)
  expect_is(date.model2$tau, "POSIXct")
})

test_that("brkpt works with poisson dist", {
  count.model <- brkpt(testcount, t = week, formula = count ~ week, family = "poisson")
  expect_s3_class(count.model, "data.frame")
  expect_is(count.model$model[[1]], "glm")
})

test_that("brkpt works with overdispersed data", {
  negbin.model <- brkpt(testcount, t = week, formula = count ~ week, family = "negbin")
  expect_s3_class(negbin.model, "data.frame")
  expect_is(negbin.model$model[[1]], "negbin")
})
