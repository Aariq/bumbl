
testbees <- bombus %>% dplyr::filter(colony == 9)

noswitch <-
  bombus %>%
  filter(colony == 9) %>%
  #do something to get reliable convergence error
  mutate(week = week * 1000000000)

testcount <-
  testbees %>%
  #fake count data
  dplyr::mutate(count = as.integer(mass) - min(as.integer(mass)))

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

test_that("brkpt errors when tau optimization does not converge", {
  expect_error({
    brkpt(noswitch, t = week, formula = d.mass ~ week)
    },
    "Search for optimal switchpoint did not converge"
  )
})

# test_that("brkpt works with dates", {
#   date.model <- brkpt(testbees, t = date, formula = mass ~ date)
#   expect_s3_class(date.model, "data.frame")
#   expect_is(date.model$tau, "Date")
#
#   testbees2 <- testbees %>% mutate(date = as.POSIXct(date))
#   date.model2 <- brkpt(testbees2, t = date, formula = mass ~ date)
#   expect_is(date.model2$tau, "POSIXct")
# })

test_that("brkpt works with poisson dist", {
  count.model <- brkpt(testcount, t = week, formula = count ~ week, family = "poisson")
  expect_s3_class(count.model, "data.frame")
  expect_s3_class(count.model$model[[1]], "glm")
})

test_that("brkpt works with overdispersed data and family = 'negbin'", {
  negbin.model <- brkpt(testcount, t = week, formula = count ~ week, family = "negbin")
  expect_s3_class(negbin.model, "data.frame")
  expect_s3_class(negbin.model$model[[1]], "negbin")
})

test_that("dots pass arguments to glm()", {
  #this isn't a great test.  Just checks that there's no error.  Can't tell if additional arg actually worked
  expect_s3_class(
    brkpt(testbees, t = week, formula = mass ~ week, model = FALSE),
    "data.frame"
  )
})

