test_good <- test_df %>% filter(colony == 1)
test_bad <- test_df %>% filter(colony == 7)


test_good_count <-
  test_good %>%
  #fake count data
  dplyr::mutate(count = (as.integer(mass) - min(as.integer(mass))) + 10)

test_that("brkpt works", {
  expect_s3_class(
    brkpt(test_good, t = week, formula = mass ~ week),
    "data.frame"
  )
})

test_that("brkpt works with more complicated formulas", {
  expect_s3_class(
    brkpt(test_good, t = week, formula = mass ~ week + floral_resources),
    "data.frame"
  )
})

test_that("brkpt errors when tau optimization does not converge", {
  expect_error({
    brkpt(test_bad, t = week, formula = mass ~ week, tau_optim_maxit = 1)
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


test_that("dots pass arguments to glm()", {
  #this isn't a great test.  Just checks that there's no error.  Can't tell if additional arg actually worked
  expect_s3_class(
    brkpt(test_good, t = week, formula = mass ~ week, model = FALSE),
    "data.frame"
  )
})

test_that("brkpt works with poisson dist", {
  skip_on_cran()
  count.model <- brkpt(test_good_count, t = week, formula = count ~ week, family = "poisson")
  expect_s3_class(count.model, "data.frame")
  expect_s3_class(count.model$model[[1]], "glm")
})

test_that("brkpt works with overdispersed data and family = 'negbin'", {
  negbin.model <- brkpt(test_good_count, t = week, formula = count ~ week, family = "negbin")
  expect_s3_class(negbin.model, "data.frame")
  expect_s3_class(negbin.model$model[[1]], "negbin")
})

