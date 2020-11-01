library(dplyr)
bombus_sub <-
  bombus %>%
  filter(colony %in% c(67, 9, 14, 82, 83, 46, 92)) %>%
  group_by(colony) %>%
  mutate(count = as.integer(mass) - min(as.integer(mass))) %>%
  ungroup()

bombus_67 <- bombus %>% filter(colony == 67)

noerrs <- bombus_sub %>% filter(colony != 67)
detach("package:dplyr")
library(lubridate)
test_that("bumbl errors if time variable is missing from formula", {
  expect_error(
    bumbl(noerrs, colonyID = colony, t = week, formula = mass ~ date),
    "'week' is missing from the model formula"
  )
})

test_that("bumbl works", {
  expect_s3_class(
    bumbl(noerrs, colonyID = colony, t = week, formula = mass ~ week),
    "data.frame"
  )
  expect_s3_class(
    bumbl(noerrs, colonyID = colony, t = week, formula = mass ~ week, augment = TRUE),
    "data.frame"
  )
})

test_that("bumbl works with custom taus", {
  expect_s3_class(
    suppressWarnings(bumbl(noerrs, colonyID = colony, taus = seq(3, 18, 0.5), t = week,
                           formula = mass ~ week)),
    "data.frame"
  )
})

test_that("bumbl errors with custom taus outside or range of data", {
  expect_warning(bumbl(noerrs, colonyID = colony, taus = seq(3, 50, 1), t = week,
                           formula = mass ~ week))
})

test_that("bumbl drops colonies that produce errors", {
  expect_message({
    out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(out), length(unique(bombus_sub$colony)) - 1)
})

test_that("bumbl returns NAs for colonies that produce errors when augment = TRUE", {
  expect_message({
    out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week,
                 augment = TRUE)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(bombus_sub), nrow(out))
})

test_that("bumbl works with co-variates", {
  out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week * cum_floral)
  expect_identical(colnames(out), c("colony", "tau", "logN0", "logLam", "decay", "logNmax", "cum_floral", "week:cum_floral"))
})

test_that("no unexpected warnings", {
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = mass ~ week))
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = mass ~ week, augment = TRUE))
})

test_that("bumbl works with poisson count data", {
  count.out <-
    bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"))
  count.out.aug <-
    bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"), augment = TRUE)
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})

test_that("bumbl.nb works with overdispersed count data", {
  count.out <-
    suppressWarnings(bumbl.nb(
      noerrs,
      colonyID = colony,
      t = week,
      formula = d.mass ~ week
    ))
  count.out.aug <-
    suppressWarnings(bumbl.nb(
      noerrs,
      colonyID = colony,
      t = week,
      formula = count ~ week,
      augment = TRUE
    ))
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})

test_that("bumbl works when no Colony ID supplied", {
  expect_s3_class(
    bumbl(bombus_67, t = week, formula = d.mass ~ week),
    "data.frame"
  )
  expect_s3_class(
    bumbl(bombus_67, t = week, formula = d.mass ~ week, augment = TRUE),
    "data.frame"
  )
})

test_that("error handling", {
  expect_error(brkpt(bombus_67, t = week, formula = I(d.mass-1) ~ week), regexp = "No valid values for tau found.+")
  expect_error(bumbl(bombus_sub, t = week, formula = I(d.mass - 1) ~ week), "Model fitting failed for all colonies.")
})

test_that("results are correct", {
  # runif(1, 1, 1000)
  x <- sim_colony(seed = 852)
  params <- attributes(x)
  testcol <- tibble(week = 1:20, mass = x, colony = "a")
  out <- bumbl(testcol, t = week, mass ~ week, family = gaussian(link = "log"))
  expect_equal(out$tau, params$tau, tolerance = 0.1)
  expect_equal(out$logN0, log(params$n0), tolerance = 0.1)
  expect_equal(out$logLam, log(params$lambda), tolerance = 0.05)
  expect_equal(out$decay, log(params$delta) - log(params$lambda), tolerance = 0.1)
})

test_that("results are robust", {
  # runif(1, 1, 1000)
  x <- sim_colony(seed = 852)
  testcol <-
    tibble(week = 1:20, mass = x, colony = "a") %>%
    mutate(mass2 = jitter(mass))
  out1 <- bumbl(testcol, t = week, mass ~ week, family = gaussian(link = "log"))
  out2 <- bumbl(testcol, t = week, mass2 ~ week, family = gaussian(link = "log"))
  expect_equal(out1$tau, out2$tau, tolerance = 0.0001)
  expect_equal(out1$logN0, out2$logN0, tolerance = 0.0001)
  expect_equal(out1$logLam, out2$logLam, tolerance = 0.0001)
  expect_equal(out1$decay, out2$decay, tolerance = 0.0001)
})

test_that("results are not dependent on row order", {
  x <- sim_colony(seed = 852)
  testcol <-
    tibble(week = 1:20, mass = x, colony = "a")
  testcol2 <-
    sample_n(testcol, nrow(testcol))
  out1 <- bumbl(testcol, t = week, mass ~ week, family = gaussian(link = "log"))
  out2 <- bumbl(testcol2, t = week, mass ~ week, family = gaussian(link = "log"))
  expect_equivalent(out1, out2)
})
