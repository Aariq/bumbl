library(dplyr)
bombus_sub <-
  bombus %>%
  filter(colony %in% c(67, 9, 14, 82, 83, 46, 92)) %>%
  group_by(colony) %>%
  mutate(count = as.integer(mass) - min(as.integer(mass))) %>%
  ungroup()

noerrs <- bombus_sub %>% filter(colony != 67)

test_that("bumbl works", {
  expect_s3_class(
    bumbl(noerrs, colonyID = colony, t = week, formula = log(mass) ~ week),
    "data.frame"
  )
})

test_that("bumbl works with custom taus", {
  expect_s3_class(
    suppressWarnings(bumbl(noerrs, colonyID = colony, taus = seq(3, 18, 0.5), t = week, formula = log(mass) ~ week)),
    "data.frame"
  )
})

test_that("bumbl drops colonies that produce errors", {
  expect_message({
    out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = log(mass) ~ week)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(out), length(unique(bombus_sub$colony)) - 1)
})

test_that("bumbl returns NAs for colonies that produce errors when augment = TRUE", {
  expect_message({
    out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE)
  }, "Warning: More than one equivalent tau found for colonyID '67'. Omitting from results.")
  expect_equal(nrow(bombus_sub), nrow(out))
})

test_that("bumbl works with co-variates", {
  out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = log(mass) ~ week * cum_floral)
  expect_identical(colnames(out), c("colony", "tau", "logN0", "logLam", "decay", "logNmax", "cum_floral", "week:cum_floral"))
})

test_that("no unexpected warnings", {
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = log(mass) ~ week))
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = log(mass) ~ week, augment = TRUE))
})

test_that("bumbl works with poisson count data", {
  count.out <-
    bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
          family = "poisson")
  count.out.aug <-
    bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
          family = "poisson", augment = TRUE)
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})

# test_that("bumbl works with overdispersed count data", {
#   count.out <-
#     bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
#           family = "overdispersed")
#   count.out.aug <-
#     bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
#           family = "overdispersed", augment = TRUE)
#   expect_s3_class(count.out, "data.frame")
#   expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
# })

# test_that("family = overdispersed doesn't produce a shit ton of warnings",{
#   expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
#                       family = "overdispersed"))
#   expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
#                       family = "overdispersed", augment = TRUE))
# })

test_that("family = poisson doesn't produce a shit ton of warnings",{
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
                      family = "poisson"))
  expect_silent(bumbl(noerrs, colonyID = colony, t = week, formula = count ~ week,
                      family = "poisson", augment = TRUE))
})
