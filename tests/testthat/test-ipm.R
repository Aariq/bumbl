test <- bipm()

test_that("changing paramaters does something", {
  expect_false(bipm(larv_surv = 0.97)$lambda == test$lambda)
  expect_false(bipm(dev_time_mean = 21)$lambda == test$lambda)
  expect_false(bipm(wkr_size_min = 2)$lambda == test$lambda)
  expect_false(bipm(wkr_size_max = 6)$lambda == test$lambda)
  expect_false(bipm(wkr_size_mean = 4)$lambda == test$lambda)
  expect_false(bipm(wkr_size_sd = 0.3)$lambda == test$lambda)
  expect_false(bipm(wkr_surv_f = function(wkr_size) plogis(5 - 0.3 * wkr_size))$lambda == test$lambda)
  expect_false(bipm(p_poln_ret_f = function(wkr_size) plogis(8 + (-5 * wkr_size) + (0.6 * wkr_size^2)))$lambda == test$lambda)
  expect_false(bipm(p_forage_f = function(wkr_size) plogis(-2 + 1.3 * wkr_size))$lambda == test$lambda)
  expect_false(bipm(trips_f = function(wkr_size) exp(-10 + (5 * wkr_size) + (-0.6 * wkr_size^2)))$lambda == test$lambda)
  expect_false(bipm(poln_mass_f = function(wkr_size) exp(-5.7 + 0.3 * wkr_size))$lambda == test$lambda)
  expect_false(bipm(poln_per_cell = 0.018)$lambda == test$lambda)
})

