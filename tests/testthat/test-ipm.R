default <- bipm()

test_that("changing paramaters does something", {
  expect_false(bipm(larv_surv = 0.97)$lambda == default$lambda)
  expect_false(bipm(dev_time_mean = 21)$lambda == default$lambda)
  expect_false(bipm(wkr_size_min = 2)$lambda == default$lambda)
  expect_false(bipm(wkr_size_max = 6)$lambda == default$lambda)
  expect_false(bipm(wkr_size_mean = 4)$lambda == default$lambda)
  expect_false(bipm(wkr_size_sd = 0.3)$lambda == default$lambda)
  expect_false(bipm(wkr_surv_f = function(wkr_size) plogis(5 - 0.3 * wkr_size))$lambda == default$lambda)
  expect_false(bipm(p_poln_ret_f = function(wkr_size) plogis(8 + (-5 * wkr_size) + (0.6 * wkr_size^2)))$lambda == default$lambda)
  expect_false(bipm(p_forage_f = function(wkr_size) plogis(-2 + 1.3 * wkr_size))$lambda == default$lambda)
  expect_false(bipm(trips_f = function(wkr_size) exp(-10 + (5 * wkr_size) + (-0.6 * wkr_size^2)))$lambda == default$lambda)
  expect_false(bipm(poln_mass_f = function(wkr_size) exp(-5.7 + 0.3 * wkr_size))$lambda == default$lambda)
  expect_false(bipm(poln_cost = 0.13)$lambda == default$lambda)
  expect_false(bipm(prop_foraging = 0.75)$lambda == default$lambda)
})

test_that("prop_foraging is working right", {
  expect_lt(bipm(prop_foraging = 0)$lambda, bipm(prop_foraging = 0.105)$lambda)
  expect_gt(bipm(prop_foraging = 1)$lambda, bipm(prop_foraging = 0.99)$lambda)
})

test_that("small worker size SDs work", {
  expect_type(bipm(wkr_size_sd = 0.0002), "list")
})
test_that("I get results close to Natalie's", {

  #task undefined field colonies
  expect_equivalent(
    bipm()$lambda, 1.026,
    tolerance = 0.0005
  )

  #task defined field colonies
  expect_equivalent(
    bipm(prop_foraging = 0.7405147)$lambda, 1.022,
    tolerance = 0.0005
  )

  #task undefined with first cohort
  expect_equivalent(
    bipm(wkr_size_mean = 4.02,
         wkr_size_sd = 0.3246114
         #this SD is from an old version.  Not sure if correct
    )$lambda, 1.032,
    tolerance = 0.0005
  )

  #task defined with first cohort
  expect_equivalent(
    bipm(wkr_size_mean = 4.02,
         wkr_size_sd = 0.3246114,
         prop_foraging = 0.7405147
    )$lambda, 1.023,
    tolerance = 0.0005
  )

  #polymorphic, task undefined highest growth rate
  expect_equivalent(
    bipm(wkr_size_mean = 4.02, wkr_size_sd = 0)$lambda, 1.031,
    tolerance = 0.0005
  )

  #polymorphic, task defined highest growth rate
  expect_equivalent(
    bipm(wkr_size_mean = 3.79,
         wkr_size_sd = 0.28,
         prop_foraging = 0.7405147
         )$lambda, 1.022,
    tolerance = 0.0005
  )
  # # monomorphic, no production cost(not sure how to do that)
  # expect_true(dplyr::near(bipm(wkr_size_mean = 4.27, wkr_size_sd = 0)$lambda,
  #                         ???,
  #                         tol = 0.0005))
  # # monomorphic with production cost--the same as polymorphic task undefined, right?
  # expect_true(dplyr::near(bipm(wkr_size_mean = 4.02, wkr_size_sd = 0)$lambda,
  #                         ???,
  #                         tol = 0.0005))
})
