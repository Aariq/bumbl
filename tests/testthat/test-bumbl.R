library(dplyr)
test_that("bumbl works", {
  data <- colony_weights %>% dplyr::filter(!ColonyID %in% c("68", "97"))
  expect_s3_class(
    bumbl(data,
          colonyID = ColonyID,
          taus = seq(2,8,0.1),
          t = Round,
          formula = log(TrueColonyWt_g) ~ Round),
    "data.frame"
  )
})

test_that("bumbl returns DF same size as data", {
  data <- colony_weights %>% dplyr::filter(!ColonyID %in% c("68", "97"))
  out <- bumbl(data,
               colonyID = ColonyID,
               taus = seq(2,8,0.1),
               t = Round,
               formula = log(TrueColonyWt_g) ~ Round)
  expect_equal(
    nrow(out), nrow(data)
  )
})

test_that("error messages propgate correctly from brkpt to bumbl", {
  expect_error(bumbl(colony_weights, colonyID = ColonyID, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round),
  "For colony 68 more than one equivalent tau found"
  )
})
