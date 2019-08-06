test_that("bumbl works", {
  expect_s3_class(
    bumbl(colony_weights %>% filter(ColonyID != 68), colonyID = ColonyID, taus = seq(2,8,0.1), t = Round, formula = log(TrueColonyWt_g) ~ Round),
    "data.frame"
  )
})

test_that("bumbl returns DF same size as data", {
  data <- colony_weights %>% filter(ColonyID != 68)
  out <- bumbl(data,
               colonyID = ColonyID,
               taus = seq(2,8,0.1),
               t = Round,
               formula = log(TrueColonyWt_g) ~ Round)
  expect_equal(
    nrow(out), nrow(data)
  )
})
