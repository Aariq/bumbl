test_good <-
  test_df %>%
  dplyr::filter(colony %in% 1:5) %>%
  mutate(count = ceiling(mass))

test_that("bumbl errors if time variable is missing from formula", {
  expect_error(
    bumbl(test_good, colonyID = colony, t = week, formula = mass ~ date),
    "'week' is missing from the model formula"
  )
})

test_that("bumble errors if colony ID not specified correctly", {
  expect_error(
    bumbl(test_good, colonyID = clooney, t = week, formula = mass ~ date),
    "The name of the colony ID column must be supplied to 'colonyID'."
  )
  expect_error(
    bumbl(test_good, t = week, formula = mass ~ date),
    "The name of the colony ID column must be supplied to 'colonyID'."
  )
})

test_that("bumbl works", {
  expect_s3_class(
    suppressWarnings(bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week)),
    "data.frame"
  )
  expect_s3_class(
    suppressWarnings(bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week, augment = TRUE)),
    "data.frame"
  )
})

test_that("bumbl drops colonies that produce errors", {
  expect_message({
    out <- bumbl(test_df %>% filter(colony %in% 1:6), colonyID = colony, t = week, formula = mass ~ week)
  }, "Warning: cannot find valid starting values: please specify some for colonyID '6'. Omitting from results.")
  expect_equal(nrow(out), 5)
})

test_that("bumbl returns NAs for colonies that produce errors when augment = TRUE", {
  expect_message({
    out <- bumbl(test_df, colonyID = colony, t = week, formula = mass ~ week,
                 augment = TRUE)
  })
  expect_equal(nrow(test_df), nrow(out))
})

test_that("bumbl works with co-variates", {
  out <- bumbl(test_df, colonyID = colony, t = week, formula = mass ~ week * floral_resources)
  expect_identical(colnames(out), c("colony", "converged", "tau", "logN0", "logLam", "decay", "logNmax", "beta_floral_resources", "beta_week:floral_resources"))
})

test_that("no unexpected warnings", {
  expect_silent(bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week))
  expect_silent(bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week, augment = TRUE))
})

test_that("bumbl works with poisson count data", {
  count.out <-
    bumbl(test_good, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"))
  count.out.aug <-
    bumbl(test_good, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"), augment = TRUE)
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})

# Don't have great test data for this so doesn't pass consistently
# test_that("bumbl works with overdispersed count data", {
#   count.out <-
#     suppressWarnings(bumbl(
#       test_good,
#       colonyID = colony,
#       t = week,
#       formula = count ~ week,
#       family = "negbin"
#     ))
#   count.out.aug <-
#     suppressWarnings(bumbl(
#       test_good,
#       colonyID = colony,
#       t = week,
#       formula = count ~ week,
#       family = "negbin",
#       augment = TRUE
#     ))
#   expect_s3_class(count.out, "data.frame")
#   expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
# })


test_that("error handling", {
  expect_error(bumbl(test_df %>% filter(colony %in% 6:7), colonyID = colony, t = week, formula = mass ~ week, tau_optim_maxit = 1), "Model fitting failed for all colonies.")
})

test_that("results are correct", {
  out <- suppressWarnings(bumbl(test_df %>% filter(colony == 1),
                                colonyID = colony,
                                t = week,
                                mass ~ week,
                                family = gaussian(link = "log")))
  expect_equal(out$tau, 11, tolerance = 0.1)
  expect_equal(out$logN0, log(30), tolerance = 0.1)
  expect_equal(out$logLam, log(1.42), tolerance = 0.05)
  expect_equal(out$decay, log(0.3) - log(1.42), tolerance = 0.1)
})

test_that("results are robust", {
  testcol <-
    test_df %>%
    filter(colony == 2) %>%
    mutate(mass2 = jitter(mass))

  out1 <- suppressWarnings(bumbl(
    testcol,
    colonyID = colony,
    t = week,
    mass ~ week,
    family = gaussian(link = "log")
    ))
  out2 <- suppressWarnings(bumbl(
    testcol,
    colonyID = colony,
    t = week,
    mass2 ~ week,
    family = gaussian(link = "log")
  ))
  expect_equal(out1$tau, out2$tau, tolerance = 0.001)
  expect_equal(out1$logN0, out2$logN0, tolerance = 0.001)
  expect_equal(out1$logLam, out2$logLam, tolerance = 0.001)
  expect_equal(out1$decay, out2$decay, tolerance = 0.001)
})

test_that("results are not dependent on row order", {
  testcol2 <-
    sample_n(test_good, nrow(test_good))
  out1 <- suppressWarnings(bumbl(test_good, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  out2 <- suppressWarnings(bumbl(test_good, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  expect_equal(out1, out2, ignore_attr = TRUE)
})

test_that("keep.model = TRUE works", {
  out <- bumbl(test_good, colonyID = colony, t = week, mass ~ week, keep.model = TRUE)
  expect_type(out$model, "list")
})

test_that("Can't use both keep.model and augment", {
  expect_error(bumbl(test_good, colonyID = colony, t = week, mass ~ week, augment = TRUE, keep.model = TRUE))
})

test_that("User can pass arguments to glm() with ...", {
  #I think the simplest test is with model = FALSE
  m2 <- bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week, keep.model = TRUE, model = FALSE)
  expect_false("model" %in% names(m2$model[[1]]))

  #Arguments where environment matters (e.g. unquoted variable names)
  #make fake offset column
  test_col <- test_good %>% mutate(effort = runif(n(), 1, 2)) %>% filter(colony == first(colony))
  expect_s3_class(
    brkpt(test_col, t=week, formula = count ~ week, family = poisson(link = "log"), offset = log(effort)),
    "tbl_df"
  )
})

test_that("Column names aren't duplicated in output when augment = TRUE", {
  out <- bumbl(test_good, colonyID = colony, t = week, formula = mass ~ week + floral_resources, augment = TRUE)
  expect_false("floral_resources.y" %in% colnames(out))
})
