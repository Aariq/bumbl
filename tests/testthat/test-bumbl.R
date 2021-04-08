library(bumbl)
bombus_sub <-
  bombus %>%
  dplyr::filter(colony %in% c("17", "104", "20", "24")) %>%
  dplyr::group_by(colony) %>%
  # dplyr::mutate(count = as.integer(mass) - min(as.integer(mass))) %>%
  dplyr::mutate(count = floor(mass)) %>%
  dplyr::ungroup()

err_df <-
  bombus_sub %>%
  mutate(week = if_else(colony == "17", week*100000000.0, as.double(week)))


test_that("bumbl errors if time variable is missing from formula", {
  expect_error(
    bumbl(bombus_sub, colonyID = colony, t = week, formula = d.mass ~ date),
    "'week' is missing from the model formula"
  )
})

test_that("bumble errors if colony ID not specified correctly", {
  expect_error(
    bumbl(bombus_sub, colonyID = clooney, t = week, formula = d.mass ~ date),
    "The name of the colony ID column must be supplied to 'colonyID'."
  )
  expect_error(
    bumbl(bombus_sub, t = week, formula = d.mass ~ date),
    "The name of the colony ID column must be supplied to 'colonyID'."
  )
})

test_that("bumbl works", {
  expect_s3_class(
    suppressWarnings(bumbl(bombus_sub, colonyID = colony, t = week, formula = d.mass ~ week)),
    "data.frame"
  )
  expect_s3_class(
    suppressWarnings(bumbl(bombus_sub, colonyID = colony, t = week, formula = d.mass ~ week, augment = TRUE)),
    "data.frame"
  )
})


test_that("bumbl drops colonies that produce errors", {
  expect_message({
    out <- bumbl(err_df, colonyID = colony, t = week, formula = d.mass ~ week)
  }, "Warning: Search for optimal switchpoint did not converge for colonyID '17'. Omitting from results.")
  expect_equal(nrow(out), length(unique(err_df$colony)) - 1)
})

test_that("bumbl returns NAs for colonies that produce errors when augment = TRUE", {
  expect_message({
    out <- bumbl(err_df, colonyID = colony, t = week, formula = d.mass ~ week,
                 augment = TRUE)
  })
  expect_equal(nrow(err_df), nrow(out))
})

test_that("bumbl works with co-variates", {
  out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week * cum_floral)
  expect_identical(colnames(out), c("colony", "converged", "tau", "logN0", "logLam", "decay", "logNmax", "beta_cum_floral", "beta_week:cum_floral"))
})

test_that("no unexpected warnings", {
  expect_silent(bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week))
  expect_silent(bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week, augment = TRUE))
})

test_that("bumbl works with poisson count data", {
  count.out <-
    bumbl(bombus_sub, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"))
  count.out.aug <-
    bumbl(bombus_sub, colonyID = colony, t = week, formula = count ~ week,
          family = poisson(link = "log"), augment = TRUE)
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})

test_that("bumbl works with overdispersed count data", {
  count.out <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = count ~ week,
      family = "negbin"
    ))
  count.out.aug <-
    suppressWarnings(bumbl(
      bombus_sub,
      colonyID = colony,
      t = week,
      formula = count ~ week,
      family = "negbin",
      augment = TRUE
    ))
  expect_s3_class(count.out, "data.frame")
  expect_s3_class(count.out.aug, c("data.frame", "bumbldf"))
})


test_that("error handling", {
  expect_error(bumbl(bombus_sub, colonyID = colony, t = week, formula = I(d.mass - 1) ~ week), "Model fitting failed for all colonies.")
})

test_that("results are correct", {
  # runif(1, 1, 1000)
  local_edition(2) #something not working right with 3e and expect_equal()
  x <- sim_colony(seed = 846)
  params <- attributes(x)
  testcol <- tibble(week = 1:20, mass = x, colony = "a")
  out <- suppressWarnings(bumbl(testcol, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  expect_equal(out$tau, params$tau, tolerance = 0.1)
  expect_equal(out$logN0, log(params$n0), tolerance = 0.1)
  expect_equal(out$logLam, log(params$lambda), tolerance = 0.05)
  expect_equal(out$decay, log(params$delta) - log(params$lambda), tolerance = 0.1)
})

test_that("results are robust", {
  # runif(1, 1, 1000)
  x <- sim_colony(seed = 846)
  testcol <-
    tibble(week = 1:20, mass = x, colony = "a") %>%
    mutate(mass2 = jitter(mass))
  out1 <- suppressWarnings(bumbl(testcol, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  out2 <- suppressWarnings(bumbl(testcol, colonyID = colony, t = week, mass2 ~ week, family = gaussian(link = "log")))
  local_edition(3)
  expect_equal(out1$tau, out2$tau, tolerance = 0.001)
  expect_equal(out1$logN0, out2$logN0, tolerance = 0.001)
  expect_equal(out1$logLam, out2$logLam, tolerance = 0.001)
  expect_equal(out1$decay, out2$decay, tolerance = 0.001)
})

test_that("results are not dependent on row order", {
  local_edition(2)
  x <- sim_colony(seed = 846)
  testcol <-
    tibble(week = 1:20, mass = x, colony = "a")
  testcol2 <-
    sample_n(testcol, nrow(testcol))
  out1 <- suppressWarnings(bumbl(testcol, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  out2 <- suppressWarnings(bumbl(testcol2, colonyID = colony, t = week, mass ~ week, family = gaussian(link = "log")))
  expect_equal(out1, out2, check.attributes = FALSE)
})

test_that("keep.model = TRUE works", {
  out <- bumbl(bombus_sub, colonyID = colony, t = week, mass ~ week, keep.model = TRUE)
  expect_type(out$model, "list")
})

test_that("Can't use both keep.model and augment", {
  expect_error(bumbl(bombus_sub, colonyID = colony, t = week, mass ~ week, augment = TRUE, keep.model = TRUE))
})

test_that("User can pass arguments to glm() with ...", {
  #I think the simplest test is with model = FALSE
  m2 <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week, keep.model = TRUE, model = FALSE)
  expect_false("model" %in% names(m2$model[[1]]))

  #Arguments where environment matters (e.g. unquoted variable names)
  #make fake offset column
  test_col <- bombus_sub %>% mutate(effort = runif(n(), 1, 2)) %>% filter(colony == first(colony))
  expect_s3_class(
    brkpt(test_col, t=week, formula = count ~ week, family = poisson(link = "log"), offset = log(effort)),
    "tbl_df"
  )
})

test_that("Column names aren't duplicated in output when augment = TRUE", {
  out <- bumbl(bombus_sub, colonyID = colony, t = week, formula = mass ~ week + cum_floral, augment = TRUE)
  expect_false("cum_floral.y" %in% colnames(out))
})
