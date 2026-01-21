test_that("Adding nominal timepoints works", {
  expect_message(
    dat <- add_nominal_timepoints(
      nm_data_timepoints,
      "TIME",
      "NOM_TIME",
      adjust = 0.7,
      verbose = TRUE
    )
  )
  expect_equal(
    dat$NOM_TIME[1:20],
    c(0, 0, 0, 0, 21, 21, 21, 21, 21, 21, 42, 42, 42, 42, 42, 42, 63, 63, 0, 0)
  )
})

test_that("adds a nominal time column", {
  dat <- data.frame(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  out <- add_nominal_timepoints(dat)
  expect_true("NOMINAL_TIME" %in% names(out))
})

test_that("custom time_var and nominal_time_var are respected", {
  dat <- data.frame(t = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  out <- add_nominal_timepoints(dat, time_var = "t", nominal_time_var = "NT")
  expect_true("NT" %in% names(out))
  expect_false("NOMINAL_TIME" %in% names(out))
})

test_that("number of rows is preserved", {
  dat <- data.frame(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  out <- add_nominal_timepoints(dat)
  expect_equal(nrow(out), nrow(dat))
})

test_that("number of rows is preserved with repeated times", {
  dat <- data.frame(time = c(0, 0, 1, 1, 2, 2))
  out <- add_nominal_timepoints(dat)
  expect_equal(nrow(out), nrow(dat))
})

test_that("input class is preserved", {
  dat <- data.frame(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  tbl <- tibble::tibble(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  out_dat <- add_nominal_timepoints(dat)
  out_tbl <- add_nominal_timepoints(tbl)
  expect_s3_class(out_dat, "data.frame")
  expect_s3_class(out_tbl, "tbl_df")
})

test_that("emits a message when verbose = TRUE", {
  dat <- data.frame(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  expect_message(
    add_nominal_timepoints(dat, verbose = TRUE),
    regexp = "Nominal times identified"
  )
})

test_that("additional arguments are passed to get_nominal_timepoints()", {
  dat <- data.frame(time = c(0.1, 0.2, 1.0, 1.1, 2.0, 2.2))
  expect_no_error(add_nominal_timepoints(dat, kernel = "epanechnikov"))
  expect_error(
    expect_warning(add_nominal_timepoints(dat, kornel = "epanechnikov")),
    class = "rlib_error_dots_unused"
  )
})

test_that("nominal times are matched to the closest timepoint", {
  data <- data.frame(time = c(0.1, 0.4, 0.6, 1.4, 1.6, 2.2))
  t_nom <- c(0, 1, 2) # Return known nominal timepoints
  out <- with_mocked_bindings(
    get_nominal_timepoints = function(t, ...) t_nom,
    add_nominal_timepoints(data)
  )
  expect_equal(out$NOMINAL_TIME, c(0, 0, 1, 1, 2, 2))
})
