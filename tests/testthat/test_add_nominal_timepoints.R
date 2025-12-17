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

