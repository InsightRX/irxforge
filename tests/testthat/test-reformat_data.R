# reformat_data() -------------------------------------------------------------
test_that("auto-detects nca input, calls reformat_data_nca_to_modeling()", {
  dat <- data.frame(ID = 1, TIME = 0, CONC = 5)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_nca_to_modeling = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "auto", output_type = "modeling")
  expect_equal(out, mock_out)
})

test_that("auto-detects modeling input, calls reformat_data_modeling_to_nca()", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_modeling_to_nca = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "auto", output_type = "nca")
  expect_equal(out, mock_out)
})

test_that("auto-detects sdtm input, calls reformat_data_sdtm_to_modeling()", {
  dat <- list(DM = data.frame())
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_sdtm_to_modeling = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "auto", output_type = "modeling")
  expect_equal(out, mock_out)
})

test_that("nca -> modeling calls reformat_data_nca_to_modeling()", {
  dat <- data.frame(ID = 1, TIME = 0, CONC = 5)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_nca_to_modeling = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "nca", output_type = "modeling")
  expect_equal(out, mock_out)
})

test_that("modeling -> modeling calls reformat_data_modeling_to_modeling()", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_modeling_to_modeling = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "modeling", output_type = "modeling")
  expect_equal(out, mock_out)
})

test_that("modeling -> nca calls reformat_data_modeling_to_nca()", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_modeling_to_nca = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "modeling", output_type = "nca")
  expect_equal(out, mock_out)
})

test_that("sdtm -> modeling calls reformat_data_modeling_to_nca()", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1)
  mock_out <- data.frame(a = 1, b = 0, c = 1)
  local_mocked_bindings(
    reformat_data_sdtm_to_modeling = function(data, dictionary, ...) mock_out
  )
  out <- reformat_data(dat, input_type = "sdtm", output_type = "modeling")
  expect_equal(out, mock_out)
})

test_that("nca -> nca is not supported", {
  dat <- data.frame(ID = 1, TIME = 0, CONC = 5)
  expect_error(
    reformat_data(df, input_type = "nca", output_type = "nca"),
    "not supported"
  )
})

test_that("sdtm -> nca is not supported", {
  dat <- list(DM = data.frame())
  expect_error(
    reformat_data(sdtm, input_type = "sdtm", output_type = "nca"),
    "not supported"
  )
})

# detect_dataset_type() -------------------------------------------------------
test_that("detect_dataset_type() detects sdtm for lists", {
  dat <- list(DM = data.frame())
  expect_equal(detect_dataset_type(dat), "sdtm")
})

test_that("detect_dataset_type() detects modeling when evid column present", {
  dat <- data.frame(ID = 1, TIME = 0, EVID = 1)
  expect_equal(detect_dataset_type(dat), "modeling")
})

test_that("detect_dataset_type() detects nca when no evid column", {
  dat <- data.frame(ID = 1, TIME = 0, CONC = 5)
  expect_equal(detect_dataset_type(dat), "nca")
})

test_that("detect_dataset_type() errors on unsupported input type", {
  expect_error(detect_dataset_type(1), "Unsupported data type")
})

# not_supported_reformat() ----------------------------------------------------
test_that("not_supported_reformat() works", {
  expect_error(
    not_supported_reformat("nca", "nca"),
    "conversion from nca to nca is not supported"
  )
})

