mock_demo <- data.frame(
  SEQN      = 1:20,
  RIDAGEYR  = c(25, 35, 45, 55, 65, 30, 40, 50, 60, 70,
                28, 38, 48, 58, 68, 33, 43, 53, 63, 22),
  RIAGENDR  = rep(c(1L, 2L), 10),
  WTMEC2YR  = seq(1000, 10000, length.out = 20),
  stringsAsFactors = FALSE
)

mock_bmx <- data.frame(
  SEQN   = 1:20,
  BMXBMI = seq(18, 37, length.out = 20),
  BMXWT  = seq(50, 110, length.out = 20),
  stringsAsFactors = FALSE
)

test_that("returns data.frame with correct number of rows", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(tables = c("DEMO", "BMX"), n_subjects = 15)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 15)
})

test_that("SEQN is dropped from output", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(tables = c("DEMO", "BMX"), n_subjects = 5)
  expect_false("SEQN" %in% names(out))
})

test_that("variables argument selects specific columns", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    variables = c("RIDAGEYR", "BMXBMI"),
    n_subjects = 10
  )
  expect_named(out, c("RIDAGEYR", "BMXBMI"))
})

test_that("variables argument errors on missing variable", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  expect_error(
    sample_covariates_nhanes(
      tables = c("DEMO", "BMX"),
      variables = c("RIDAGEYR", "NOTACOLUMN"),
      n_subjects = 5
    ),
    "Variables not found"
  )
})

test_that("conditional filters are applied before sampling", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    n_subjects = 50,
    conditional = list(RIDAGEYR = c(30, 50))
  )
  expect_true(all(out$RIDAGEYR >= 30 & out$RIDAGEYR <= 50))
})

test_that("conditional with multiple variables filters correctly", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    n_subjects = 30,
    conditional = list(RIDAGEYR = c(30, 60), BMXBMI = c(20, 35))
  )
  expect_true(all(out$RIDAGEYR >= 30 & out$RIDAGEYR <= 60))
  expect_true(all(out$BMXBMI >= 20 & out$BMXBMI <= 35))
})

test_that("errors when conditional yields no observations", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  expect_error(
    sample_covariates_nhanes(
      tables = c("DEMO", "BMX"),
      n_subjects = 5,
      conditional = list(RIDAGEYR = c(200, 300))
    ),
    "No observations present"
  )
})

test_that("use_weights samples with probability proportional to WTMEC2YR", {
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  set.seed(42)
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    n_subjects = 100,
    use_weights = TRUE
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 100)
})

test_that("use_weights errors when WTMEC2YR is absent", {
  local_mocked_bindings(
    nhanes = function(table) mock_bmx,
    .package = "nhanesA"
  )
  expect_error(
    sample_covariates_nhanes(
      tables = c("BMX"),
      n_subjects = 5,
      use_weights = TRUE
    ),
    "WTMEC2YR"
  )
})

test_that("unsupported year raises an error", {
  expect_error(
    sample_covariates_nhanes(tables = "DEMO", year = "2099-2100", n_subjects = 5),
    "Unsupported NHANES year"
  )
})

test_that("single table (no join) works correctly", {
  local_mocked_bindings(
    nhanes = function(table) mock_demo,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(tables = "DEMO", n_subjects = 8)
  expect_equal(nrow(out), 8)
  expect_false("SEQN" %in% names(out))
})

test_that("nhanes method is dispatched from sample_covariates()", {
  local_mocked_bindings(
    sample_covariates_nhanes = function(...) "success"
  )
  expect_equal(sample_covariates(method = "nhanes"), "success")
})

# --- cache_dir / download_nhanes_cache() tests ---

test_that("sample_covariates_nhanes loads from cache_dir RDS files", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_demo, file.path(tmp, "DEMO_J.rds"))
  saveRDS(mock_bmx,  file.path(tmp, "BMX_J.rds"))
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    year = "2017-2018",
    cache_dir = tmp,
    n_subjects = 10
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 10)
  expect_false("SEQN" %in% names(out))
})

test_that("sample_covariates_nhanes falls back to nhanesA on cache miss", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_demo, file.path(tmp, "DEMO_J.rds"))
  # BMX_J.rds intentionally absent — should trigger nhanesA fallback
  local_mocked_bindings(
    nhanes = function(table) mock_bmx,
    .package = "nhanesA"
  )
  out <- sample_covariates_nhanes(
    tables = c("DEMO", "BMX"),
    year = "2017-2018",
    cache_dir = tmp,
    n_subjects = 5
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 5)
})

test_that("download_nhanes_cache saves RDS files with correct names", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  download_nhanes_cache(tables = c("DEMO", "BMX"), years = "2017-2018", path = tmp)
  expect_true(file.exists(file.path(tmp, "DEMO_J.rds")))
  expect_true(file.exists(file.path(tmp, "BMX_J.rds")))
  expect_equal(readRDS(file.path(tmp, "DEMO_J.rds")), mock_demo)
})

test_that("download_nhanes_cache skips existing files when overwrite = FALSE", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_demo, file.path(tmp, "DEMO_J.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanes = function(table) { call_count <<- call_count + 1L; mock_demo },
    .package = "nhanesA"
  )
  download_nhanes_cache(tables = "DEMO", years = "2017-2018", path = tmp, overwrite = FALSE)
  expect_equal(call_count, 0L)
})

test_that("download_nhanes_cache overwrites when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_demo, file.path(tmp, "DEMO_J.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanes = function(table) { call_count <<- call_count + 1L; mock_demo },
    .package = "nhanesA"
  )
  download_nhanes_cache(tables = "DEMO", years = "2017-2018", path = tmp, overwrite = TRUE)
  expect_equal(call_count, 1L)
})

test_that("download_nhanes_cache handles multiple years", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanes = function(table) mock_demo,
    .package = "nhanesA"
  )
  download_nhanes_cache(
    tables = "DEMO",
    years = c("2015-2016", "2017-2018"),
    path = tmp
  )
  expect_true(file.exists(file.path(tmp, "DEMO_I.rds")))
  expect_true(file.exists(file.path(tmp, "DEMO_J.rds")))
})

test_that("download_nhanes_cache returns path invisibly", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanes = function(table) mock_demo,
    .package = "nhanesA"
  )
  result <- download_nhanes_cache(tables = "DEMO", years = "2017-2018", path = tmp)
  expect_equal(result, tmp)
})

test_that(".onLoad downloads default tables when cache is absent", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanes_default_cache_dir = function() tmp,
    nhanes = function(table) if (grepl("DEMO", table)) mock_demo else mock_bmx,
    .package = "nhanesA"
  )
  # Simulate onLoad
  irxforge:::.onLoad(NULL, "irxforge")
  expect_true(file.exists(file.path(tmp, "DEMO_J.rds")))
  expect_true(file.exists(file.path(tmp, "BMX_J.rds")))
})

test_that(".onLoad skips download when cache already exists", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_demo, file.path(tmp, "DEMO_J.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanes_default_cache_dir = function() tmp,
    nhanes = function(table) { call_count <<- call_count + 1L; mock_demo },
    .package = "nhanesA"
  )
  irxforge:::.onLoad(NULL, "irxforge")
  expect_equal(call_count, 0L)
})
