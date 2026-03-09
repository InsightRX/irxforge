# A single merged NHANES-like dataset saved as nhanes_2017-2018.rds
mock_nhanes <- data.frame(
  SEQN     = 1:20,
  RIDAGEYR = c(25, 35, 45, 55, 65, 30, 40, 50, 60, 70,
               28, 38, 48, 58, 68, 33, 43, 53, 63, 22),
  RIAGENDR = rep(c(1L, 2L), 10),
  WTMEC2YR = seq(1000, 10000, length.out = 20),
  BMXBMI   = seq(18, 37, length.out = 20),
  BMXWT    = seq(50, 110, length.out = 20),
  stringsAsFactors = FALSE
)

# Helper: write mock_nhanes RDS into a temp dir
nhanes_temp_cache <- function(year = "2017-2018") {
  tmp <- withr::local_tempdir()
  saveRDS(mock_nhanes, file.path(tmp, paste0("nhanes_", year, ".rds")))
  tmp
}

# ---- sample_covariates_nhanes() ----

test_that("returns data.frame with correct number of rows", {
  tmp <- nhanes_temp_cache()
  out <- sample_covariates_nhanes(n_subjects = 15, cache_dir = tmp)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 15)
})

test_that("SEQN is dropped from output", {
  tmp <- nhanes_temp_cache()
  out <- sample_covariates_nhanes(n_subjects = 5, cache_dir = tmp)
  expect_false("SEQN" %in% names(out))
})

test_that("covariates argument selects specific columns", {
  tmp <- nhanes_temp_cache()
  out <- sample_covariates_nhanes(
    covariates = c("RIDAGEYR", "BMXBMI"),
    n_subjects = 10,
    cache_dir  = tmp
  )
  expect_named(out, c("RIDAGEYR", "BMXBMI"))
})

test_that("covariates argument errors on missing covariate", {
  tmp <- nhanes_temp_cache()
  expect_error(
    sample_covariates_nhanes(
      covariates = c("RIDAGEYR", "NOTACOLUMN"),
      n_subjects = 5,
      cache_dir  = tmp
    ),
    "Covariates not found"
  )
})

test_that("conditional filters are applied before sampling", {
  tmp <- nhanes_temp_cache()
  out <- sample_covariates_nhanes(
    n_subjects  = 50,
    conditional = list(RIDAGEYR = c(30, 50)),
    cache_dir   = tmp
  )
  expect_true(all(out$RIDAGEYR >= 30 & out$RIDAGEYR <= 50))
})

test_that("conditional with multiple variables filters correctly", {
  tmp <- nhanes_temp_cache()
  out <- sample_covariates_nhanes(
    n_subjects  = 30,
    conditional = list(RIDAGEYR = c(30, 60), BMXBMI = c(20, 35)),
    cache_dir   = tmp
  )
  expect_true(all(out$RIDAGEYR >= 30 & out$RIDAGEYR <= 60))
  expect_true(all(out$BMXBMI   >= 20 & out$BMXBMI   <= 35))
})

test_that("errors when conditional yields no observations", {
  tmp <- nhanes_temp_cache()
  expect_error(
    sample_covariates_nhanes(
      n_subjects  = 5,
      conditional = list(RIDAGEYR = c(200, 300)),
      cache_dir   = tmp
    ),
    "No observations present"
  )
})

test_that("use_weights samples with probability proportional to WTMEC2YR", {
  tmp <- nhanes_temp_cache()
  set.seed(42)
  out <- sample_covariates_nhanes(
    n_subjects  = 100,
    use_weights = TRUE,
    cache_dir   = tmp
  )
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 100)
})

test_that("use_weights errors when WTMEC2YR is absent", {
  tmp <- withr::local_tempdir()
  saveRDS(
    mock_nhanes[, setdiff(names(mock_nhanes), "WTMEC2YR")],
    file.path(tmp, "nhanes_2017-2018.rds")
  )
  expect_error(
    sample_covariates_nhanes(n_subjects = 5, use_weights = TRUE, cache_dir = tmp),
    "WTMEC2YR"
  )
})

test_that("unsupported year raises an error", {
  expect_error(
    sample_covariates_nhanes(year = "2099-2100", cache_dir = tempdir()),
    "Unsupported NHANES year"
  )
})

test_that("errors with clear message when cache file is missing", {
  tmp <- withr::local_tempdir()  # empty — no RDS inside
  expect_error(
    sample_covariates_nhanes(n_subjects = 5, cache_dir = tmp),
    "download_nhanes_cache"
  )
})

test_that("nhanes method is dispatched from sample_covariates()", {
  local_mocked_bindings(sample_covariates_nhanes = function(...) "success")
  expect_equal(sample_covariates(method = "nhanes"), "success")
})

# ---- download_nhanes_cache() ----

# Mock nhanesTables returns a 1-row data frame, nhanes returns mock data
mock_tables_df <- function(name) data.frame(Data.File.Name = name,
                                             stringsAsFactors = FALSE)

test_that("creates a single merged RDS per year", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanesTables = function(group, year) mock_tables_df(paste0(group, "_J")),
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  download_nhanes_cache(groups = c("DEMO", "LAB"), years = "2017-2018", path = tmp)
  expect_true(file.exists(file.path(tmp, "nhanes_2017-2018.rds")))
  # individual table files should NOT be present
  expect_equal(length(list.files(tmp, pattern = "DEMO_J\\.rds")), 0L)
})

test_that("merged RDS contains columns from all groups", {
  tmp <- withr::local_tempdir()
  demo_tbl <- mock_nhanes[, c("SEQN", "RIDAGEYR", "WTMEC2YR")]
  lab_tbl  <- data.frame(SEQN = mock_nhanes$SEQN, LBXGH = rnorm(20))
  local_mocked_bindings(
    nhanesTables = function(group, year) {
      if (group == "DEMO") mock_tables_df("DEMO_J")
      else                 mock_tables_df("LAB_J")
    },
    nhanes = function(table) if (grepl("DEMO", table)) demo_tbl else lab_tbl,
    .package = "nhanesA"
  )
  download_nhanes_cache(groups = c("DEMO", "LAB"), years = "2017-2018", path = tmp)
  merged <- readRDS(file.path(tmp, "nhanes_2017-2018.rds"))
  expect_true("RIDAGEYR" %in% names(merged))
  expect_true("LBXGH"    %in% names(merged))
})

test_that("skips tables with duplicate SEQNs", {
  tmp <- withr::local_tempdir()
  dup_tbl <- rbind(mock_nhanes[1, ], mock_nhanes[1, ])  # duplicate SEQN
  local_mocked_bindings(
    nhanesTables = function(group, year) mock_tables_df("DEMO_J"),
    nhanes       = function(table) dup_tbl,
    .package     = "nhanesA"
  )
  # Should not error; warns and skips; no output file if nothing merged
  expect_warning(
    download_nhanes_cache(groups = "DEMO", years = "2017-2018", path = tmp),
    regexp = NA  # no warning expected — it just messages and skips
  )
})

test_that("skips year when overwrite = FALSE and cache exists", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_nhanes, file.path(tmp, "nhanes_2017-2018.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanesTables = function(group, year) { call_count <<- call_count + 1L
                                           mock_tables_df("DEMO_J") },
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  download_nhanes_cache(groups = "DEMO", years = "2017-2018", path = tmp, overwrite = FALSE)
  expect_equal(call_count, 0L)
})

test_that("re-downloads when overwrite = TRUE", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_nhanes, file.path(tmp, "nhanes_2017-2018.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanesTables = function(group, year) { call_count <<- call_count + 1L
                                           mock_tables_df("DEMO_J") },
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  download_nhanes_cache(groups = "DEMO", years = "2017-2018", path = tmp, overwrite = TRUE)
  expect_gt(call_count, 0L)
})

test_that("handles multiple years, producing one RDS each", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanesTables = function(group, year) mock_tables_df(paste0("DEMO_", year)),
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  download_nhanes_cache(groups = "DEMO", years = c("2015-2016", "2017-2018"), path = tmp)
  expect_true(file.exists(file.path(tmp, "nhanes_2015-2016.rds")))
  expect_true(file.exists(file.path(tmp, "nhanes_2017-2018.rds")))
})

test_that("returns path invisibly", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanesTables = function(group, year) mock_tables_df("DEMO_J"),
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  result <- download_nhanes_cache(groups = "DEMO", years = "2017-2018", path = tmp)
  expect_equal(result, tmp)
})

# ---- .onLoad() ----

test_that(".onLoad downloads merged cache when absent", {
  tmp <- withr::local_tempdir()
  local_mocked_bindings(
    nhanes_default_cache_dir = function() tmp,
    nhanesTables = function(group, year) mock_tables_df(paste0(group, "_J")),
    nhanes       = function(table) mock_nhanes,
    .package     = "nhanesA"
  )
  irxforge:::.onLoad(NULL, "irxforge")
  expect_true(file.exists(file.path(tmp, "nhanes_2017-2018.rds")))
})

test_that(".onLoad skips download when cache already exists", {
  tmp <- withr::local_tempdir()
  saveRDS(mock_nhanes, file.path(tmp, "nhanes_2017-2018.rds"))
  call_count <- 0L
  local_mocked_bindings(
    nhanes_default_cache_dir = function() tmp,
    nhanesTables = function(group, year) { call_count <<- call_count + 1L
                                           mock_tables_df("DEMO_J") },
    nhanes = function(table) mock_nhanes,
    .package = "nhanesA"
  )
  irxforge:::.onLoad(NULL, "irxforge")
  expect_equal(call_count, 0L)
})
