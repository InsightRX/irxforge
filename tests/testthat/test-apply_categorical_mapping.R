test_that("NULL categorical_mapping returns data unchanged with no attribute", {
  dat <- data.frame(ID = 1:3, SEX = c("M", "F", "M"))
  out <- apply_categorical_mapping(dat, NULL)
  expect_identical(out, dat)
  expect_null(attr(out, "categorical_mapping"))
})

test_that("character vector: most common class gets 0", {
  dat <- data.frame(
    ID = 1:5,
    SEX = c("M", "M", "M", "F", "F")
  )
  out <- apply_categorical_mapping(dat, c("SEX"))
  expect_equal(out$SEX, c(0, 0, 0, 1, 1))
  mapping <- attr(out, "categorical_mapping")
  expect_true(is.data.frame(mapping))
  expect_equal(names(mapping), c("column", "original_value", "encoded_value"))
  expect_equal(mapping$original_value[mapping$encoded_value == 0], "M")
  expect_equal(mapping$original_value[mapping$encoded_value == 1], "F")
})

test_that("character vector: NA values become -99", {
  dat <- data.frame(
    ID = 1:4,
    SEX = c("M", "F", NA, "M")
  )
  out <- apply_categorical_mapping(dat, c("SEX"))
  expect_equal(out$SEX, c(0, 1, -99, 0))
})

test_that("character vector: factor columns are handled", {
  dat <- data.frame(
    ID = 1:4,
    SEX = factor(c("M", "F", "M", "F"))
  )
  out <- apply_categorical_mapping(dat, c("SEX"))
  expect_true(is.numeric(out$SEX))
  # Both equally common, but order is deterministic (alphabetical tie-break from table())
  expect_true(all(out$SEX %in% c(0, 1)))
})

test_that("character vector: nonexistent column warns", {
  dat <- data.frame(ID = 1:3, SEX = c("M", "F", "M"))
  expect_warning(
    out <- apply_categorical_mapping(dat, c("SEX", "NONEXISTENT")),
    "NONEXISTENT"
  )
  # SEX should still be encoded
  expect_true(is.numeric(out$SEX))
})

test_that("character vector: multiple columns encoded independently", {
  dat <- data.frame(
    ID = 1:4,
    SEX = c("M", "M", "F", "F"),
    RACE = c("WHITE", "BLACK", "WHITE", "WHITE")
  )
  out <- apply_categorical_mapping(dat, c("SEX", "RACE"))
  mapping <- attr(out, "categorical_mapping")
  # RACE: WHITE is most common -> 0
  expect_equal(mapping$encoded_value[mapping$column == "RACE" & mapping$original_value == "WHITE"], 0)
  expect_equal(mapping$encoded_value[mapping$column == "RACE" & mapping$original_value == "BLACK"], 1)
})

test_that("data.frame input: user mapping applied correctly", {
  dat <- data.frame(
    ID = 1:4,
    SEX = c("male", "female", "male", "female")
  )
  user_map <- data.frame(
    column = c("SEX", "SEX"),
    original_value = c("male", "female"),
    encoded_value = c(0, 1)
  )
  out <- apply_categorical_mapping(dat, user_map)
  expect_equal(out$SEX, c(0, 1, 0, 1))
  mapping <- attr(out, "categorical_mapping")
  expect_equal(nrow(mapping), 2)
})

test_that("data.frame input: unmapped classes get continuation values", {
  dat <- data.frame(
    ID = 1:5,
    RACE = c("WHITE", "BLACK", "ASIAN", "ASIAN", "WHITE")
  )
  user_map <- data.frame(
    column = c("RACE", "RACE"),
    original_value = c("WHITE", "BLACK"),
    encoded_value = c(0, 1)
  )
  out <- apply_categorical_mapping(dat, user_map)
  expect_equal(out$RACE[dat$RACE == "WHITE"], c(0, 0))
  expect_equal(out$RACE[dat$RACE == "BLACK"], 1)
  expect_equal(out$RACE[dat$RACE == "ASIAN"], c(2, 2)) # continuation from max(1) + 1
  mapping <- attr(out, "categorical_mapping")
  expect_equal(nrow(mapping), 3)
  expect_equal(mapping$encoded_value[mapping$original_value == "ASIAN"], 2)
})

test_that("data.frame input: NA values become -99", {
  dat <- data.frame(
    ID = 1:3,
    SEX = c("M", NA, "F")
  )
  user_map <- data.frame(
    column = c("SEX", "SEX"),
    original_value = c("M", "F"),
    encoded_value = c(0, 1)
  )
  out <- apply_categorical_mapping(dat, user_map)
  expect_equal(out$SEX, c(0, -99, 1))
})

test_that("data.frame input: missing required columns errors", {
  dat <- data.frame(ID = 1:3, SEX = c("M", "F", "M"))
  bad_map <- data.frame(col = "SEX", val = "M", enc = 0)
  expect_error(
    apply_categorical_mapping(dat, bad_map),
    "column.*original_value.*encoded_value"
  )
})

test_that("data.frame input: uppercase column names accepted", {
  dat <- data.frame(
    ID = 1:3,
    SEX = c("M", "F", "M")
  )
  user_map <- data.frame(
    COLUMN = c("SEX", "SEX"),
    ORIGINAL_VALUE = c("M", "F"),
    ENCODED_VALUE = c(0, 1)
  )
  out <- apply_categorical_mapping(dat, user_map)
  expect_equal(out$SEX, c(0, 1, 0))
})

test_that("attribute has correct structure", {
  dat <- data.frame(
    ID = 1:3,
    SEX = c("M", "F", "M"),
    RACE = c("W", "W", "B")
  )
  out <- apply_categorical_mapping(dat, c("SEX", "RACE"))
  mapping <- attr(out, "categorical_mapping")
  expect_true(is.data.frame(mapping))
  expect_equal(names(mapping), c("column", "original_value", "encoded_value"))
  expect_true(all(c("SEX", "RACE") %in% mapping$column))
  expect_true(is.numeric(mapping$encoded_value))
})

test_that("invalid input type errors", {
  dat <- data.frame(ID = 1:3, SEX = c("M", "F", "M"))
  expect_error(
    apply_categorical_mapping(dat, 42),
    "character vector or a data.frame"
  )
})
