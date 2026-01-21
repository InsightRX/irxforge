test_that("create_demographics_table produces expected results", {
  dm <- data.frame(
    STUDYID = "Study A",
    USUBJID = LETTERS[1:10],
    COUNTRY = "USA",
    SITEID = "Site A",
    ARM = rep(c("Placebo", "Drug X High Dose", "Drug X Low Dose"), length.out = 10),
    RACE = rep(c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "ASIAN"), length.out = 10),
    ETHNIC = rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), length.out = 10),
    SEX = rep(c("F", "M"), length.out = 10),
    AGE = c(40, 61, 65, 52, 70, 71, 67, 80, 50, 59)
  )
  vs <- data.frame(
    USUBJID = rep(LETTERS[1:10], 2),
    VSTEST = rep(c("Height", "Weight"), each = 10),
    VSSTRESC = c(
      "147.32",
      "162.56",
      "177.8",
      "175.26",
      "154.94",
      "148.59",
      "168.91",
      "158.24",
      "181.61",
      "180.34",
      "53.98",
      "54.43",
      "53.07",
      "53.98",
      "61.19",
      "60.07",
      "65.53",
      "68.00",
      "59.59",
      "59.19"
    )
  )
  dat <- list(dm = dm, vs = vs)
  res <- create_demographics_table(dat)

  expect_named(res, c("Demographic", "Statistic", "Value/count"))
  expect_equal(
    res[res$Demographic == "age" & res$Statistic == "mean", "Value/count", drop = TRUE],
    as.character(round(mean(dm$AGE), 1))
  )
  expect_equal(
    res[2, "Value/count", drop = TRUE], # SD
    as.character(round(sd(dm$AGE), 1))
  )
  expect_equal(
    res[3, "Value/count", drop = TRUE],
    as.character(round(median(dm$AGE), 1))
  )
  expect_equal(
    res[res$Demographic == "arm" & res$Statistic == "Drug X High Dose", "Value/count", drop = TRUE],
    as.character(3)
  )
  expect_error(create_demographics_table(dat, "foo"))
})

test_that("demographics arg can be uppercase or lower", {
  dm <- data.frame(
    STUDYID = "Study A",
    USUBJID = LETTERS[1:4],
    COUNTRY = "USA",
    SITEID = "Site A",
    ARM = rep(c("Placebo", "Drug X High Dose", "Drug X Low Dose"), length.out = 4)
  )
  vs <- data.frame(
    USUBJID = rep(LETTERS[1:4], 2),
    VSTEST = rep(c("Height", "Weight"), each = 4),
    VSSTRESC = c(
      "147.32",
      "162.56",
      "177.8",
      "175.26",
      "53.98",
      "54.43",
      "53.07",
      "53.98"
    )
  )
  dat <- list(dm = dm, vs = vs)
  res1 <- create_demographics_table(dat, demographics = c("WEIGHT", "HEIGHT", "ARM"))
  res2 <- create_demographics_table(dat, demographics = c("weight", "height", "arm"))
  expect_equal(res1, res2)
})

test_that("If subjects are missing from vs, stats of dm variables aren't affected", {
  # Previously there was an incorrect join that would filter out individuals
  # that weren't in both data frames

  dm <- data.frame(
    STUDYID = "Study A",
    USUBJID = LETTERS[1:10],
    COUNTRY = "USA",
    SITEID = "Site A",
    ARM = rep(c("Placebo", "Drug X High Dose", "Drug X Low Dose"), length.out = 10),
    RACE = rep(c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "ASIAN"), length.out = 10),
    ETHNIC = rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), length.out = 10),
    SEX = rep(c("F", "M"), length.out = 10),
    AGE = c(40, 61, 65, 52, 70, 71, 67, 80, 50, 59)
  )
  vs <- data.frame(
    USUBJID = LETTERS[1:5],
    VSTEST = rep("Height", 5),
    VSSTRESC = c(
      "147.32",
      "162.56",
      "177.8",
      "175.26",
      "154.94"
    )
  )
  dat <- list(dm = dm, vs = vs)
  res1 <- create_demographics_table(dat, "arm")
  res2 <- create_demographics_table(dat, c("arm", "height"))
  expect_equal(
    res1[res1$Statistic %in% c("Drug X High Dose", "Drug X Low Dose", "Placebo"), "Value/count", drop = TRUE],
    res2[res2$Statistic %in% c("Drug X High Dose", "Drug X Low Dose", "Placebo"), "Value/count", drop = TRUE]
  )

})

test_that("If subjects are missing from dm, stats of vs variables aren't affected", {
  dm <- data.frame(
    STUDYID = "Study A",
    USUBJID = LETTERS[1:5],
    COUNTRY = "USA",
    SITEID = "Site A",
    ARM = rep(c("Placebo", "Drug X High Dose", "Drug X Low Dose"), length.out = 5),
    RACE = rep(c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "ASIAN"), length.out = 5),
    ETHNIC = rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), length.out = 5),
    SEX = rep(c("F", "M"), length.out = 5),
    AGE = c(40, 61, 65, 52, 70)
  )
  vs <- data.frame(
    USUBJID = rep(LETTERS[1:10], 2),
    VSTEST = rep(c("Height", "Weight"), each = 10),
    VSSTRESC = c(
      "147.32",
      "162.56",
      "177.8",
      "175.26",
      "154.94",
      "148.59",
      "168.91",
      "158.24",
      "181.61",
      "180.34",
      "53.98",
      "54.43",
      "53.07",
      "53.98",
      "61.19",
      "60.07",
      "65.53",
      "68.00",
      "59.59",
      "59.19"
    )
  )
  dat <- list(dm = dm, vs = vs)
  res1 <- create_demographics_table(dat, "height")
  res2 <- create_demographics_table(dat, c("arm", "height"))

  expect_equal(head(res1$`Value/count`, 4), head(res2$`Value/count`, 4))
})

test_that("csv export works", {
  tmpfile <- withr::local_file(file.path(tempdir(), "demographics_table.csv"))

  dm <- data.frame(
    STUDYID = "Study A",
    USUBJID = LETTERS[1:10],
    COUNTRY = "USA",
    SITEID = "Site A",
    ARM = rep(c("Placebo", "Drug X High Dose", "Drug X Low Dose"), length.out = 10),
    RACE = rep(c("WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "BLACK OR AFRICAN AMERICAN", "ASIAN"), length.out = 10),
    ETHNIC = rep(c("HISPANIC OR LATINO", "NOT HISPANIC OR LATINO"), length.out = 10),
    SEX = rep(c("F", "M"), length.out = 10),
    AGE = c(40, 61, 65, 52, 70, 71, 67, 80, 50, 59)
  )
  tbl <- create_demographics_table(
    data = list(dm = dm),
    demographics = "age",
    path = tmpfile
  )
  res <- read.csv(tmpfile)
  expect_equal(nrow(res), 4)
})
