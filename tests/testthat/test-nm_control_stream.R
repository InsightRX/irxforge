write_stream <- function(lines) {
  f <- withr::local_tempfile(fileext = ".mod", .local_envir = parent.frame())
  writeLines(lines, f)
  f
}

test_that("nm_input_columns() resolves labels, synonyms and drops by position", {
  f <- write_stream(c(
    "$PROBLEM test",
    "$INPUT ID TIME DV EVID AMT CMT=DROP RATE=INFRATE DROP=L2",
    "       SEX ; renamed from the data header",
    "$DATA data.csv IGNORE=@"
  ))
  inp <- nm_input_columns(f, c("ID", "TIME", "DV", "EVID", "AMT", "CMT", "RATE",
                               "L2", "SEKS"))
  expect_equal(inp$column, c("ID", "TIME", "DV", "EVID", "AMT", "CMT", "RATE", "L2", "SEKS"))
  expect_equal(inp$dropped, c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(inp$label[c(6, 7, 9)], c("CMT", "RATE", "SEX"))
  expect_equal(inp$alias[7], "INFRATE")
})

test_that("nm_input_columns() rejects more $INPUT items than data columns", {
  f <- write_stream(c("$INPUT ID TIME DV EXTRA", "$DATA d.csv"))
  expect_error(nm_input_columns(f, c("ID", "TIME", "DV")), "4 items")
})

test_that("nm_columns_read() sees renamed columns, filter columns and $TABLE", {
  f <- write_stream(c(
    "$PROBLEM WT is mentioned here but this is free text",
    "$INPUT C ID TIME DV AMT SEX CREAT HT UNUSED DROP",
    "$DATA data.csv IGNORE=@ IGNORE=(C.EQ.'C')",
    "$PK",
    "IF (SEX.EQ.0) K=1.23 ; HT in a comment does not count",
    "CL = THETA(1)*K/CREAT",
    "$TABLE ID TIME CL NOPRINT"
  ))
  data_names <- c("C", "ID", "TIME", "DV", "AMT", "SEKS", "CREA", "HT", "UNUSED", "X")
  expect_setequal(nm_columns_read(f, data_names),
                  c("C", "ID", "TIME", "DV", "AMT", "SEKS", "CREA"))
})

test_that("nm_columns_read() treats implicit data items as read unless dropped", {
  f <- write_stream(c("$INPUT ID TIME DV EVID AMT CMT=DROP L2 MDV", "$DATA d.csv",
                      "$PK", "CL=THETA(1)"))
  read <- nm_columns_read(f, c("ID", "TIME", "DV", "EVID", "AMT", "CMT", "L2", "MDV"))
  expect_false("CMT" %in% read)
  expect_true(all(c("ID", "TIME", "DV", "EVID", "AMT", "L2", "MDV") %in% read))
})

test_that("nm_columns_read() is case-insensitive and handles unparenthesised filters", {
  f <- write_stream(c("$INPUT C ID TIME DV FLAG", "$DATA d.csv IGNORE(flag.EQ.1)"))
  expect_true("FLAG" %in% nm_columns_read(f, c("C", "ID", "TIME", "DV", "FLAG")))
})

test_that("nm_rewrite_input() keeps each column's item, including =DROP", {
  f <- write_stream(c(
    "$PROBLEM warfarin",
    "$INPUT ID TIME DV EVID AMT CMT=DROP RATE MDV",
    "$DATA source.csv IGNORE=@",
    "$PK", "CL=THETA(1)"
  ))
  out <- nm_rewrite_input(
    f,
    data_names = c("ID", "TIME", "DV", "EVID", "AMT", "CMT", "RATE", "MDV"),
    new_data_names = c("ID", "TIME", "EVID", "AMT", "RATE", "DV", "CMT", "MDV", "NEW"),
    data_file = "data.csv"
  )
  expect_true("$INPUT ID TIME EVID AMT RATE DV CMT=DROP MDV NEW=DROP" %in% out)
  expect_true("$DATA data.csv IGNORE=@" %in% out)
  expect_true(all(c("$PK", "CL=THETA(1)") %in% out))
})

test_that("nm_rewrite_input() keeps synonyms and renamed labels", {
  f <- write_stream(c("$INPUT ID TIME RATE=INFRATE SEX DV", "$DATA d.csv",
                      "$PK", "IF (SEX.EQ.1) CL=THETA(1)"))
  out <- nm_rewrite_input(f, c("ID", "TIME", "RATE", "SEKS", "DV"),
                          c("ID", "TIME", "DV", "SEKS", "RATE"))
  expect_true("$INPUT ID TIME DV SEX RATE=INFRATE" %in% out)
})

test_that("nm_rewrite_input() errors when a read column is missing from the new data", {
  f <- write_stream(c(
    "$INPUT C ID TIME DV SEX",
    "$DATA data.csv IGNORE=@ IGNORE=(C.EQ.'C')",
    "$PK", "IF (SEX.EQ.0) CL=THETA(1)"
  ))
  expect_error(
    nm_rewrite_input(f, c("C", "ID", "TIME", "DV", "SEKS"), c("ID", "TIME", "DV")),
    "C, SEKS"
  )
})

test_that("nm_rewrite_input() writes the stream and wraps a long $INPUT", {
  f <- write_stream(c("$INPUT ID TIME DV", "$DATA d.csv"))
  new <- c("ID", "TIME", "DV", sprintf("COVARIATE_%02d", 1:10))
  out_file <- withr::local_tempfile(fileext = ".mod")
  nm_rewrite_input(f, c("ID", "TIME", "DV"), new, output_file = out_file)
  out <- readLines(out_file)
  expect_true(all(nchar(out) <= 78))
  inp <- nm_input_columns(out_file, new)
  expect_equal(inp$column, new)
  expect_equal(sum(inp$dropped), 10)
})

test_that("comment lines before $PROBLEM do not make the problem text count as code", {
  f <- write_stream(c(
    ";; run1: header comment",
    "$PROBLEM\tPEDS DATA, two compartments",
    "$INPUT ID TIME DV DATA",
    "$DATA d.csv"
  ))
  expect_false("DATA" %in% nm_columns_read(f, c("ID", "TIME", "DV", "DATA")))
})

test_that("nm_rewrite_input() follows renamed and re-cased columns", {
  f <- write_stream(c("$INPUT id time PKDV amt", "$DATA d.csv",
                      "$PK", "CL=THETA(1)"))
  out <- nm_rewrite_input(f, c("id", "time", "PKDV", "amt"), c("ID", "TIME", "AMT", "DV"),
                          rename = c(DV = "PKDV"))
  expect_true("$INPUT id time amt PKDV" %in% out)
  expect_error(nm_rewrite_input(f, c("id", "time", "PKDV", "amt"), c("ID", "TIME"),
                                rename = c(DV = "PKDV")),
               "missing from the new data: amt\\.")
  expect_error(nm_rewrite_input(f, c("id", "time", "PKDV", "amt"), c("ID"),
                                rename = c(DV = "NOPE")),
               "NOPE")
})

test_that("nm_rewrite_input() removes a $TABLE-only column with a warning", {
  f <- write_stream(c(
    "$INPUT ID TIME DV CRCL pct_BLQ",
    "$DATA d.csv",
    "$PK", "CL=THETA(1)",
    "$TABLE ID TIME CRCL CL",
    "       PCT_BLQ ETA(1) NOPRINT ; output only"
  ))
  expect_warning(
    out <- nm_rewrite_input(f, c("ID", "TIME", "DV", "CRCL", "pct_BLQ"), c("ID", "TIME", "DV")),
    "CRCL, pct_BLQ"
  )
  expect_true("$TABLE ID TIME CL" %in% out)
  expect_true("       ETA(1) NOPRINT ; output only" %in% out)
  expect_true("$INPUT ID TIME DV" %in% out)
})
