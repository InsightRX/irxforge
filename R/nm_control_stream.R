#' Map a NONMEM `$INPUT` record onto the columns of its dataset
#'
#' NONMEM binds `$INPUT` items to data columns by position, not by name, so the
#' label a control stream uses for a column can differ from the column's header
#' (e.g. `SEX` for a column headed `SEKS`). Each item is a plain label (`WT`), a
#' label with a synonym (`RATE=INFRATE`), or a dropped column (`DROP`, `SKIP`,
#' `CMT=DROP`, `DROP=L2`).
#'
#' @param model_file path to a NONMEM control stream.
#' @param data_names character vector of the dataset's column names, in file
#'   order. If `NULL` (default), the `column` field of the result is `NA`.
#'
#' @returns A data.frame with one row per `$INPUT` item: `position`, `column`
#'   (the data column at that position), `label` and `alias` (the names the
#'   control stream can use for it; `alias` is `NA` without a synonym), `token`
#'   (the item as written) and `dropped`.
#'
#' @export
nm_input_columns <- function(model_file, data_names = NULL) {
  records <- nm_records(readLines(model_file, warn = FALSE))
  input <- records[nm_record_names(records) == "INPUT"]
  if (length(input) != 1) {
    stop("Expected exactly one $INPUT record in ", model_file, ", found ",
         length(input), ".", call. = FALSE)
  }
  body <- sub("^\\s*\\$INPUT\\b", "", paste(nm_strip_comments(input[[1]]), collapse = " "),
              ignore.case = TRUE)
  tokens <- strsplit(trimws(body), "[[:space:],]+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (!is.null(data_names) && length(tokens) > length(data_names)) {
    stop("$INPUT lists ", length(tokens), " items but the data has ",
         length(data_names), " columns.", call. = FALSE)
  }
  parts <- strsplit(tokens, "=", fixed = TRUE)
  is_drop <- function(x) toupper(x) %in% c("DROP", "SKIP")
  rows <- lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    dropped <- any(is_drop(p))
    named <- p[!is_drop(p)]
    data.frame(
      position = i,
      column = if (is.null(data_names)) NA_character_ else data_names[i],
      label = if (length(named) >= 1) named[1] else NA_character_,
      alias = if (length(named) >= 2) named[2] else NA_character_,
      token = tokens[i],
      dropped = dropped,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Find the data columns a NONMEM control stream reads
#'
#' A column counts as read when it is not dropped in `$INPUT` and either it is a
#' data item NONMEM consumes implicitly (`ID`, `TIME`, `DV`, `AMT`, `CMT`, `L2`,
#' ...), or one of its `$INPUT` names appears anywhere in the stream outside
#' `$INPUT` and `$PROBLEM`: model code, a `$DATA` `IGNORE`/`ACCEPT` filter, or a
#' `$TABLE` list. Matching is case-insensitive and ignores comments.
#'
#' Use it to decide which columns a de-identified dataset must keep for the
#' stream to run. It resolves names through `$INPUT`, so a column the stream
#' renames (`SEX` for `SEKS`) or reads only in a filter (`IGNORE=(C.EQ.'C')`)
#' is not mistaken for an unread one.
#'
#' @inheritParams nm_input_columns
#' @param data_names character vector of the dataset's column names, in file
#'   order.
#'
#' @returns Character vector of the names in `data_names` that the stream reads.
#'
#' @export
nm_columns_read <- function(model_file, data_names) {
  nm_columns_read_in(model_file, data_names)
}

# `exclude` names records whose references do not count (e.g. "TABLE").
nm_columns_read_in <- function(model_file, data_names, exclude = character()) {
  inp <- nm_input_columns(model_file, data_names)
  inp <- inp[!inp$dropped, , drop = FALSE]
  refs <- nm_referenced_names(readLines(model_file, warn = FALSE), exclude)
  implicit <- c("ID", "L1", "L2", "DV", "MDV", "TIME", "DATE", "DAT1", "DAT2",
                "DAT3", "EVID", "AMT", "RATE", "SS", "II", "ADDL", "CMT",
                "PCMT", "CALL", "CONT", "MRG_", "RAW_", "RPT_",
                paste0("XVID", 1:5))
  names_of <- function(i) toupper(stats::na.omit(c(inp$label[i], inp$alias[i])))
  read <- vapply(seq_len(nrow(inp)), function(i) {
    any(names_of(i) %in% c(implicit, refs))
  }, logical(1))
  inp$column[read]
}

#' Rewrite a NONMEM `$INPUT` record for a reordered or reduced dataset
#'
#' Rebuilds `$INPUT` so that each column of the new dataset keeps the `$INPUT`
#' item it had in the source stream, with synonyms and `=DROP` intact (a
#' `CMT=DROP` stays dropped). A column of the new dataset that the source
#' `$INPUT` did not list is written as `<name>=DROP`. A column the stream reads
#' (see [nm_columns_read()]) that is missing from the new dataset is an error,
#' because the rewritten stream would fail in NM-TRAN or, worse, silently
#' change meaning. The exception is a column listed only in `$TABLE`: it is
#' output, so it is removed from `$TABLE` with a warning.
#'
#' @inheritParams nm_columns_read
#' @param data_names character vector of the source dataset's column names, in
#'   file order: the dataset the source `$INPUT` was written for.
#' @param new_data_names character vector of the new dataset's column names, in
#'   file order. Columns are matched to the source by name, ignoring case.
#' @param rename optional named character vector for columns renamed between
#'   the two datasets, as `c(<new name> = "<source name>")` (e.g.
#'   `c(DV = "PKDV")`), the same direction as `anonymize_dataset()`'s
#'   `dictionary`.
#' @param output_file optional path to write the rewritten stream to.
#' @param data_file optional new file name for `$DATA`. Its filter options
#'   (`IGNORE`, `ACCEPT`, ...) are kept.
#'
#' @returns The rewritten control stream as a character vector of lines,
#'   invisibly when `output_file` is given.
#'
#' @export
nm_rewrite_input <- function(model_file, data_names, new_data_names,
                             rename = NULL, output_file = NULL, data_file = NULL) {
  # express the source columns in the new dataset's names
  src_as_new <- data_names
  if (!is.null(rename)) {
    if (is.null(names(rename)) || any(!nzchar(names(rename)))) {
      stop("`rename` must be a named character vector.", call. = FALSE)
    }
    i <- match(toupper(rename), toupper(data_names))
    if (anyNA(i)) {
      stop("`rename` names source column(s) not in `data_names`: ",
           paste(rename[is.na(i)], collapse = ", "), ".", call. = FALSE)
    }
    src_as_new[i] <- names(rename)
  }
  inp <- nm_input_columns(model_file, src_as_new)
  is_missing <- function(x) x[!toupper(x) %in% toupper(new_data_names)]
  missing <- is_missing(nm_columns_read_in(model_file, src_as_new, exclude = "TABLE"))
  if (length(missing) > 0) {
    stop("The control stream reads column(s) missing from the new data: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  table_only <- is_missing(nm_columns_read(model_file, src_as_new))
  if (length(table_only) > 0) {
    warning("Removed from $TABLE, as missing from the new data: ",
            paste(table_only, collapse = ", "), ".", call. = FALSE)
  }
  tokens <- vapply(new_data_names, function(col) {
    i <- match(toupper(col), toupper(inp$column))
    if (is.na(i)) paste0(col, "=DROP") else inp$token[i]
  }, character(1), USE.NAMES = FALSE)

  records <- nm_records(readLines(model_file, warn = FALSE))
  rec_names <- nm_record_names(records)
  records[[which(rec_names == "INPUT")]] <- nm_wrap_record("$INPUT", tokens)
  if (length(table_only) > 0) {
    hit <- inp[match(toupper(table_only), toupper(inp$column)), , drop = FALSE]
    drop_names <- stats::na.omit(c(hit$label, hit$alias))
    for (i in which(rec_names == "TABLE")) {
      records[[i]] <- nm_drop_items(records[[i]], drop_names)
    }
  }
  if (!is.null(data_file)) {
    i <- which(rec_names == "DATA")
    if (length(i) != 1) stop("Expected exactly one $DATA record.", call. = FALSE)
    records[[i]][1] <- sub("^(\\s*\\$DATA\\s+)(\"[^\"]*\"|'[^']*'|\\S+)",
                           paste0("\\1", data_file), records[[i]][1],
                           ignore.case = TRUE)
  }
  out <- unlist(records, use.names = FALSE)
  if (is.null(output_file)) return(out)
  writeLines(out, output_file)
  invisible(out)
}

# Split control-stream lines into records, each starting at a `$` line.
nm_records <- function(lines) {
  starts <- grepl("^\\s*\\$", lines)
  if (!any(starts)) return(list(lines))
  group <- cumsum(starts)
  # lines before the first record stay attached to it
  group[group == 0] <- 1
  unname(split(lines, group))
}

nm_record_names <- function(records) {
  vapply(records, function(r) {
    # the first record can be preceded by comment lines
    head <- r[grepl("^\\s*\\$", r)][1]
    if (is.na(head)) return("")
    nm <- toupper(sub("^\\s*\\$([A-Za-z]+).*", "\\1", head))
    # NONMEM accepts a record name abbreviated to three or more characters
    full <- c("INPUT", "DATA", "PROBLEM", "TABLE")
    hit <- full[nchar(nm) >= 3 & startsWith(full, nm)]
    if (length(hit) > 0) hit[1] else nm
  }, character(1))
}

nm_strip_comments <- function(lines) sub(";.*$", "", lines)

# Upper-cased identifiers used outside $INPUT and $PROBLEM. In $DATA only the
# parenthesised IGNORE/ACCEPT filters count; the file name and IGNORE=@ do not.
nm_referenced_names <- function(lines, exclude = character()) {
  records <- nm_records(nm_strip_comments(lines))
  rec_names <- nm_record_names(records)
  text <- character()
  for (i in seq_along(records)) {
    body <- paste(records[[i]], collapse = " ")
    if (rec_names[i] %in% c("INPUT", "PROBLEM", exclude)) next
    if (rec_names[i] == "DATA") {
      body <- unlist(regmatches(body, gregexpr(
        "(IGNORE|ACCEPT)\\s*=?\\s*\\([^)]*\\)", body, ignore.case = TRUE)))
      body <- sub("^(IGNORE|ACCEPT)", "", body, ignore.case = TRUE)
      # a quoted comparison value is data, not a column
      body <- gsub("'[^']*'|\"[^\"]*\"", " ", body)
      # .EQ., .NE., ... are operators
      body <- gsub("\\.[A-Za-z]{2}\\.", " ", body)
    } else {
      body <- sub("\\$[A-Za-z]+", "", body)
    }
    text <- c(text, body)
  }
  ids <- unlist(regmatches(text, gregexpr("[A-Za-z_][A-Za-z0-9_]*", text)))
  unique(toupper(ids))
}

# Remove whole-word items (case-insensitive) from the code part of each line,
# keeping indentation and comments.
nm_drop_items <- function(lines, items) {
  pat <- paste0("(?<![\\w(])(", paste(items, collapse = "|"), ")(?![\\w(])")
  vapply(lines, function(l) {
    code <- sub(";.*$", "", l)
    comment <- substring(l, nchar(code) + 1)
    lead <- sub("^(\\s*).*$", "\\1", code)
    body <- gsub(pat, "", substring(code, nchar(lead) + 1), perl = TRUE, ignore.case = TRUE)
    body <- trimws(gsub("(?<=\\S)[ \t]{2,}(?=\\S)", " ", body, perl = TRUE))
    paste0(lead, body, if (nzchar(comment)) paste0(if (nzchar(body)) " " else "", comment) else "")
  }, character(1), USE.NAMES = FALSE)
}

nm_wrap_record <- function(head, tokens, width = 78) {
  lines <- character()
  cur <- head
  for (tok in tokens) {
    if (nchar(cur) + 1 + nchar(tok) > width && cur != head) {
      lines <- c(lines, cur)
      cur <- paste0(strrep(" ", nchar(head)), " ", tok)
    } else {
      cur <- paste(cur, tok)
    }
  }
  c(lines, cur)
}
