#' Apply categorical-to-numeric encoding for specified columns
#'
#' Internal helper used by `reformat_data_*` functions to convert character or
#' factor columns to numeric values according to a user-supplied mapping or
#' automatic frequency-based encoding.
#'
#' @param data A data.frame.
#' @param categorical_mapping Either:
#'   - A character vector of column names to auto-encode (most common value
#'     gets 0, next most common gets 1, etc.).
#'   - A data.frame with columns `column`, `original_value`, `encoded_value`
#'     (case-insensitive) specifying explicit mappings. Values in the data not
#'     covered by the mapping receive continuation integers starting from
#'     `max(encoded_value) + 1`.
#'   - `NULL` (default) to skip encoding.
#'
#' NA values are always encoded as -99.
#'
#' @returns The input `data` with specified columns converted to numeric. A
#'   `"categorical_mapping"` attribute is attached: a data.frame with columns
#'   `column`, `original_value`, `encoded_value` describing the full mapping
#'   used.
#'
#' @keywords internal
apply_categorical_mapping <- function(data, categorical_mapping = NULL) {
  if (is.null(categorical_mapping)) {
    return(data)
  }

  if (is.character(categorical_mapping)) {
    mapping <- apply_categorical_mapping_auto(data, categorical_mapping)
  } else if (is.data.frame(categorical_mapping)) {
    mapping <- apply_categorical_mapping_manual(data, categorical_mapping)
  } else {
    stop(
      "`categorical_mapping` must be a character vector or a data.frame, ",
      "got ", class(categorical_mapping)[1], "."
    )
  }

  # Apply the mapping to data
  data <- apply_mapping_to_data(data, mapping)

  attr(data, "categorical_mapping") <- mapping
  data
}

#' Build an automatic frequency-based mapping
#' @param data A data.frame
#' @param columns Character vector of column names
#' @returns A data.frame with columns `column`, `original_value`, `encoded_value`
#' @keywords internal
apply_categorical_mapping_auto <- function(data, columns) {
  mappings <- list()

  for (col in columns) {
    if (!col %in% names(data)) {
      warning("Column '", col, "' not found in data, skipping.")
      next
    }

    vals <- data[[col]]
    if (is.factor(vals)) vals <- as.character(vals)

    # Frequency table excluding NAs, sorted descending
    freq <- sort(table(vals, useNA = "no"), decreasing = TRUE)
    class_names <- names(freq)

    mappings[[length(mappings) + 1]] <- data.frame(
      column = col,
      original_value = class_names,
      encoded_value = seq_along(class_names) - 1L,
      stringsAsFactors = FALSE
    )
  }

  if (length(mappings) == 0) {
    return(data.frame(
      column = character(0),
      original_value = character(0),
      encoded_value = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, mappings)
}

#' Build a mapping from a user-supplied data.frame, filling in unmapped classes
#' @param data A data.frame
#' @param mapping_df A data.frame with columns column/original_value/encoded_value
#' @returns A data.frame with columns `column`, `original_value`, `encoded_value`
#' @keywords internal
apply_categorical_mapping_manual <- function(data, mapping_df) {
  # Normalize column names to lowercase for matching
  names(mapping_df) <- tolower(names(mapping_df))

  required_cols <- c("column", "original_value", "encoded_value")
  missing_cols <- setdiff(required_cols, names(mapping_df))
  if (length(missing_cols) > 0) {
    stop(
      "`categorical_mapping` data.frame must contain columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ", paste(missing_cols, collapse = ", "), "."
    )
  }

  # Ensure consistent types
  mapping_df$column <- as.character(mapping_df$column)
  mapping_df$original_value <- as.character(mapping_df$original_value)
  mapping_df$encoded_value <- as.numeric(mapping_df$encoded_value)

  mappings <- list()

  for (col in unique(mapping_df$column)) {
    if (!col %in% names(data)) {
      warning("Column '", col, "' not found in data, skipping.")
      next
    }

    col_mapping <- mapping_df[mapping_df$column == col, , drop = FALSE]

    vals <- data[[col]]
    if (is.factor(vals)) vals <- as.character(vals)

    # Find unmapped non-NA values
    mapped_values <- col_mapping$original_value
    unique_vals <- unique(vals[!is.na(vals)])
    unmapped <- setdiff(unique_vals, mapped_values)

    if (length(unmapped) > 0) {
      # Sort unmapped by descending frequency
      freq <- sort(table(vals[vals %in% unmapped], useNA = "no"), decreasing = TRUE)
      unmapped_sorted <- names(freq)

      next_value <- max(col_mapping$encoded_value) + 1L

      extra_mapping <- data.frame(
        column = col,
        original_value = unmapped_sorted,
        encoded_value = seq(next_value, length.out = length(unmapped_sorted)),
        stringsAsFactors = FALSE
      )

      col_mapping <- rbind(
        col_mapping[, required_cols, drop = FALSE],
        extra_mapping
      )
    } else {
      col_mapping <- col_mapping[, required_cols, drop = FALSE]
    }

    mappings[[length(mappings) + 1]] <- col_mapping
  }

  if (length(mappings) == 0) {
    return(data.frame(
      column = character(0),
      original_value = character(0),
      encoded_value = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  result <- do.call(rbind, mappings)
  rownames(result) <- NULL
  result
}

#' Apply a mapping data.frame to the data columns
#' @param data A data.frame
#' @param mapping A data.frame with columns column/original_value/encoded_value
#' @returns The modified data.frame
#' @keywords internal
apply_mapping_to_data <- function(data, mapping) {
  for (col in unique(mapping$column)) {
    if (!col %in% names(data)) next

    col_map <- mapping[mapping$column == col, ]
    lookup <- stats::setNames(col_map$encoded_value, col_map$original_value)

    vals <- data[[col]]
    if (is.factor(vals)) vals <- as.character(vals)

    # Map values: use lookup for known values, -99 for NA
    new_vals <- numeric(length(vals))
    for (i in seq_along(vals)) {
      if (is.na(vals[i])) {
        new_vals[i] <- -99
      } else if (vals[i] %in% names(lookup)) {
        new_vals[i] <- lookup[vals[i]]
      } else {
        # Should not happen if mapping was built correctly, but be safe
        new_vals[i] <- -99
      }
    }

    data[[col]] <- new_vals
  }

  data
}
