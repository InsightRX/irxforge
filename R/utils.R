#' Defuse a list of symbols
#'
#' @param x A (named) list of strings.
#'
#' @returns
#' A (named) list of expressions.
syms_to_exprs <- function(x) {
  rlang::exprs(!!!rlang::syms(x))
}

match.closest <- function(x, y) {
  cuts <- c(-Inf, y[-1] - diff(y)/2, Inf)
  idx <- findInterval(x, cuts)
}
