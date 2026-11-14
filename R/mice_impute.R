#' Multivariate imputation by chained equations
#'
#' Minimal, dependency-free re-implementation of the subset of the `mice`
#' package (van Buuren & Groothuis-Oudshoorn, 2011) that is used by
#' [sample_covariates_mice()]. Missing values are imputed variable-by-variable
#' using a Gibbs sampler: each incomplete variable is regressed on all other
#' (current, completed) predictors, and new imputations are drawn from the
#' posterior predictive distribution of that model. This is repeated for
#' `maxit` iterations, independently for each of the `m` replicates.
#'
#' Supported imputation methods:
#' - `pmm`: predictive mean matching (type 1 matching, as in `mice`). Imputed
#'   values are always values that are observed in the data.
#' - `norm`: Bayesian linear regression (Rubin, 1987, p. 167).
#' - `logreg`: Bayesian logistic regression, for binary factors. Uses
#'   data augmentation (White, Daniel & Royston, 2010) to deal with perfect
#'   prediction.
#' - `polyreg`: polytomous regression for unordered factors with more than two
#'   levels, approximated by separate baseline-category binary logistic
#'   regressions (Begg & Gray, 1984).
#' - `""`: variable is not imputed.
#'
#' @param data data.frame with missing values coded as `NA`. Categorical
#' variables must be factors.
#' @param method named character vector with the imputation method per
#' variable. Variables not in `method` get a default method based on their
#' type: `pmm` for numeric, `logreg` for factors with 2 levels, and `polyreg`
#' for factors with more than 2 levels.
#' @param predictors character vector with names of variables that may be used
#' as predictors. Default is all variables in `data`.
#' @param m number of imputed data sets (replicates).
#' @param maxit number of iterations of the Gibbs sampler.
#' @param donors number of donors to sample from in predictive mean matching.
#' @param ridge ridge penalty used in the linear regression draws, to stabilize
#' the estimation in case of collinear predictors.
#'
#' @returns list of `m` completed data.frames.
#'
#' @references
#' van Buuren S, Groothuis-Oudshoorn K (2011). mice: Multivariate Imputation by
#' Chained Equations in R. Journal of Statistical Software, 45(3), 1-67.
#'
#' Rubin DB (1987). Multiple Imputation for Nonresponse in Surveys. Wiley.
#'
#' White IR, Daniel R, Royston P (2010). Avoiding bias due to perfect
#' prediction in multiple imputation of incomplete categorical variables.
#' Computational Statistics & Data Analysis, 54(10), 2267-2275.
#'
#' Begg CB, Gray R (1984). Calculation of polychotomous logistic regression
#' parameters using individualized regressions. Biometrika, 71(1), 11-18.
#'
#' @keywords internal
mice_impute <- function(
  data,
  method = NULL,
  predictors = names(data),
  m = 1,
  maxit = 5,
  donors = 5L,
  ridge = 1e-5
) {
  data <- as.data.frame(data)
  where <- is.na(data)
  method <- mice_default_method(data, method)
  impute_vars <- names(data)[colSums(where) > 0 & method != ""]
  for (v in impute_vars) {
    if (all(where[, v])) {
      stop("Variable `", v, "` has no observed values, cannot impute.")
    }
    if (method[[v]] %in% c("pmm", "norm") && !is.numeric(data[[v]])) {
      stop("Method `", method[[v]], "` requires numeric variable `", v, "`.")
    }
    if (method[[v]] %in% c("logreg", "polyreg") && !is.factor(data[[v]])) {
      stop("Method `", method[[v]], "` requires factor variable `", v, "`.")
    }
  }
  # variables that remain incomplete cannot be used as predictors
  incomplete <- names(data)[colSums(where) > 0 & method == ""]
  predictors <- setdiff(predictors, incomplete)

  dedupe_mice_warnings(lapply(seq_len(m), function(i) {
    dat <- data
    # initialize missing values with random draws from the observed values
    for (v in impute_vars) {
      ry <- !where[, v]
      dat[!ry, v] <- dat[ry, v][sample.int(sum(ry), sum(!ry), replace = TRUE)]
    }
    for (it in seq_len(maxit)) {
      for (v in impute_vars) {
        ry <- !where[, v]
        X <- mice_design_matrix(dat[setdiff(predictors, v)])
        dat[!ry, v] <- tryCatch(
          mice_impute_variable(
            y = dat[[v]],
            ry = ry,
            X = X,
            method = method[[v]],
            donors = donors,
            ridge = ridge,
            var = v
          ),
          error = function(e) {
            stop(
              "Imputation of variable `", v, "` (method `", method[[v]],
              "`) failed: ", conditionMessage(e),
              call. = FALSE
            )
          }
        )
      }
    }
    still_missing <- impute_vars[colSums(is.na(dat[impute_vars])) > 0]
    if (length(still_missing) > 0) {
      stop(
        "Imputation failed to complete variable(s): ",
        paste(still_missing, collapse = ", ")
      )
    }
    dat
  }))
}

#' Default imputation method per variable, based on variable type
#'
#' @inheritParams mice_impute
#' @returns named character vector of methods for all variables in `data`
#' @keywords internal
mice_default_method <- function(data, method = NULL) {
  out <- vapply(data, function(x) {
    if (is.numeric(x)) {
      "pmm"
    } else if (is.factor(x)) {
      if (nlevels(x) > 2) "polyreg" else "logreg"
    } else {
      ""
    }
  }, character(1))
  allowed <- c("pmm", "norm", "logreg", "polyreg", "")
  if (!is.null(method)) {
    unknown <- setdiff(method, allowed)
    if (length(unknown) > 0) {
      stop(
        "Unsupported imputation method(s): ", paste(unknown, collapse = ", "),
        ". Supported are: ", paste(allowed[allowed != ""], collapse = ", ")
      )
    }
    keep <- intersect(names(method), names(out))
    out[keep] <- method[keep]
  }
  out
}

#' Create design matrix (with intercept) from predictor data
#'
#' Factors are dummy-coded, and constant predictors are dropped.
#'
#' @param df data.frame with complete predictor data
#' @returns numeric matrix, with column names identifying the predictor (and
#'   factor level) of each column; the first column is the intercept
#' @keywords internal
mice_design_matrix <- function(df) {
  cols <- lapply(names(df), function(nm) {
    x <- df[[nm]]
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) < 2) return(NULL)
      out <- stats::model.matrix(~ x)[, -1, drop = FALSE]
      dimnames(out) <- list(NULL, paste0(nm, levels(x)[-1]))
      out
    } else if (is.numeric(x)) {
      if (length(unique(x)) < 2) return(NULL)
      matrix(as.numeric(x), ncol = 1, dimnames = list(NULL, nm))
    } else {
      NULL
    }
  })
  intercept <- matrix(1, nrow = nrow(df), ncol = 1,
                      dimnames = list(NULL, "(Intercept)"))
  do.call(cbind, c(list(intercept), cols))
}

#' Draw imputations for a single variable
#'
#' @param y vector with current (completed) values of the variable
#' @param ry logical vector, `TRUE` where `y` was originally observed
#' @param X design matrix
#' @param var name of the variable being imputed, used in messages.
#' @inheritParams mice_impute
#' @returns vector of imputed values for `y[!ry]`
#' @keywords internal
mice_impute_variable <- function(
  y, ry, X, method, donors = 5L, ridge = 1e-5, var = NULL
) {
  X_obs <- X[ry, , drop = FALSE]
  X_mis <- X[!ry, , drop = FALSE]
  if (method %in% c("pmm", "norm")) {
    draw <- mice_norm_draw(y[ry], X_obs, ridge = ridge, var = var)
    if (method == "norm") {
      return(
        as.vector(X_mis %*% draw$beta) + stats::rnorm(nrow(X_mis)) * draw$sigma
      )
    }
    yhat_obs <- as.vector(X_obs %*% draw$coef)
    yhat_mis <- as.vector(X_mis %*% draw$beta)
    idx <- mice_match_donors(yhat_obs, yhat_mis, donors = donors)
    return(y[ry][idx])
  }
  # categorical methods
  y_obs <- droplevels(y[ry])
  lev_obs <- levels(y_obs)
  if (length(lev_obs) == 1) {
    return(factor(rep(lev_obs, nrow(X_mis)), levels = levels(y)))
  }
  # linear predictors for each non-reference level vs the reference level
  # (separate binary logistic regressions, Begg & Gray 1984). For a binary
  # variable (logreg) this reduces to a single logistic regression.
  eta <- vapply(lev_obs[-1], function(l) {
    sel <- y_obs %in% c(lev_obs[1], l)
    beta <- mice_logreg_draw(
      y = as.integer(y_obs[sel] == l),
      X = X_obs[sel, , drop = FALSE]
    )
    as.vector(X_mis %*% beta)
  }, numeric(nrow(X_mis)))
  eta <- cbind(0, matrix(eta, nrow = nrow(X_mis)))
  # numerically stable softmax: bound the linear predictors (overflow to +/-Inf
  # would otherwise give Inf - Inf = NaN) and subtract the row maximum before
  # exponentiation
  eta[is.nan(eta)] <- 0
  eta <- pmin(pmax(eta, -500), 500)
  eta <- eta - apply(eta, 1, max)
  expeta <- exp(eta)
  prob <- expeta / rowSums(expeta)
  # draw category for each missing value
  u <- stats::runif(nrow(prob))
  cat_idx <- 1 + rowSums(u > t(apply(prob, 1, cumsum)))
  cat_idx <- pmin(cat_idx, length(lev_obs))
  factor(lev_obs[cat_idx], levels = levels(y))
}

#' Draw parameters of a Bayesian linear regression model
#'
#' Following Rubin (1987, p. 167), as implemented in `mice:::.norm.draw()`.
#'
#' The penalized normal equations are solved on the correlation scale (the
#' columns of `X` scaled to unit root-mean-square), which is mathematically
#' identical to applying the ridge penalty `ridge * diag(crossprod(X))` but
#' prevents predictors on very different scales from making the system
#' numerically singular. If the system is still singular (e.g. because of
#' (near-)collinear predictors), a pseudo-inverse is used instead and a warning
#' naming the imputed variable and the collinear predictors is raised.
#'
#' @param y numeric outcome vector (observed values)
#' @param X design matrix for observed values
#' @param var name of the variable being imputed, used in messages.
#' @inheritParams mice_impute
#' @returns list with least-squares estimate `coef`, drawn coefficients `beta`
#' and drawn residual standard deviation `sigma`
#' @keywords internal
mice_norm_draw <- function(y, X, ridge = 1e-5, var = NULL) {
  xtx <- crossprod(X)
  s <- sqrt(diag(xtx))
  s[!is.finite(s) | s == 0] <- 1
  scale_mat <- outer(s, s)
  sxtx <- xtx / scale_mat
  pen <- ridge * diag(sxtx)
  pen[pen == 0] <- ridge
  a <- sxtx + diag(pen, nrow = ncol(X))
  v <- tryCatch(solve(a), error = function(e) NULL)
  if (is.null(v)) {
    v <- mice_pseudo_inverse(a)
    mice_warn_collinear(var, mice_collinear_predictors(X))
  }
  v <- v / scale_mat
  v <- (v + t(v)) / 2
  coef <- v %*% crossprod(X, y)
  residuals <- y - X %*% coef
  df <- max(length(y) - ncol(X), 1)
  sigma <- sqrt(sum(residuals^2) / stats::rchisq(1, df))
  beta <- coef + mice_sqrt_psd(v) %*% stats::rnorm(ncol(X)) * sigma
  list(coef = as.vector(coef), beta = as.vector(beta), sigma = sigma)
}

#' Moore-Penrose pseudo-inverse of a symmetric matrix
#'
#' @param a symmetric numeric matrix
#' @returns pseudo-inverse of `a`
#' @keywords internal
mice_pseudo_inverse <- function(a) {
  e <- eigen((a + t(a)) / 2, symmetric = TRUE)
  tol <- max(dim(a)) * max(abs(e$values)) * .Machine$double.eps
  inv_values <- ifelse(e$values > tol, 1 / e$values, 0)
  e$vectors %*% (inv_values * t(e$vectors))
}

#' Left square root of a symmetric positive semi-definite matrix
#'
#' Returns `L` such that `L %*% t(L)` equals `v`: the transposed Cholesky
#' factor when `v` is positive definite, otherwise a square root based on the
#' eigendecomposition (negative eigenvalues from rounding are set to zero).
#'
#' @param v symmetric positive semi-definite matrix
#' @returns numeric matrix
#' @keywords internal
mice_sqrt_psd <- function(v) {
  ch <- tryCatch(chol(v), error = function(e) NULL)
  if (!is.null(ch)) return(t(ch))
  e <- eigen(v, symmetric = TRUE)
  e$vectors %*% diag(sqrt(pmax(e$values, 0)), nrow = length(e$values))
}

#' Identify (near-)collinear predictors in a design matrix
#'
#' Uses a pivoted QR decomposition of the column-scaled design matrix. Columns
#' that are (near-)linear combinations of the others are reported, together
#' with the columns they depend on.
#'
#' @param X design matrix with column names
#' @param tol tolerance for the QR rank determination
#' @returns character vector with names of the collinear predictors (without
#'   the intercept), possibly empty
#' @keywords internal
mice_collinear_predictors <- function(X, tol = 1e-7) {
  nms <- colnames(X)
  if (is.null(nms)) nms <- paste0("X", seq_len(ncol(X)))
  rms <- sqrt(colMeans(X^2))
  rms[!is.finite(rms) | rms == 0] <- 1
  Xs <- sweep(X, 2, rms, "/")
  q <- qr(Xs, tol = tol)
  if (q$rank >= ncol(Xs)) return(character())
  basis <- q$pivot[seq_len(q$rank)]
  aliased <- q$pivot[-seq_len(q$rank)]
  coefs <- qr.coef(qr(Xs[, basis, drop = FALSE]), Xs[, aliased, drop = FALSE])
  coefs <- matrix(coefs, nrow = length(basis))
  partners <- basis[rowSums(abs(coefs) > 1e-6, na.rm = TRUE) > 0]
  involved <- nms[sort(unique(c(partners, aliased)))]
  setdiff(involved, "(Intercept)")
}

#' Warn that collinear predictors required a pseudo-inverse
#'
#' @param var name of the variable being imputed
#' @param collinear names of the collinear predictors
#' @keywords internal
mice_warn_collinear <- function(var, collinear) {
  msg <- paste0(
    "Predictors used to impute ",
    if (is.null(var)) "a variable" else paste0("`", var, "`"),
    " are (nearly) collinear",
    if (length(collinear) > 0) {
      paste0(": ", paste0("`", collinear, "`", collapse = ", "))
    },
    ". Using a pseudo-inverse for the regression draw. Consider removing ",
    "redundant covariates, or declaring integer-coded categorical covariates ",
    "in `cat_covs`."
  )
  warning(warningCondition(
    msg,
    class = c("irxforge_mice_collinear_warning", "irxforge_mice_warning")
  ))
}

#' Evaluate an expression, emitting each distinct irxforge MICE warning once
#'
#' The Gibbs sampler draws each variable's regression many times (per
#' iteration, replicate and time-varying transition step), so the same warning
#' would otherwise be repeated many times.
#'
#' @param expr expression to evaluate
#' @returns value of `expr`
#' @keywords internal
dedupe_mice_warnings <- function(expr) {
  seen <- list()
  out <- withCallingHandlers(
    expr,
    irxforge_mice_warning = function(w) {
      msg <- conditionMessage(w)
      if (!msg %in% names(seen)) seen[[msg]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  for (w in seen) {
    w$call <- NULL
    warning(w)
  }
  out
}

#' Warn about integer-coded covariates that will be treated as continuous
#'
#' Numeric covariates with only whole-number values and 2 or 3 distinct
#' values (e.g. `SEX = 0/1`, `FED = 0/1`) are most likely categorical codes.
#' Covariates with fewer than 10 non-missing values are not checked. If
#' they are not listed in `cat_covs`, they are imputed as continuous, which
#' gives non-integer values and can make the imputation models (nearly)
#' singular.
#'
#' @param data data.frame with covariates
#' @param covs names of the covariates to check
#' @param cat_covs names of the covariates declared categorical
#' @returns names of the suspicious covariates, invisibly
#' @keywords internal
warn_integer_coded_covariates <- function(data, covs, cat_covs = NULL) {
  covs <- setdiff(intersect(covs, names(data)), cat_covs)
  suspicious <- covs[vapply(covs, function(cov) {
    x <- data[[cov]]
    if (!is.numeric(x)) return(FALSE)
    x <- x[!is.na(x)]
    ux <- unique(x)
    # with very few values, a small number of distinct values is uninformative
    length(x) >= 10 && length(ux) >= 2 && length(ux) <= 3 &&
      all(ux == round(ux))
  }, logical(1))]
  if (length(suspicious) > 0) {
    warning(warningCondition(
      paste0(
        "Integer covariate(s) with at most 3 distinct values are not in ",
        "`cat_covs` and will be imputed as continuous: ",
        paste0("`", suspicious, "`", collapse = ", "),
        ". If these are categorical codes (e.g. `SEX = 0/1`), add them to ",
        "`cat_covs`."
      ),
      class = c("irxforge_integer_coded_warning", "irxforge_mice_warning")
    ))
  }
  invisible(suspicious)
}

#' Reject categorical covariates with implausibly many levels
#'
#' Each categorical covariate is imputed with a (multinomial) logistic
#' regression, whose cost grows quickly with the number of levels. A
#' categorical covariate with hundreds of levels is almost always a continuous
#' column (e.g. a time or dose counter) passed in `cat_covs` by mistake, and
#' would make the sampler run for hours without any output. Covariates listed
#' in `cat_covs`, as well as factor and character columns (which are always
#' treated as categorical), are checked against `max_cat_levels`.
#'
#' @param data data.frame with covariates
#' @param covs names of the covariates to check
#' @param cat_covs names of the covariates declared categorical
#' @param max_cat_levels maximum number of distinct (non-missing) values
#'   allowed for a categorical covariate. `Inf` disables the check.
#' @returns named integer vector with the number of levels of each categorical
#'   covariate, invisibly
#' @keywords internal
check_cat_covs_levels <- function(data, covs, cat_covs = NULL,
                                  max_cat_levels = 20) {
  if (!is.numeric(max_cat_levels) || length(max_cat_levels) != 1 ||
      is.na(max_cat_levels) || max_cat_levels < 2) {
    stop(
      "`max_cat_levels` must be a single number of at least 2 (or `Inf`).",
      call. = FALSE
    )
  }
  covs <- intersect(covs, names(data))
  is_cat <- vapply(covs, function(cov) {
    cov %in% cat_covs || is.factor(data[[cov]]) || is.character(data[[cov]])
  }, logical(1))
  cat <- covs[is_cat]
  n_levels <- vapply(cat, function(cov) {
    x <- data[[cov]]
    length(unique(x[!is.na(x)]))
  }, integer(1))
  names(n_levels) <- cat
  too_many <- n_levels[n_levels > max_cat_levels]
  if (length(too_many) > 0) {
    stop(errorCondition(
      paste0(
        "Categorical covariate(s) with more than ", max_cat_levels,
        " levels: ",
        paste0("`", names(too_many), "` (", too_many, " levels)",
               collapse = ", "),
        ". Imputing a categorical covariate fits a multinomial regression ",
        "per level, which becomes extremely slow with this many levels. ",
        "This usually means a continuous column (e.g. a time, dose or ",
        "occasion counter) was passed in `cat_covs`, or is stored as ",
        "character/factor; treat it as continuous (convert it to numeric ",
        "and remove it from `cat_covs`) or drop it from the covariates. ",
        "If the covariate really is categorical, raise `max_cat_levels`. ",
        "Levels per categorical covariate: ",
        paste0("`", names(n_levels), "` = ", n_levels, collapse = ", "),
        "."
      ),
      class = "irxforge_cat_levels_error",
      call = NULL
    ))
  }
  invisible(n_levels)
}

#' Draw parameters of a Bayesian logistic regression model
#'
#' Fits a logistic regression on data augmented with pseudo-observations
#' (White, Daniel & Royston, 2010) to prevent perfect prediction, and draws
#' coefficients from the asymptotic normal distribution of the estimates.
#'
#' @param y integer outcome vector (0/1)
#' @param X design matrix (first column is the intercept)
#' @returns numeric vector of drawn coefficients (aliased coefficients are 0)
#' @keywords internal
mice_logreg_draw <- function(y, X) {
  aug <- mice_augment(y, X)
  fit <- suppressWarnings( # non-integer #successes due to augmentation weights
    stats::glm.fit(
      x = aug$X,
      y = aug$y,
      weights = aug$w,
      family = stats::binomial()
    )
  )
  coef <- fit$coefficients
  est <- !is.na(coef)
  beta <- rep(0, length(coef))
  rank <- fit$rank
  pivot <- fit$qr$pivot[seq_len(rank)]
  vcov <- chol2inv(fit$qr$qr[seq_len(rank), seq_len(rank), drop = FALSE])
  vcov <- (vcov + t(vcov)) / 2
  chol_v <- tryCatch(chol(vcov), error = function(e) NULL)
  if (is.null(chol_v)) {
    beta[pivot] <- coef[pivot]
  } else {
    beta[pivot] <- coef[pivot] + as.vector(t(chol_v) %*% stats::rnorm(rank))
  }
  beta[!est | !is.finite(beta)] <- 0
  beta
}

#' Augment binary data with weighted pseudo-observations
#'
#' For each predictor (excluding the intercept) and each outcome category, two
#' pseudo-observations are added at the predictor mean +/- half a standard
#' deviation (other predictors at their mean). The pseudo-observations have
#' a total weight of `p + 1`, with `p` the number of predictors
#' (White, Daniel & Royston, 2010).
#'
#' @inheritParams mice_logreg_draw
#' @returns list with augmented `y`, `X`, and weights `w`
#' @keywords internal
mice_augment <- function(y, X) {
  p <- ncol(X) - 1
  categories <- c(0L, 1L)
  if (p == 0) {
    X_aug <- matrix(1, nrow = length(categories), ncol = 1)
    y_aug <- categories
  } else {
    x <- X[, -1, drop = FALSE]
    mu <- colMeans(x)
    sds <- apply(x, 2, stats::sd)
    sds[is.na(sds)] <- 0
    rows <- lapply(seq_len(p), function(j) {
      d <- matrix(mu, nrow = 2, ncol = p, byrow = TRUE)
      d[, j] <- mu[j] + c(0.5, -0.5) * sds[j]
      d
    })
    pseudo <- do.call(rbind, rows)
    pseudo <- pseudo[rep(seq_len(nrow(pseudo)), times = length(categories)), ,
                     drop = FALSE]
    X_aug <- cbind(1, pseudo)
    y_aug <- rep(categories, each = 2 * p)
  }
  n_aug <- length(y_aug)
  list(
    y = c(y, y_aug),
    X = rbind(X, X_aug),
    w = c(rep(1, length(y)), rep((p + 1) / n_aug, n_aug))
  )
}

#' Predictive mean matching: select donors
#'
#' For each missing value, finds the `donors` observed cases with the closest
#' predicted values and randomly selects one of them.
#'
#' @param yhat_obs predicted values for observed cases
#' @param yhat_mis predicted values for missing cases
#' @inheritParams mice_impute
#' @returns integer vector of indices into the observed cases
#' @keywords internal
mice_match_donors <- function(yhat_obs, yhat_mis, donors = 5L) {
  n <- length(yhat_obs)
  k <- min(donors, n)
  # shuffle before sorting so that ties are broken randomly
  perm <- sample.int(n)
  ord <- perm[order(yhat_obs[perm])]
  ys <- yhat_obs[ord]
  pos <- findInterval(yhat_mis, ys)
  vapply(seq_along(yhat_mis), function(i) {
    cand <- seq.int(max(1, pos[i] - k + 1), min(n, pos[i] + k))
    nearest <- cand[order(abs(ys[cand] - yhat_mis[i]))[seq_len(k)]]
    ord[nearest[sample.int(k, 1)]]
  }, integer(1))
}
