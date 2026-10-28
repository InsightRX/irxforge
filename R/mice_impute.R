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

  lapply(seq_len(m), function(i) {
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
        dat[!ry, v] <- mice_impute_variable(
          y = dat[[v]],
          ry = ry,
          X = X,
          method = method[[v]],
          donors = donors,
          ridge = ridge
        )
      }
    }
    dat
  })
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
#' @returns numeric matrix
#' @keywords internal
mice_design_matrix <- function(df) {
  cols <- lapply(df, function(x) {
    if (is.factor(x)) {
      x <- droplevels(x)
      if (nlevels(x) < 2) return(NULL)
      stats::model.matrix(~ x)[, -1, drop = FALSE]
    } else if (is.numeric(x)) {
      if (length(unique(x)) < 2) return(NULL)
      matrix(as.numeric(x), ncol = 1)
    } else {
      NULL
    }
  })
  X <- do.call(cbind, c(list(rep(1, nrow(df))), cols))
  unname(X)
}

#' Draw imputations for a single variable
#'
#' @param y vector with current (completed) values of the variable
#' @param ry logical vector, `TRUE` where `y` was originally observed
#' @param X design matrix
#' @inheritParams mice_impute
#' @returns vector of imputed values for `y[!ry]`
#' @keywords internal
mice_impute_variable <- function(y, ry, X, method, donors = 5L, ridge = 1e-5) {
  X_obs <- X[ry, , drop = FALSE]
  X_mis <- X[!ry, , drop = FALSE]
  if (method %in% c("pmm", "norm")) {
    draw <- mice_norm_draw(y[ry], X_obs, ridge = ridge)
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
  eta <- matrix(eta, nrow = nrow(X_mis))
  expeta <- exp(cbind(0, eta))
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
#' @param y numeric outcome vector (observed values)
#' @param X design matrix for observed values
#' @inheritParams mice_impute
#' @returns list with least-squares estimate `coef`, drawn coefficients `beta`
#' and drawn residual standard deviation `sigma`
#' @keywords internal
mice_norm_draw <- function(y, X, ridge = 1e-5) {
  xtx <- crossprod(X)
  pen <- ridge * diag(xtx)
  pen[pen == 0] <- ridge
  v <- solve(xtx + diag(pen, nrow = ncol(X)))
  v <- (v + t(v)) / 2
  coef <- v %*% crossprod(X, y)
  residuals <- y - X %*% coef
  df <- max(length(y) - ncol(X), 1)
  sigma <- sqrt(sum(residuals^2) / stats::rchisq(1, df))
  beta <- coef + t(chol(v)) %*% stats::rnorm(ncol(X)) * sigma
  list(coef = as.vector(coef), beta = as.vector(beta), sigma = sigma)
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
  beta[!est] <- 0
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
