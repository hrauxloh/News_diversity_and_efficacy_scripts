# helpers.R ---------------------------------------------------------------
# Helper functions for the revision analysis. Sourced by the numbered scripts.

suppressPackageStartupMessages({
  library(dplyr)
  library(splines)
  library(ggplot2)
  library(sandwich)
  library(lmtest)
})

if (!exists("RA_DIR")) RA_DIR <- "revision_analysis"   # run scripts from the repo root
OUT_DIR <- file.path(RA_DIR, "output")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# z-score returning a plain numeric vector
zs <- function(x) as.numeric(scale(x))

# Mode of a vector (used to hold categorical controls constant in predictions)
mode_val <- function(x) {
  ux <- unique(stats::na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# One row with every model predictor except `focal` (and except those in `fixed`)
# held at its mean (numeric) or mode (factor/character).
base_row <- function(model, data, focal) {
  vars <- setdiff(all.vars(delete.response(terms(model))), focal)
  out <- lapply(vars, function(v) {
    x <- data[[v]]
    if (is.numeric(x)) mean(x, na.rm = TRUE) else mode_val(x)
  })
  names(out) <- vars
  as.data.frame(out, stringsAsFactors = FALSE)
}

# Design matrix at chosen values of the focal predictor.
# `fixed` = named list of values for other predictors, e.g. list(wave = "W4").
design_at <- function(model, data, focal, values, fixed = NULL) {
  nd <- base_row(model, data, focal)[rep(1, length(values)), , drop = FALSE]
  nd[[focal]] <- values
  for (nm in names(fixed)) {
    nd[[nm]] <- factor(fixed[[nm]], levels = levels(data[[nm]]))
  }
  tt <- delete.response(terms(model))
  model.matrix(tt, model.frame(tt, nd, xlev = model$xlevels))
}

# Predicted outcome with 95% CI (HC3-robust by default)
predict_curve <- function(model, data, focal, values, fixed = NULL,
                          vcov. = sandwich::vcovHC(model, type = "HC3")) {
  X   <- design_at(model, data, focal, values, fixed)
  fit <- as.numeric(X %*% coef(model))
  se  <- sqrt(rowSums((X %*% vcov.) * X))
  tq  <- qt(.975, model$df.residual)
  data.frame(value = values, fit = fit, lwr = fit - tq * se, upr = fit + tq * se)
}

# Difference in predicted outcome between two values of the focal predictor
contrast_at <- function(model, data, focal, v_lo, v_hi, fixed = NULL,
                        vcov. = sandwich::vcovHC(model, type = "HC3")) {
  X   <- design_at(model, data, focal, c(v_lo, v_hi), fixed)
  L   <- X[2, ] - X[1, ]
  est <- sum(L * coef(model))
  se  <- sqrt(as.numeric(t(L) %*% vcov. %*% L))
  tq  <- qt(.975, model$df.residual)
  data.frame(diff = est, lwr = est - tq * se, upr = est + tq * se)
}

# Contrast between two PERCENTILES of the focal predictor
contrast_pct <- function(model, data, focal, p_lo, p_hi, fixed = NULL, sd_outcome = NULL) {
  v <- quantile(data[[focal]], c(p_lo, p_hi), names = FALSE)
  out <- contrast_at(model, data, focal, v[1], v[2], fixed)
  out <- cbind(from_pct = p_lo * 100, to_pct = p_hi * 100, out)
  if (!is.null(sd_outcome)) out$diff_in_SD <- out$diff / sd_outcome
  out
}

# Repeated k-fold cross-validated RMSE (knots are re-estimated inside each fold)
cv_rmse <- function(formula, data, outcome = "Internal", k = 10, reps = 20, seed = 2025) {
  set.seed(seed)
  n <- nrow(data)
  mean(replicate(reps, {
    folds <- sample(rep(seq_len(k), length.out = n))
    sq <- unlist(lapply(seq_len(k), function(i) {
      fit <- lm(formula, data = data[folds != i, ])
      (data[[outcome]][folds == i] - predict(fit, newdata = data[folds == i, ]))^2
    }))
    sqrt(mean(sq))
  }))
}

# Robust (HC3) coefficient table as a data frame
robust_table <- function(model) {
  ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3"))
  data.frame(term = rownames(ct), unclass(ct), row.names = NULL, check.names = FALSE)
}

# Robust coefficient (and p-value) of one named term, as a one-row data frame
focal_term <- function(model, term, label = term) {
  ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3"))
  if (!term %in% rownames(ct)) stop("term not found: ", term)
  data.frame(label = label, n = nobs(model),
             estimate = ct[term, 1], se = ct[term, 2], p = ct[term, 4],
             lwr = ct[term, 1] - qt(.975, model$df.residual) * ct[term, 2],
             upr = ct[term, 1] + qt(.975, model$df.residual) * ct[term, 2],
             adj_r2 = summary(model)$adj.r.squared, row.names = NULL)
}
