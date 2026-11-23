# rao_own.R ------------------------------------------------------------------
# Own implementation of Rao's quadratic entropy (no SYNCSA needed), used by 01_prepare_data.R
# and checkable against the package.
#
#   Q_i = sum_j sum_k d_jk * p_ij * p_ik
# where p_ij is the share of participant i's abundance (visits or seconds) that falls on outlet j,
# and d_jk is the dissimilarity between outlets j and k computed from their traits.
#
# Dissimilarity: Gower distance on the trait columns. With 0/1 dummy columns this is the mean
# absolute difference over the columns (columns that do not vary are ignored). The package
# documentation describes its distance as the square root of 1 - Gower similarity, so the default
# here is "sqrt_gower"; 01_prepare_data.R checks which variant matches the package scores.

trait_distance <- function(traits, how = c("sqrt_gower", "gower")) {
  how <- match.arg(how)
  X <- as.matrix(traits)
  storage.mode(X) <- "double"
  rng <- apply(X, 2, function(x) diff(range(x)))
  keep <- rng > 0
  if (!any(keep)) stop("None of the trait columns varies between outlets.")
  Xs <- sweep(X[, keep, drop = FALSE], 2, rng[keep], "/")
  D <- as.matrix(stats::dist(Xs, method = "manhattan")) / ncol(Xs)   # mean absolute difference
  dimnames(D) <- list(rownames(X), rownames(X))
  if (how == "sqrt_gower") D <- sqrt(D)
  D
}

rao_q <- function(comm, D) {
  comm <- as.matrix(comm)
  stopifnot(!is.null(colnames(comm)), all(colnames(comm) %in% rownames(D)))
  D <- D[colnames(comm), colnames(comm), drop = FALSE]
  tot <- rowSums(comm)
  if (any(tot <= 0)) stop("Some participants have no abundance.")
  p <- comm / tot
  Q <- rowSums((p %*% D) * p)
  names(Q) <- rownames(comm)
  Q
}

# Compare own scores with scores from the package (named vector or data frame psid_tracking / FunRao)
compare_rao <- function(own, pkg) {
  if (is.data.frame(pkg)) pkg <- setNames(pkg[[2]], as.character(pkg[[1]]))
  ids <- intersect(names(own), names(pkg))
  a <- own[ids]; b <- pkg[ids]
  data.frame(n = length(ids), correlation = cor(a, b),
             mean_ratio_pkg_over_own = mean(b[a > 0] / a[a > 0]),
             max_abs_diff = max(abs(a - b)))
}

# Builds comm and traits_mat as in the original pipeline (visits per participant and domain, participants
# with more than min_views views; dummy-coded news type and orientation)
build_comm_traits <- function(tracking_data_0, domain_attributes, min_views = 30) {
  suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(tibble) })
  tr <- tracking_data_0 %>% group_by(psid_tracking) %>% filter(n() > min_views)
  da <- domain_attributes %>% filter(!(domain == "yahoo.com" & news_type == "portal"))
  comm <- tr %>% count(psid_tracking, domain) %>%
    pivot_wider(names_from = domain, values_from = n, values_fill = 0) %>%
    column_to_rownames("psid_tracking")
  traits <- da %>% distinct(domain, news_type, Orientation) %>% column_to_rownames("domain")
  traits[] <- lapply(traits, as.factor)
  list(comm = comm, traits_mat = model.matrix(~ . - 1, data = traits))
}
