# 04_wave_and_robustness.R -------------------------------------------------
# Robustness analyses. Run 01 and 02 first (03 is independent but gives the
# primary results these are compared against).
#   A. Survey wave (comment 9): a wave dummy is included in ALL models below
#      (efficacy is treated as stable over time, so there are no wave-specific
#      models, no diversity x wave interaction and no timing-based subsetting)
#   B. Extra controls and exposure volume
#   C. Alternative / component diversity measures (comments 3, 7)
#   D. Tracking-coverage restrictions (previously "Samples 2 and 3")

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
stopifnot(exists("analysis_data"))
d <- analysis_data
sd_y <- sd(d$Internal)
BASE_NO_WAVE <- "age_z + gender + edu_z + log_unique_days + offline_z + offline_missing"
BASE <- paste(BASE_NO_WAVE, "+ wave")      # wave dummy (W4 vs W1) in every model
mk <- function(rhs, controls = BASE) as.formula(paste("Internal ~", rhs, "+", controls))
# Subsets may contain only one wave; then the dummy cannot be estimated and is dropped.
ctrl_for <- function(dd, extra = "") {
  base <- if (nlevels(droplevels(dd$wave)) > 1) BASE else BASE_NO_WAVE
  paste(base, extra)
}

# ---- A. Wave -------------------------------------------------------------
wave_n <- as.data.frame(table(wave = d$wave))
print(wave_n); write.csv(wave_n, file.path(OUT_DIR, "wave_counts.csv"), row.names = FALSE)

# Effect of adding the wave dummy to the linear model: does it change the diversity estimate?
w0 <- lm(mk("rao_z", BASE_NO_WAVE), d)                   # without wave
w1 <- lm(mk("rao_z"), d)                                 # with wave dummy (as in script 03)
wave_check <- bind_rows(
  focal_term(w0, "rao_z",   "Rao (per SD), without wave dummy"),
  focal_term(w1, "rao_z",   "Rao (per SD), with wave dummy"),
  focal_term(w1, "waveW4",  "wave dummy (W4 vs W1)"))
print(wave_check, digits = 3)
write.csv(wave_check, file.path(OUT_DIR, "wave_dummy_check.csv"), row.names = FALSE)

# ---- B. Extra controls and exposure volume -------------------------------
dext <- droplevels(d[stats::complete.cases(d[, c("social_z", "socialmedia_z", "log_visits_z")]), ])
EXT <- ctrl_for(dext, "+ social_z + socialmedia_z + log_visits_z")
b1 <- lm(mk("rao_z", EXT), dext); b3 <- lm(mk("ns(rao_z, df = 4)", EXT), dext)
ext <- bind_rows(
  cbind(model = "linear + extra controls", focal_term(b1, "rao_z", "Rao (per SD)")[, c("n", "estimate", "lwr", "upr", "p")]),
  cbind(model = "spline + extra controls: 10th vs 90th pct", n = nrow(dext),
        setNames(contrast_pct(b3, dext, "rao_z", .10, .90)[, c("diff", "lwr", "upr")],
                 c("estimate", "lwr", "upr")), p = NA))
print(ext, digits = 3)
write.csv(ext, file.path(OUT_DIR, "extra_controls.csv"), row.names = FALSE)

# ---- C. Alternative / component diversity measures -----------------------
need <- c("shannon_z", "outlets_z", "rao_type_z", "rao_ideol_z")
if (all(need %in% names(d))) {
  alt_terms <- c("rao_z", "shannon_z", "outlets_z", "rao_type_z", "rao_ideol_z")
  alt <- bind_rows(lapply(alt_terms, function(v) {
    m <- lm(mk(v), d); focal_term(m, v, paste(v, "(per SD)"))
  }))
  joint <- lm(mk("rao_type_z + rao_ideol_z"), d)           # which trait carries the association?
  alt <- bind_rows(alt,
                   focal_term(joint, "rao_type_z",  "rao_type_z (joint model)"),
                   focal_term(joint, "rao_ideol_z", "rao_ideol_z (joint model)"))
  # Does Rao add anything beyond Shannon entropy and the number of outlets?
  both <- lm(mk("rao_z + shannon_z + outlets_z"), d)
  alt <- bind_rows(alt, focal_term(both, "rao_z", "rao_z controlling Shannon + n outlets"))
  print(alt, digits = 3)
  write.csv(alt, file.path(OUT_DIR, "alternative_measures.csv"), row.names = FALSE)
} else {
  message("Skipping C: run 02_alt_diversity_measures.R first.")
}

# ---- D. Tracking-coverage restrictions ------------------------------------
cov_res <- bind_rows(lapply(c(0, 50, 150), function(min_days) {
  dd <- droplevels(subset(d, unique_days >= min_days))
  cc <- ctrl_for(dd)
  m1 <- lm(mk("rao_z", cc), dd); m3 <- lm(mk("ns(rao_z, df = 4)", cc), dd)
  bind_rows(
    cbind(min_days = min_days, model = "linear", focal_term(m1, "rao_z", "Rao (per SD)")[, c("n", "estimate", "lwr", "upr", "p")]),
    cbind(min_days = min_days, model = "spline: 10th vs 90th pct", n = nrow(dd),
          setNames(contrast_pct(m3, dd, "rao_z", .10, .90)[, c("diff", "lwr", "upr")],
                   c("estimate", "lwr", "upr")), p = NA))
}))
print(cov_res, digits = 3)
write.csv(cov_res, file.path(OUT_DIR, "coverage_restrictions.csv"), row.names = FALSE)
