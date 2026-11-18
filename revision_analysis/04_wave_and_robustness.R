# 04_wave_and_robustness.R -------------------------------------------------
# Robustness analyses. Run 01 and 02 first (03 is independent but gives the
# primary results these are compared against).
#   A. Survey wave (comment 9): wave control, diversity x wave, W4-only
#   B. Extra controls and exposure volume
#   C. Alternative / component diversity measures (comments 3, 7)
#   D. Tracking-coverage restrictions (previously "Samples 2 and 3")

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
stopifnot(exists("analysis_data"))
d <- analysis_data
sd_y <- sd(d$Internal)
BASE <- "age_z + gender + edu_z + log_unique_days + offline_z"
mk <- function(rhs, controls = BASE) as.formula(paste("Internal ~", rhs, "+", controls))

# ---- A. Wave -------------------------------------------------------------
wave_n <- as.data.frame(table(wave = d$wave))
print(wave_n); write.csv(wave_n, file.path(OUT_DIR, "wave_counts.csv"), row.names = FALSE)

a1 <- lm(mk("rao_z + wave"), d)                          # wave control
a2 <- lm(mk("rao_z * wave"), d)                          # diversity x wave
a3 <- lm(mk("ns(rao_z, df = 4) + wave"), d)              # spline + wave control
a3i <- lm(mk("ns(rao_z, df = 4) * wave"), d)             # spline x wave
wave_tests <- rbind(
  data.frame(test = "linear: add wave",              as.data.frame(anova(lm(mk("rao_z"), d), a1))[2, c("F", "Pr(>F)")]),
  data.frame(test = "linear: rao x wave",            as.data.frame(anova(a1, a2))[2, c("F", "Pr(>F)")]),
  data.frame(test = "spline: rao x wave",            as.data.frame(anova(a3, a3i))[2, c("F", "Pr(>F)")])
)
print(wave_tests, digits = 3)
write.csv(wave_tests, file.path(OUT_DIR, "wave_tests.csv"), row.names = FALSE)

# W4-only: efficacy measured at the END of tracking, so diversity (tracked
# Mar 2022 - Dec 2023) precedes the outcome. W1: most tracking comes AFTER efficacy.
by_wave <- bind_rows(lapply(c("W1", "W4"), function(w) {
  dw <- droplevels(subset(d, wave == w))
  m1 <- lm(mk("rao_z"), dw); m3 <- lm(mk("ns(rao_z, df = 4)"), dw)
  bind_rows(
    cbind(sample = w, model = "linear", focal_term(m1, "rao_z", "Rao (per SD)")[, c("n", "estimate", "lwr", "upr", "p")]),
    cbind(sample = w, model = "spline: 10th vs 90th pct", n = nrow(dw),
          setNames(contrast_pct(m3, dw, "rao_z", .10, .90)[, c("diff", "lwr", "upr")],
                   c("estimate", "lwr", "upr")), p = NA))
}))
print(by_wave, digits = 3)
write.csv(by_wave, file.path(OUT_DIR, "by_wave.csv"), row.names = FALSE)

# ---- B. Extra controls and exposure volume -------------------------------
EXT <- paste(BASE, "+ social_z + socialmedia_z + log_visits_z")
dext <- d[stats::complete.cases(d[, c("social_z", "socialmedia_z", "log_visits_z")]), ]
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
  dd <- subset(d, unique_days >= min_days)
  m1 <- lm(mk("rao_z"), dd); m3 <- lm(mk("ns(rao_z, df = 4)"), dd)
  bind_rows(
    cbind(min_days = min_days, model = "linear", focal_term(m1, "rao_z", "Rao (per SD)")[, c("n", "estimate", "lwr", "upr", "p")]),
    cbind(min_days = min_days, model = "spline: 10th vs 90th pct", n = nrow(dd),
          setNames(contrast_pct(m3, dd, "rao_z", .10, .90)[, c("diff", "lwr", "upr")],
                   c("estimate", "lwr", "upr")), p = NA))
}))
print(cov_res, digits = 3)
write.csv(cov_res, file.path(OUT_DIR, "coverage_restrictions.csv"), row.names = FALSE)
