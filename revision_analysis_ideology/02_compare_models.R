# 02_compare_models.R --------------------------------------------------------
# Fits the same model sequence for (a) the CURRENT Rao score and (b) the Rao scores that use
# the coded outlet ideology, and puts the results side by side.
#   M0 controls only | M1 linear | M2 quadratic | M3 natural spline (df = 4)
# Needs `analysis_data` (the main folder's 01_prepare_data.R) and `rao_ideol_df`
# (01_rao_with_ideology_scores.R). The sample is the same for all versions.
#
# How to read it: compare effect sizes and confidence intervals. Both versions are
# reported whichever looks stronger; the primary version was fixed in advance
# (FunRao_ideol_eq: news type and ideology equal weight).

RA_DIR <- "revision_analysis_ideology"
source("revision_analysis/helpers.R")
rm(RA_DIR)   # clear it so that the other folders' scripts are not sent to the wrong folder
stopifnot(exists("analysis_data"), exists("rao_ideol_df"))

d0 <- analysis_data
# gender: female vs not female (safe to run twice)
if (!(is.factor(d0$gender) && all(c("not female", "female") %in% levels(d0$gender)))) {
  d0$gender <- factor(ifelse(is.na(d0$gender), NA_character_, ifelse(d0$gender == 2, "female", "not female")),
                      levels = c("not female", "female"))
}
d0$psid_tracking <- as.character(d0$psid_tracking)
d0 <- dplyr::left_join(d0, rao_ideol_df, by = "psid_tracking")
stopifnot(!anyNA(d0$FunRao_ideol_eq), !anyNA(d0$FunRao_ideol_k1))
message("Sample: N = ", nrow(d0), " (identical for all versions)")

CONTROLS <- "age_z + gender + edu_z + log_unique_days + offline_z + offline_missing + wave"
mk <- function(rhs) as.formula(paste("Internal ~", rhs, "+", CONTROLS))
variants <- c(current = "FunRao", ideology_equal_weight = "FunRao_ideol_eq", ideology_k1 = "FunRao_ideol_k1")

res <- lapply(names(variants), function(v) {
  d <- d0
  d$rao_z <- zs(d[[variants[[v]]]])
  forms <- list(M0 = mk("1"), M1_linear = mk("rao_z"), M2_quadratic = mk("rao_z + I(rao_z^2)"),
                M3_spline = mk("ns(rao_z, df = 4)"))
  fits <- lapply(forms, lm, data = d)
  sd_y <- sd(d$Internal)
  fit <- data.frame(variant = v, model = names(fits),
                    adj_r2 = sapply(fits, function(m) summary(m)$adj.r.squared),
                    AIC = sapply(fits, AIC), BIC = sapply(fits, BIC),
                    cv_rmse = sapply(forms, cv_rmse, data = d), row.names = NULL)
  lin <- focal_term(fits$M1_linear, "rao_z", v)
  eff <- bind_rows(
    data.frame(variant = v, quantity = "linear: per SD of Rao", estimate = lin$estimate, lwr = lin$lwr, upr = lin$upr, p = lin$p),
    cbind(variant = v, quantity = "M1 linear: 10th vs 90th pct", setNames(contrast_pct(fits$M1_linear, d, "rao_z", .10, .90)[, c("diff", "lwr", "upr")], c("estimate", "lwr", "upr")), p = NA),
    cbind(variant = v, quantity = "M3 spline: 10th vs 90th pct", setNames(contrast_pct(fits$M3_spline, d, "rao_z", .10, .90)[, c("diff", "lwr", "upr")], c("estimate", "lwr", "upr")), p = NA))
  tests <- data.frame(variant = v,
    test = c("M0 vs M1 (linear term)", "M1 vs M2 (quadratic)", "M1 vs M3 (spline)"),
    p = c(anova(fits$M0, fits$M1_linear)[2, "Pr(>F)"], anova(fits$M1_linear, fits$M2_quadratic)[2, "Pr(>F)"],
          anova(fits$M1_linear, fits$M3_spline)[2, "Pr(>F)"]))
  list(fit = fit, eff = eff, tests = tests, d = d, fits = fits)
})
names(res) <- names(variants)

effects <- bind_rows(lapply(res, `[[`, "eff"))
fitstats <- bind_rows(lapply(res, `[[`, "fit"))
tests <- bind_rows(lapply(res, `[[`, "tests"))
cat("\n--- Effect of diversity on internal efficacy (0-20 scale) ---\n"); print(effects, digits = 3)
cat("\n--- Model fit ---\n");                                              print(fitstats, digits = 4)
cat("\n--- Nested tests (p-values) ---\n");                                print(tests, digits = 3)
write.csv(effects,  file.path(OUT_DIR, "comparison_effects.csv"),  row.names = FALSE)
write.csv(fitstats, file.path(OUT_DIR, "comparison_fit.csv"),      row.names = FALSE)
write.csv(tests,    file.path(OUT_DIR, "comparison_tests.csv"),    row.names = FALSE)

# Fitted spline curves along percentiles of diversity, one panel per version
pcts <- seq(.02, .98, by = .02)
curves <- bind_rows(lapply(names(res), function(v) {
  d <- res[[v]]$d
  cbind(variant = v, pct = pcts * 100,
        predict_curve(res[[v]]$fits$M3_spline, d, "rao_z", quantile(d$rao_z, pcts, names = FALSE)))
}))
p <- ggplot(curves, aes(pct, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = .2) + geom_line(linewidth = 1) +
  facet_wrap(~ variant) +
  labs(x = "Percentile of news-diet diversity", y = "Predicted internal efficacy (0-20)",
       caption = "Spline model (df = 4); controls held at their means / modal category; band = 95% CI (HC3).") +
  theme_minimal()
ggsave(file.path(OUT_DIR, "comparison_curves.png"), p, width = 10, height = 4, dpi = 300)

# Does the ideology-based score measure something different from the current one?
print(round(cor(d0[, unname(variants)]), 2))
