# 03_main_models.R ---------------------------------------------------------
# Primary analysis: one pre-specified model sequence, fitted curve and contrast.
#   M0  controls only
#   M1  + linear diversity       (directional hypothesis: higher diversity, lower efficacy)
#   M2  + quadratic diversity    (direct test of the inverted-U implied by the theory)
#   M3  + natural cubic spline   (flexible check; read from the fitted curve, NOT coefficients)
# Responds to reviewer comments (1), (10) and (12).
# Run 01_prepare_data.R first.

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
stopifnot(exists("analysis_data"))
# Recode gender only if it is still the original numeric code (1/2/3). Script 01 already
# recodes it; running this on an already recoded factor would turn everyone into "not female".
if (!(is.factor(analysis_data$gender) && all(c("not female", "female") %in% levels(analysis_data$gender)))) {
  analysis_data$gender <- factor(
    ifelse(is.na(analysis_data$gender), NA_character_,
           ifelse(analysis_data$gender == 2, "female", "not female")),
    levels = c("not female", "female"))
}
table(analysis_data$gender)
d <- analysis_data

SPLINE_DF <- 4   # df = 4 puts knots at the 25th/50th/75th percentiles (as in the original models)
CONTROLS  <- "age_z + gender + edu_z + log_unique_days + offline_z + offline_missing + wave"   # wave = dummy for efficacy survey wave (W4 vs W1)
mk <- function(rhs) as.formula(paste("Internal ~", rhs, "+", CONTROLS))

forms <- list(
  M0_controls  = mk("1"),
  M1_linear    = mk("rao_z"),
  M2_quadratic = mk("rao_z + I(rao_z^2)"),
  M3_spline    = mk(sprintf("ns(rao_z, df = %d)", SPLINE_DF))
)
fits <- lapply(forms, lm, data = d)

# Diversity-only model (no controls): the unadjusted association, shown next to the fully controlled one
# (reviewer request). Kept outside `fits` so the curves and contrasts below are unchanged.
form_unadj <- as.formula("Internal ~ rao_z")
fit_unadj  <- lm(form_unadj, data = d)

# --- Model equation (for the Methods) ------------------------------------
# Internal_i = b0 + f(Rao_i) + g1*Age_i + g2*Gender_i + g3*Education_i
#              + g4*log(TrackingDays_i) + g5*OfflineNews_i + g6*NoOfflineData_i + g7*Wave4_i + e_i
# where f() is linear (M1), quadratic (M2) or a natural cubic spline with
# 3 interior knots at the quartiles of Rao (M3); Rao and continuous controls are z-scored.

# --- Model comparison -----------------------------------------------------
cmp <- data.frame(
  model   = names(fits),
  n_par   = sapply(fits, function(m) length(coef(m)) + 1),
  adj_r2  = sapply(fits, function(m) summary(m)$adj.r.squared),
  AIC     = sapply(fits, AIC),
  BIC     = sapply(fits, BIC),
  cv_rmse = sapply(forms, cv_rmse, data = d),
  row.names = NULL
)
cmp <- rbind(cmp, data.frame(
  model = "M_diversity_only", n_par = length(coef(fit_unadj)) + 1,
  adj_r2 = summary(fit_unadj)$adj.r.squared, AIC = AIC(fit_unadj), BIC = BIC(fit_unadj),
  cv_rmse = cv_rmse(form_unadj, d)))
print(cmp, digits = 4)
write.csv(cmp, file.path(OUT_DIR, "model_comparison.csv"), row.names = FALSE)

nested <- rbind(
  data.frame(test = "M0 vs M1 (linear term)",    as.data.frame(anova(fits$M0_controls, fits$M1_linear))[2, c("F", "Pr(>F)")]),
  data.frame(test = "M1 vs M2 (quadratic term)", as.data.frame(anova(fits$M1_linear, fits$M2_quadratic))[2, c("F", "Pr(>F)")]),
  data.frame(test = "M1 vs M3 (spline terms)",   as.data.frame(anova(fits$M1_linear, fits$M3_spline))[2, c("F", "Pr(>F)")])
)
print(nested, digits = 4)
write.csv(nested, file.path(OUT_DIR, "nested_tests.csv"), row.names = FALSE)

# --- Robust (HC3) coefficients -------------------------------------------
coef_all <- dplyr::bind_rows(lapply(names(fits), function(nm) cbind(model = nm, robust_table(fits[[nm]]))))
coef_all <- dplyr::bind_rows(coef_all, cbind(model = "M_diversity_only", robust_table(fit_unadj)))
write.csv(coef_all, file.path(OUT_DIR, "coefficients_all_models_robust.csv"), row.names = FALSE)   # for Table 1
print(coef_all, digits = 3)
write.csv(robust_table(fits$M1_linear),    file.path(OUT_DIR, "coefs_M1_linear_robust.csv"),    row.names = FALSE)
write.csv(robust_table(fits$M2_quadratic), file.path(OUT_DIR, "coefs_M2_quadratic_robust.csv"), row.names = FALSE)
write.csv(robust_table(fit_unadj), file.path(OUT_DIR, "coefs_M_diversity_only_robust.csv"), row.names = FALSE)
message("Diversity only (no controls): b = ", round(coef(fit_unadj)["rao_z"], 3),
        ", robust p = ", signif(robust_table(fit_unadj)$`Pr(>|t|)`[2], 3))

# Turning point of the quadratic (in SDs of Rao) and whether it falls inside the data
b <- coef(fits$M2_quadratic)
tp <- unname(-b["rao_z"] / (2 * b["I(rao_z^2)"]))
message("Quadratic turning point: ", round(tp, 2), " SD; observed range: ",
        paste(round(range(d$rao_z), 2), collapse = " to "))

# --- Fitted curves along percentiles of diversity -------------------------
pcts <- seq(.02, .98, by = .02)
vals <- quantile(d$rao_z, pcts, names = FALSE)
curves <- bind_rows(lapply(names(fits)[-1], function(nm) {
  cbind(model = nm, pct = pcts * 100, predict_curve(fits[[nm]], d, "rao_z", vals))
}))
write.csv(curves, file.path(OUT_DIR, "fitted_curves.csv"), row.names = FALSE)

p <- ggplot(curves, aes(pct, fit, colour = model, fill = model)) +
  geom_ribbon(data = subset(curves, model == "M3_spline"), aes(ymin = lwr, ymax = upr),
              alpha = .15, colour = NA) +
  geom_line(linewidth = 1) +
  geom_rug(data = d, aes(x = 100 * ecdf(rao_z)(rao_z)), inherit.aes = FALSE,
           sides = "b", alpha = .15) +
  labs(x = "Percentile of news-diet diversity (Rao)",
       y = "Predicted internal efficacy (0-20)",
       caption = "Controls held at their means / modal category. Band = 95% CI (HC3) for the spline.") +
  theme_minimal()
ggsave(file.path(OUT_DIR, "fitted_curves.png"), p, width = 8, height = 5, dpi = 300)

# --- Pre-specified contrasts (estimand) -----------------------------------
sd_y <- sd(d$Internal)
contrasts <- bind_rows(lapply(names(fits)[-1], function(nm) {
  cbind(model = nm, bind_rows(
    contrast_pct(fits[[nm]], d, "rao_z", .10, .90, sd_outcome = sd_y),  # PRIMARY estimand
    contrast_pct(fits[[nm]], d, "rao_z", .25, .75, sd_outcome = sd_y),
    contrast_pct(fits[[nm]], d, "rao_z", .75, .95, sd_outcome = sd_y)))
}))
print(contrasts, digits = 3)
write.csv(contrasts, file.path(OUT_DIR, "contrasts.csv"), row.names = FALSE)

# --- Sensitivity of M3 to the number of spline df -------------------------
df_sens <- bind_rows(lapply(2:6, function(k) {
  f <- mk(sprintf("ns(rao_z, df = %d)", k)); m <- lm(f, data = d)
  cbind(df = k, AIC = AIC(m), BIC = BIC(m), cv_rmse = cv_rmse(f, d),
        contrast_pct(m, d, "rao_z", .10, .90, sd_outcome = sd_y))
}))
print(df_sens, digits = 3)
write.csv(df_sens, file.path(OUT_DIR, "spline_df_sensitivity.csv"), row.names = FALSE)

# --- Descriptives of the diversity measure (Methods / Table) --------------
qs <- quantile(d$FunRao, c(.25, .5, .75))
quart <- cut(d$FunRao, c(-Inf, qs, Inf), labels = paste0("Q", 1:4))
desc <- data.frame(mean = mean(d$FunRao), sd = sd(d$FunRao), min = min(d$FunRao),
                   q25 = qs[[1]], median = qs[[2]], q75 = qs[[3]], max = max(d$FunRao),
                   n = nrow(d))
print(desc, digits = 3); print(table(quart))
write.csv(desc, file.path(OUT_DIR, "rao_descriptives.csv"), row.names = FALSE)
write.csv(as.data.frame(table(quartile = quart)), file.path(OUT_DIR, "rao_quartile_counts.csv"), row.names = FALSE)

# --- Inter-item correlation of the two efficacy items (comment 9) ----------
r <- cor(d$poleffic2, d$poleffic4, use = "complete.obs")   # the two internal-efficacy items
message("Inter-item correlation r = ", round(r, 2), "; Spearman-Brown (2-item alpha) = ",
        round(2 * r / (1 + r), 2))
