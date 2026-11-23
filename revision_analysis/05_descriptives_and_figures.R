# 05_descriptives_and_figures.R ---------------------------------------------
# Table 2 (summary by Rao quartile) and the figures for the manuscript.
# Run after 01 and 02 (02 adds n_visits and n_outlets to analysis_data). Figure 3 (fitted curves)
# is made by 03_main_models.R (output/fitted_curves.png).
#   Figure 1: distribution of internal efficacy  -> output/fig1_efficacy_distribution.png
#   Figure S: distribution of the Rao score      -> output/fig_rao_distribution.png
#   Table 2 : by Rao quartile                    -> output/table2_by_quartile.csv

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
stopifnot(exists("analysis_data"), all(c("n_visits", "n_outlets") %in% names(analysis_data)))
d <- analysis_data
library(ggplot2)

# ---- Table 2: summary by quartile of Rao ---------------------------------------
qs <- quantile(d$FunRao, c(.25, .5, .75))
d$quartile <- cut(d$FunRao, c(-Inf, qs, Inf), labels = paste0("Q", 1:4), right = TRUE)
sm <- function(x) c(mean = mean(x), sd = sd(x), median = median(x))
table2 <- do.call(rbind, lapply(split(d, d$quartile), function(g) {
  data.frame(quartile = g$quartile[1], n = nrow(g),
             rao_min = min(g$FunRao), rao_max = max(g$FunRao),
             visits_min = min(g$n_visits), visits_max = max(g$n_visits),
             visits_mean = mean(g$n_visits), visits_sd = sd(g$n_visits), visits_median = median(g$n_visits),
             outlets_min = min(g$n_outlets), outlets_max = max(g$n_outlets),
             outlets_mean = mean(g$n_outlets), outlets_sd = sd(g$n_outlets), outlets_median = median(g$n_outlets),
             efficacy_mean = mean(g$Internal), efficacy_sd = sd(g$Internal))
}))
print(table2, digits = 3, row.names = FALSE)
write.csv(table2, file.path(OUT_DIR, "table2_by_quartile.csv"), row.names = FALSE)
cat("Quartile cut-points of Rao:", round(qs, 3), "\n")

# ---- Figure 1: distribution of internal efficacy --------------------------------
f1 <- ggplot(d, aes(Internal)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, boundary = -0.5,
                 fill = "grey70", colour = "white") +
  geom_density(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, 2), limits = c(-0.5, 20.5)) +
  labs(x = "Internal political efficacy (0-20)", y = "Density",
       caption = paste0("N = ", nrow(d))) +
  theme_minimal()
ggsave(file.path(OUT_DIR, "fig1_efficacy_distribution.png"), f1, width = 7, height = 4.5, dpi = 300)

# ---- Distribution of the Rao diversity score --------------------------------------
f_rao <- ggplot(d, aes(FunRao)) +
  geom_histogram(bins = 30, fill = "grey70", colour = "white") +
  geom_vline(xintercept = qs, linetype = "dashed") +
  labs(x = "Diversity of news diet (Rao's quadratic entropy)", y = "Participants",
       caption = "Dashed lines: quartile cut-points") +
  theme_minimal()
ggsave(file.path(OUT_DIR, "fig_rao_distribution.png"), f_rao, width = 7, height = 4.5, dpi = 300)
message("Saved Table 2 and the figures to ", OUT_DIR)
