# 01_rao_with_time.R ---------------------------------------------------------
# Rao score with TIME SPENT as the abundance of each outlet (instead of the number of visits).
# Traits are unchanged: news type and the Left / Center / Right orientation of the main analysis.
#   FunRao_time_cap  PRIMARY: seconds, each visit capped at 600 s
#   FunRao_time_raw  SENSITIVITY: seconds, no cap
# Needs: comm_seconds_capped, comm_seconds_raw (00_build_time_matrices.R), traits_mat.

RA_DIR <- "revision_analysis_time"
source("revision_analysis/helpers.R")
rm(RA_DIR)   # clear it so that the other folders' scripts are not sent to the wrong folder
suppressPackageStartupMessages(library(SYNCSA))
stopifnot(exists("comm_seconds_capped"), exists("comm_seconds_raw"), exists("traits_mat"))

rao_cap <- rao.diversity(comm_seconds_capped, traits = traits_mat, standardize = TRUE)$FunRao
rao_raw <- rao.diversity(comm_seconds_raw,    traits = traits_mat, standardize = TRUE)$FunRao

rao_time_df <- data.frame(
  psid_tracking   = rownames(comm_seconds_capped),
  FunRao_time_cap = as.numeric(rao_cap[rownames(comm_seconds_capped)]),
  FunRao_time_raw = as.numeric(rao_raw[rownames(comm_seconds_capped)]),
  stringsAsFactors = FALSE)
print(summary(rao_time_df[, -1]))

if (exists("rao_scores_df")) {
  m <- merge(rao_time_df, setNames(rao_scores_df[, 1:2], c("psid_tracking", "FunRao_views")), by = "psid_tracking")
  message("\nCorrelation of the Rao scores (views vs time):")
  print(round(cor(m[, c("FunRao_views", "FunRao_time_cap", "FunRao_time_raw")]), 2))
}
write.csv(rao_time_df[, -1], file.path(OUT_DIR, "rao_time_scores_NO_IDS.csv"), row.names = FALSE)
