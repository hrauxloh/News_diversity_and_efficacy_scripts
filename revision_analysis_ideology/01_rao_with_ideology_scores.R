# 01_rao_with_ideology_scores.R ---------------------------------------------
# Recomputes the Rao diversity score using the coded ideology scores of the outlets
# (instead of the old Left / Center / Right dummies for hyperpartisan outlets only).
#
# Needs in the R session:
#   comm                       participant x domain visit counts (from the pipeline)
#   traits_mat                 dummy-coded outlet traits (from the pipeline)
#   outlet_ideology_imputed    table with columns `domain` and `score_imputed`
#                              (score -2..+2; outlets without a score set to 0 = centre)
#
# What changes compared with the current measure
#   * news type: unchanged (same dummy columns as before)
#   * ideology:  the two Orientation dummies are replaced by ONE numeric column,
#                the coded score (-2 = left ... +2 = right).
#                The Gower distance on this column is |score_i - score_j| / 4.
# Two versions (the number of columns the ideology score is repeated in the trait matrix
# sets how much it counts relative to news type; a different news type adds a distance of
# 2 dummy columns, so repeating the ideology column twice gives both traits equal weight):
#   FunRao_ideol_eq  PRIMARY: ideology repeated twice  (news type and ideology equal weight)
#   FunRao_ideol_k1  SENSITIVITY: ideology once        (ideology counts half as much)
# Output: `rao_ideol_df` (participant id + the two scores) and tables in
#   revision_analysis_ideology/output/

RA_DIR <- "revision_analysis_ideology"
source("revision_analysis/helpers.R")        # the helper functions of the main folder; outputs go to RA_DIR/output
suppressPackageStartupMessages(library(SYNCSA))
stopifnot(exists("comm"), exists("traits_mat"), exists("outlet_ideology_imputed"))
stopifnot(all(c("domain", "score_imputed") %in% names(outlet_ideology_imputed)))

comm_m <- as.matrix(comm)
type_cols <- grep("^news_type", colnames(traits_mat))
stopifnot(length(type_cols) > 0, all(colnames(comm_m) %in% rownames(traits_mat)))

ideol <- outlet_ideology_imputed$score_imputed[match(rownames(traits_mat), outlet_ideology_imputed$domain)]
message("Outlets in traits_mat without an ideology score in the table: ", sum(is.na(ideol)),
        " (set to 0, as for the other uncoded outlets)")
ideol[is.na(ideol)] <- 0
message("Ideology scores in traits_mat: ", paste(names(table(ideol)), table(ideol), sep = "=", collapse = ", "))

make_traits <- function(k) {
  cbind(traits_mat[, type_cols, drop = FALSE],
        matrix(rep(ideol, k), ncol = k,
               dimnames = list(rownames(traits_mat), paste0("ideology", seq_len(k)))))
}
rao_with <- function(traits) rao.diversity(comm_m, traits = traits, standardize = TRUE)$FunRao

rao_eq <- rao_with(make_traits(2))      # primary
rao_k1 <- rao_with(make_traits(1))      # sensitivity
rao_id <- rao_with(matrix(ideol, ncol = 1, dimnames = list(rownames(traits_mat), "ideology")))   # ideology only

rao_ideol_df <- data.frame(
  psid_tracking   = rownames(comm_m),
  FunRao_ideol_eq = as.numeric(rao_eq[rownames(comm_m)]),
  FunRao_ideol_k1 = as.numeric(rao_k1[rownames(comm_m)]),
  rao_ideology_only = as.numeric(rao_id[rownames(comm_m)]),
  stringsAsFactors = FALSE)

message("\nShare of participants with ideology-only Rao == 0 under the new coding: ",
        round(mean(rao_ideol_df$rao_ideology_only == 0), 3), "  (old coding: 0.816)")
print(summary(rao_ideol_df[, -1]))

if (exists("rao_scores_df")) {
  m <- merge(rao_ideol_df, setNames(rao_scores_df[, 1:2], c("psid_tracking", "FunRao_old")), by = "psid_tracking")
  message("\nCorrelation with the current Rao score:")
  print(round(cor(m[, c("FunRao_old", "FunRao_ideol_eq", "FunRao_ideol_k1", "rao_ideology_only")]), 2))
}
write.csv(rao_ideol_df[, c("FunRao_ideol_eq", "FunRao_ideol_k1", "rao_ideology_only")],
          file.path(OUT_DIR, "rao_ideol_scores_NO_IDS.csv"), row.names = FALSE)
