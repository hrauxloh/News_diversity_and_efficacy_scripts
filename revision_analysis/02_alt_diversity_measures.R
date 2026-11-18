# 02_alt_diversity_measures.R ---------------------------------------------
# Adds alternative / component diversity measures and exposure-volume variables
# to `analysis_data`. Needs `comm` (participant x domain visit counts) and
# `traits_mat` (dummy-coded outlet traits) from Full_pipeline_diversity_github.R.
# Addresses reviewer comments (3) and (7): what does the Rao measure capture
# beyond the number of outlets, and which trait drives the pattern?

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
suppressPackageStartupMessages(library(SYNCSA))
stopifnot(exists("analysis_data"), exists("comm"), exists("traits_mat"))

comm_m <- as.matrix(comm)
stopifnot(all(colnames(comm_m) %in% rownames(traits_mat)))

n_visits  <- rowSums(comm_m)
n_outlets <- rowSums(comm_m > 0)
p         <- comm_m / n_visits
shannon   <- -rowSums(ifelse(p > 0, p * log(p), 0))   # Shannon entropy (ignores outlet traits)
simpson   <- 1 - rowSums(p^2)                          # Gini-Simpson (Rao with all distances = 1)

# Rao restricted to ONE trait at a time. News type vs ideological orientation.
type_cols <- grep("^news_type",   colnames(traits_mat))
ori_cols  <- grep("^Orientation", colnames(traits_mat))
stopifnot(length(type_cols) > 0, length(ori_cols) > 0)

rao_type  <- rao.diversity(comm_m, traits = traits_mat[, type_cols, drop = FALSE],
                           standardize = TRUE)$FunRao
rao_ideol <- rao.diversity(comm_m, traits = traits_mat[, ori_cols, drop = FALSE],
                           standardize = TRUE)$FunRao

alt <- data.frame(psid_tracking = rownames(comm_m),
                  n_visits, n_outlets, shannon, simpson,
                  rao_type = as.numeric(rao_type[rownames(comm_m)]),
                  rao_ideol = as.numeric(rao_ideol[rownames(comm_m)]),
                  stringsAsFactors = FALSE)

analysis_data$psid_tracking <- as.character(analysis_data$psid_tracking)
analysis_data <- dplyr::left_join(analysis_data, alt, by = "psid_tracking")

analysis_data <- analysis_data %>%
  mutate(shannon_z   = zs(shannon),
         simpson_z   = zs(simpson),
         outlets_z   = zs(log(n_outlets)),
         log_visits  = log(n_visits),
         log_visits_z = zs(log(n_visits)),
         rao_type_z  = zs(rao_type),
         rao_ideol_z = zs(rao_ideol))

message("Share of participants with ideology-only Rao == 0 (no hyperpartisan left/right mix): ",
        round(mean(analysis_data$rao_ideol == 0, na.rm = TRUE), 3))
cors <- cor(analysis_data[, c("FunRao", "shannon", "simpson", "n_outlets",
                              "rao_type", "rao_ideol", "n_visits")],
            use = "pairwise.complete.obs")
print(round(cors, 2))
write.csv(round(cors, 3), file.path(OUT_DIR, "correlations_diversity_measures.csv"))
