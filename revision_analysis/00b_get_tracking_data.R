# 00b_get_tracking_data.R ---------------------------------------------------
# Downloads the web-tracking visits that enter the diversity measure and builds the objects
# the later scripts need: tracking_data_0, comm (participant x outlet visit counts) and traits_mat.
# It replaces the commented-out "Tracking Data" block of the original pipeline.
#
# CHANGE FROM THE ORIGINAL ANALYSIS: a visit now counts only if it lasted at least
# MIN_ACTIVE_SECONDS = 3 active seconds (before: 2). Everything else is unchanged:
# news deep links, domains on the outlet list, classified as political (is_pol_ml = 'political'),
# No minimum number of visits is applied here: comm_all contains every participant with at least one
# qualifying visit. The minimum (main analysis: more than 30 visits) is applied in 01_prepare_data.R, so the
# robustness checks with other minimums (script 04, section E) can use the same data.
#
# Fill in the PLACEHOLDERS below. No password is written in this file: it is read from an
# environment variable (set it once with usethis::edit_r_environ()).
# Nothing is written inside the repository; the downloaded visits are participant-level data.
# Run from the repository root.

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "rao_own.R"))
library(DBI)

# ---- Settings ---------------------------------------------------------------
MIN_ACTIVE_SECONDS <- 3     # visit threshold (was 2)

# PLACEHOLDERS: server access ---------------------------------------------------
DB_NAME <- "<DATABASE_NAME>"            # e.g. the tracking database
DB_HOST <- "<SERVER_HOST_OR_IP>"
DB_PORT <- 5432                         # <PORT> if different
DB_USER <- "<READ_ONLY_USER>"
DB_PWD  <- Sys.getenv("<NAME_OF_PASSWORD_ENV_VARIABLE>")   # e.g. Sys.getenv("poltrack_pwd")

# PLACEHOLDERS: local files (keep them OUTSIDE the repository) ------------------
DOMAINS_FILE            <- "<PATH/TO/GONDv3_domains.rds>"                 # data frame with a column `domain`
DOMAIN_ATTRIBUTES_FILE  <- "<PATH/TO/domain_attributes_0_june2025.csv>"   # domain, news_type, Orientation
TRACKING_OUT_DIR        <- "<PATH/OUTSIDE/REPO/tracking_3s>"              # where the results are saved

stopifnot(!grepl("^<", c(DB_NAME, DB_HOST, DB_USER, DOMAINS_FILE, DOMAIN_ATTRIBUTES_FILE, TRACKING_OUT_DIR)))
stopifnot(nzchar(DB_PWD))
dir.create(TRACKING_OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- Connect ------------------------------------------------------------------
con_tracking <- dbConnect(RPostgres::Postgres(), dbname = DB_NAME, host = DB_HOST,
                          port = DB_PORT, user = DB_USER, password = DB_PWD)
print(dbListTables(con_tracking))

domains_df <- readRDS(DOMAINS_FILE)
domains <- paste(dbQuoteString(con_tracking, trimws(domains_df$domain)), collapse = ",")

# ---- Query ----------------------------------------------------------------------
# Same query as before; only the threshold changes.
query <- paste0("
  SELECT v.*
  FROM views_new v
  JOIN content_new c
    ON v.url_hash = c.url_hash
  WHERE domain IN (", domains, ")
    AND v.is_news_deep_link = TRUE
    AND v.active_seconds >= ", MIN_ACTIVE_SECONDS, "
    AND c.is_pol_ml = 'political'
")
tracking_data_0 <- dbGetQuery(con_tracking, query)
colnames(tracking_data_0)[1] <- "psid_tracking"          # first column is panelist_id
tracking_data_0$psid_tracking <- as.character(tracking_data_0$psid_tracking)

# ---- For the Methods / reviewers: how many visits does the stricter threshold remove? ----
count_at <- function(sec) {
  q <- paste0("SELECT COUNT(*)::float8 AS n FROM views_new v JOIN content_new c ON v.url_hash = c.url_hash
               WHERE domain IN (", domains, ") AND v.is_news_deep_link = TRUE
               AND v.active_seconds >= ", sec, " AND c.is_pol_ml = 'political'")
  dbGetQuery(con_tracking, q)$n
}
n2 <- count_at(2); n3 <- count_at(3)
message("Visits with >= 2 s: ", n2, "; with >= 3 s: ", n3, " (", round(100 * (1 - n3 / n2), 1), "% removed)")
stopifnot(abs(n3 - nrow(tracking_data_0)) < 0.5, min(tracking_data_0$active_seconds) >= MIN_ACTIVE_SECONDS)
dbDisconnect(con_tracking)

# ---- Participant x outlet matrix and traits (as in the original pipeline) -----------
domain_attributes <- read.csv(DOMAIN_ATTRIBUTES_FILE, stringsAsFactors = FALSE)
ct <- build_comm_traits(tracking_data_0, domain_attributes, min_views = 0)   # 0 = everyone with >= 1 visit
comm_all <- ct$comm; traits_mat <- ct$traits_mat
attr(comm_all, "min_active_seconds") <- MIN_ACTIVE_SECONDS     # lets script 01 detect stale 2-second objects
comm <- comm_all
vis <- rowSums(comm_all)
message("Participants with at least 1 visit: ", sum(vis >= 1), "; more than 10: ", sum(vis > 10),
        "; more than 30 (main analysis): ", sum(vis > 30), "; visits in total: ", sum(comm_all),
        "; outlets: ", ncol(comm_all))
message("(2-second analysis for comparison: 1,220 participants, 433,831 visits)")

# ---- Save (outside the repository) -----------------------------------------------------
saveRDS(tracking_data_0, file.path(TRACKING_OUT_DIR, "tracking_data_0_3s.rds"))
saveRDS(comm_all,        file.path(TRACKING_OUT_DIR, "comm_all_3s.rds"))
saveRDS(traits_mat,      file.path(TRACKING_OUT_DIR, "traits_mat.rds"))
saveRDS(domain_attributes, file.path(TRACKING_OUT_DIR, "domain_attributes.rds"))
message("Saved to ", TRACKING_OUT_DIR)

# NOTE: `unique_days` (days with tracked activity) is read from total_tracking_activity_0_total_june2025 in
# script 01. If that file counts days with ANY tracking activity, the threshold does not affect it. If it
# counts days with qualifying news visits (>= 2 s), it must be rebuilt with the same threshold.
