# 00_build_time_matrices.R ---------------------------------------------------
# Builds participant x domain matrices of TIME SPENT (seconds), with the same rows and
# columns as `comm` (visit counts), so that the Rao score can be computed with time as
# the measure of abundance instead of the number of visits.
#
# Needs in the R session: con_tracking (open connection), domain_attributes, comm.
# Creates: comm_seconds_capped (primary), comm_seconds_raw (sensitivity).
#
# Choices fixed in advance:
#   * same filters as the main analysis: news deep links, at least 2 active seconds,
#     classified as political (is_pol_ml = 'political')
#   * the time of a single visit is capped at CAP_SECONDS (600 s = about the 99th percentile
#     of active_seconds, 616 s) in the primary version, because a few visits last hours
#     (maximum 83,125 s), probably tabs left open. The uncapped version is a sensitivity analysis.

library(dplyr); library(tidyr); library(tibble); library(DBI)
stopifnot(exists("con_tracking"), exists("domain_attributes"), exists("comm"))
CAP_SECONDS <- 600

domains <- paste(DBI::dbQuoteString(con_tracking, trimws(domain_attributes$domain)), collapse = ",")
query_time <- paste0("
  SELECT v.panelist_id AS psid_tracking, v.domain,
         COUNT(*)                                   AS n_views,
         SUM(v.active_seconds)                      AS total_seconds,
         SUM(LEAST(v.active_seconds, ", CAP_SECONDS, ")) AS total_seconds_capped
  FROM views_new v
  JOIN content_new c ON v.url_hash = c.url_hash
  WHERE v.domain IN (", domains, ")
    AND v.is_news_deep_link = TRUE
    AND v.active_seconds >= 2
    AND c.is_pol_ml = 'political'
  GROUP BY 1, 2")
tm <- dbGetQuery(con_tracking, query_time)
tm$psid_tracking <- as.character(tm$psid_tracking)
tm <- tm %>% filter(psid_tracking %in% rownames(comm))

to_matrix <- function(col) {
  m <- tm %>% select(psid_tracking, domain, all_of(col)) %>%
    pivot_wider(names_from = domain, values_from = all_of(col), values_fill = 0) %>%
    column_to_rownames("psid_tracking")
  as.matrix(m[rownames(comm), colnames(comm)])
}
comm_views_check    <- to_matrix("n_views")
comm_seconds_raw    <- to_matrix("total_seconds")
comm_seconds_capped <- to_matrix("total_seconds_capped")

# Checks (all should be TRUE)
message("Visit counts identical to comm:        ", isTRUE(all(comm_views_check == as.matrix(comm))))
message("Same participant x domain cells used:  ", isTRUE(all((comm_seconds_raw > 0) == (as.matrix(comm) > 0))))
message("No participant with zero time:         ", all(rowSums(comm_seconds_capped) > 0))
message("Share of seconds removed by the cap:   ", round(1 - sum(comm_seconds_capped) / sum(comm_seconds_raw), 3))
print(summary(rowSums(comm_seconds_raw) / 60))     # minutes of political news time per participant
