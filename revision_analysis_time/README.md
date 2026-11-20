# Revision analysis with time spent (version 3)

Parallel version of `revision_analysis/` in which the abundance of each outlet in a participant's
news diet is the **time spent** (seconds of active time) and not the number of visits. Traits (news type
and the Left / Center / Right orientation) are unchanged. The main folder is **not changed**.

> Written without access to the data and **not yet run**.

## Needs in the R session
- `con_tracking` (open connection), `domain_attributes`, `comm` (visit counts), `traits_mat`
- `analysis_data` (from `revision_analysis/01_prepare_data.R`, N = 716) and optionally `rao_scores_df`

## Run
```r
source("revision_analysis_time/00_build_time_matrices.R")   # queries the database, builds the seconds matrices
source("revision_analysis_time/01_rao_with_time.R")
source("revision_analysis_time/02_compare_models.R")
```

## Choices fixed in advance
- Same filters as the main analysis (news deep links, at least 2 active seconds, classified political).
- **Primary version:** each visit capped at 600 s (about the 99th percentile of `active_seconds`, 616 s).
  A few visits last hours (maximum 83,125 s), probably tabs left open.
- **Sensitivity version:** no cap.
- Same sample (N = 716) for all versions, so the comparison is like for like.

## Caution
Time and visits are usually strongly related, so similar results are expected. Report all versions
whichever is stronger.
