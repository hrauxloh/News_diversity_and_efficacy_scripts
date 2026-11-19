# Revision analysis with coded outlet ideology (version 2)

A parallel version of `revision_analysis/` in which the ideological trait of the Rao score comes from
the coded outlet ideology scores (-2 left ... +2 right) instead of the Left / Center / Right dummies
for hyperpartisan outlets only. The first folder is **not changed**; outputs go to
`revision_analysis_ideology/output/`.

> Written without access to the data and **not yet run**.

## Needs in the R session
- `analysis_data`: from `revision_analysis/01_prepare_data.R` (current Rao, same sample N = 716)
- `comm`, `traits_mat`: from the pipeline (the participant x domain counts and outlet traits)
- `outlet_ideology_imputed`: table with columns `domain` and `score_imputed`
  (`score_imputed` = the coded score; outlets without a score (NK1, NK2, NP, NO) set to 0)
- optional `rao_scores_df`, to print the correlation with the current Rao score

## Run
```r
source("revision_analysis_ideology/01_rao_with_ideology_scores.R")
source("revision_analysis_ideology/02_compare_models.R")
```

## What is compared
| Version | Ideology in the trait matrix |
|---|---|
| `current` | two Orientation dummies (Left, Right); Center is the reference |
| `ideology_equal_weight` (**primary**) | the coded score as one numeric column repeated twice, so news type and ideology count equally |
| `ideology_k1` (sensitivity) | the coded score once; ideology counts about half as much as news type |

For each version: models M0 (controls), M1 (linear), M2 (quadratic) and M3 (spline, df = 4), with
effect per SD of Rao, the 10th-vs-90th-percentile contrasts, AIC, BIC, cross-validated error, nested
tests and fitted curves. The sample and the other variables are identical.

## Cautions
- **Setting uncoded outlets to 0** treats about a quarter of visits (NK1, NK2) and non-political sites (NP)
  as ideologically centrist. That shrinks ideological distances and is an assumption, not a finding.
  A version that leaves them out of the ideology trait (distance on news type only) is the more
  cautious alternative.
- The coding is a **first pass** by one (AI-assisted) coder and has not been validated.
- Choose the primary version before looking at the efficacy results and report both. Do not pick
  the version with the strongest effect.
