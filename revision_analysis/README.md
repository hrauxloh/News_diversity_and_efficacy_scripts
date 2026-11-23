# Revision analysis (proposed restructure)

Proposed re-analysis responding to the reviewer comments on the manuscript
"The more news sources, the better for democracy?". It **adds** a folder; the
original scripts are unchanged.

> **Status: written but not yet run.** The scripts were drafted without access to the
> data or to an R installation, so they have not been executed. Expect small
> errors on first run (column names, object names). Please run and fix before use.

## Important correction: which items make up internal efficacy
`poleffic1` is an **external** efficacy item ("how much does the political system give people like
you a say"). The old code defined `Internal = poleffic1 + poleffic2`, which mixes an external and an
internal item. `00_import_clean_survey.R` defines `Internal = poleffic2 + poleffic4` (to be verified
against the codebook wording of the two items), drops External, and shifts the raw 1-11 codes to 0-10
(so Internal runs 0-20). **All earlier results (Table 1 etc.) used the old outcome and must be re-run.**

## 3-second threshold (new)
A visit now counts if it lasted at least **3 active seconds** (original analysis: 2). `00b_get_tracking_data.R`
downloads the visits with this threshold (server access and file paths are placeholders to fill in; the password is
read from an environment variable), builds `comm` and `traits_mat`, tags `comm` with the threshold and saves them
outside the repository. `01_prepare_data.R` stops if `comm` does not carry the 3-second tag, so old 2-second objects
cannot be reused by mistake. Order of the run: `00b` -> `01` -> `02` -> `03` -> `04`
(`rm(list = ls())` first, then keep only what 01 needs: `total_tracking_activity_0_total_june2025`, `mapping_consolidated`).
`unique_days` comes from `total_tracking_activity_0_total_june2025`; check whether that file was built with the
2-second filter (see the comment in 00b).

## How to run
1. Re-import the survey with `00_import_clean_survey.R` (needs your `con_survey` and
   `SQL_query_function`) and save `survey_data_v2.Rds` outside the repository.
2. Run the existing pipeline (`Full_pipeline_diversity_github.R`) **only up to the Rao scores**
   (about line 102), so that `rao_scores_df`, `total_tracking_activity_0_total_june2025`, `comm` and
   `traits_mat` exist in the session (and `mapping_consolidated`, or the file `mapping_consolidated.rds`).
   The survey part of the old pipeline is no longer used.
3. `01_prepare_data.R` then reads `survey_data_v2.Rds` (set `SURVEY_V2 <- "path/to/survey_data_v2.Rds"`
   first if it is not in the working directory), joins survey, tracking and Rao data into `full_data_sample`,
   and builds `analysis_data`. Then, from the repository root:

```r
source("revision_analysis/01_prepare_data.R")
source("revision_analysis/02_alt_diversity_measures.R")
source("revision_analysis/03_main_models.R")
source("revision_analysis/04_wave_and_robustness.R")
```
Outputs (tables and figures) go to `revision_analysis/output/` and are git-ignored.
Packages: dplyr, splines, ggplot2, sandwich, lmtest, SYNCSA.

## What changed and why
| Reviewer comment | Change |
|---|---|
| (1) hypotheses vs results | One primary model sequence: M0 controls, M1 linear, M2 quadratic (tests the inverted U directly, with turning point), M3 natural spline (flexible check). One pre-specified estimand: predicted efficacy at the 90th vs the 10th percentile of diversity. |
| (10) spline interpretation | Results are read from the fitted curve and contrasts with HC3 CIs (`fitted_curves.png`, `contrasts.csv`), never from individual spline coefficients. The model equation is written in `03_main_models.R`. |
| (12) spline vs linear | Nested F-tests, AIC, BIC and repeated 10-fold cross-validated RMSE for all models (`model_comparison.csv`, `nested_tests.csv`); sensitivity to spline df 2 to 6. |
| (9) wave and timing | A wave dummy (W4 vs W1) is included in every model. Efficacy is treated as stable over time, so there are **no** wave-specific models, no diversity x wave interaction and no timing-based restriction of the tracking data. Only about 6% of the sample (44 of 742 with efficacy data) answered in wave 4. `wave_dummy_check.csv` shows whether adding the dummy changes the diversity estimate. The inter-item correlation of the two efficacy items is printed by script 03. |
| (3), (7) what the measure captures | Shannon, Gini-Simpson, number of outlets, and Rao split into news-type-only and ideology-only versions; joint models and Rao controlling for Shannon plus outlet count (`alternative_measures.csv`). |
| Minimum visits | Section E of script 04 repeats the analysis with participants needing more than 10, 30 (main), 50 or 100 qualifying visits (`min_visits_sensitivity.csv`); needs `tracking_data_0` and `domain_attributes` from 00b. |
| Forking paths | The three "samples" become coverage restrictions in an appendix-style table (`coverage_restrictions.csv`) instead of choosing the best-performing sample. |

## Things in the existing code worth checking
1. **`age` looks like birth year** (e.g. 1958 in `head(full_data_sample)`). The original models standardise
   it as is, so the "Age" coefficient (+0.44) would then mean *later birth year = higher efficacy*, i.e. the
   opposite of an age effect. `01_prepare_data.R` converts it to age in years when all values exceed 1900.
2. **`log_unique_days` is the log of the number of unique tracking days**, but the manuscript and Table 1
   call it "log of total visits". `02_alt_diversity_measures.R` adds true visit counts and outlet counts.
3. **Wave is not in any model** in the original scripts (added here as a dummy; see above).
4. **The 2-second visit filter** mentioned in the manuscript does not appear in these scripts.
5. The Orientation dummies use `Center` as the reference level, so Left vs Right is twice as far apart
   (two columns differ) as Center vs Left or Right (one column). State this in the Methods or change the coding.
6. `social_freq` and `socialmedia_freq` are assumed to be interpersonal political talk and social media use,
   inferred from the column names. Check the questionnaire.

## Saving the output so it can be shared
Run a script with `run_logged()` instead of `source()`; it saves everything the script prints to
`revision_analysis/output/log_<script>.txt` (and the package versions, e.g. of SYNCSA):

```r
source("revision_analysis/run_logged.R")
run_logged("revision_analysis/01_prepare_data.R")
run_logged("revision_analysis/03_main_models.R")
run_logged("revision_analysis/04_wave_and_robustness.R")
```
The csv tables the scripts write (model comparison, coefficients of all models, contrasts, robustness) are in the same
output folder. No data set is saved. Check the files before sharing them.
