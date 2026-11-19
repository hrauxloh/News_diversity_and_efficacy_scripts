# 01_prepare_data.R -------------------------------------------------------
# Reads the re-imported survey (survey_data_v2.Rds, made by 00_import_clean_survey.R),
# joins it to the tracking data and Rao scores, and builds `analysis_data`.
# It REPLACES the survey part of Full_pipeline_diversity_github.R, so run that pipeline
# only up to the point where `rao_scores_df` has been created (about line 102).
# Run from the repository root.
#
# Needed in the R session before running:
#   rao_scores_df                               (participant id + FunRao, from the pipeline)
#   total_tracking_activity_0_total_june2025    (psid_tracking, unique_days, first_day, last_day, ...)
#   mapping_consolidated                        (survey id <-> tracking id), or the file
#                                               mapping_consolidated.rds in the working directory
# Optional: psid_to_remove (the ids excluded in the original pipeline).

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
SURVEY_V2 <- if (exists("SURVEY_V2")) SURVEY_V2 else "survey_data_v2.Rds"   # set the full path if needed

# --- Build full_data_sample from the new survey file ------------------------
stopifnot(file.exists(SURVEY_V2))
needed <- c("rao_scores_df", "total_tracking_activity_0_total_june2025")
missing_obj <- needed[!vapply(needed, exists, logical(1))]
if (length(missing_obj) > 0) stop("Run the pipeline up to the Rao scores first. Missing: ",
                                  paste(missing_obj, collapse = ", "))
if (!exists("mapping_consolidated")) {
  stopifnot(file.exists("mapping_consolidated.rds"))
  mapping_consolidated <- readRDS("mapping_consolidated.rds")
}

# Ids can be numeric (16 digits -> scientific notation when converted), so format them safely
id_to_chr <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE)
  as.character(x)
}

survey_data <- readRDS(SURVEY_V2)
colnames(survey_data)[1] <- "psid_survey"            # the id is the first column
survey_data$psid_survey <- id_to_chr(survey_data$psid_survey)
message("Survey file: ", nrow(survey_data), " rows; ",
        sum(!is.na(survey_data$Internal)), " with Internal efficacy")

# Compare ids in one common format: plain text, without the ",00" suffix, trimmed.
norm_id <- function(x) trimws(stringr::str_remove_all(id_to_chr(x), ",00"))
survey_data$psid_survey <- norm_id(survey_data$psid_survey)

if (exists("psid_to_remove")) {
  remove_ids <- unique(norm_id(psid_to_remove))
  n_found <- sum(remove_ids %in% survey_data$psid_survey)
  message("psid_to_remove: ", length(remove_ids), " ids in the list, ", n_found, " found in the survey file")
  if (n_found == 0) warning("None of the ids in psid_to_remove were found. Compare the formats: ",
                            "head(psid_to_remove) vs head(survey_data$psid_survey).")
  survey_data <- survey_data[!survey_data$psid_survey %in% remove_ids, ]
} else {
  message("psid_to_remove not found: no ids excluded (the original pipeline excluded a list of ids).")
}

# The tracking id column is called `panelist_id` in the raw tracking table; the original
# pipeline renames the first column to `psid_tracking`. Do the same here if that hasn't happened.
ensure_tracking_id <- function(df, what) {
  if (!"psid_tracking" %in% names(df)) {
    if ("panelist_id" %in% names(df)) names(df)[names(df) == "panelist_id"] <- "psid_tracking"
    else { message(what, ": no psid_tracking/panelist_id column; using the first column (", names(df)[1], ")")
           names(df)[1] <- "psid_tracking" }
  }
  df$psid_tracking <- id_to_chr(df$psid_tracking)
  df
}

map_ids <- mapping_consolidated
map_ids$psid_survey <- id_to_chr(map_ids$psid_survey)
map_ids <- ensure_tracking_id(map_ids, "mapping_consolidated")
tracking_ids <- ensure_tracking_id(total_tracking_activity_0_total_june2025, "tracking activity")
rao_ids <- ensure_tracking_id(rao_scores_df, "rao_scores_df")

full <- survey_data %>% left_join(map_ids, by = "psid_survey")
message("After joining the id mapping: ", sum(!is.na(full$psid_tracking)), " with a tracking id")
full <- full %>% inner_join(tracking_ids, by = "psid_tracking")
message("After joining tracking activity: ", nrow(full), " rows")
full <- full %>% inner_join(rao_ids, by = "psid_tracking")
message("After joining Rao scores: ", nrow(full), " rows (the original pipeline had 954)")
if (anyDuplicated(full$psid_survey) > 0) warning("Duplicated survey ids after the joins.")

full$edu[full$edu == -77] <- NA
full$log_unique_days <- log(full$unique_days)
full_data_sample <- full

d <- full_data_sample

# --- Outcome and survey wave ---------------------------------------------
# Internal efficacy = poleffic2 + poleffic4 (two items on 0-10, range 0-20).
# Produced by 00_import_clean_survey.R (raw codes 1-11 shifted to 0-10).
bad <- sum(d$Internal != d$poleffic2 + d$poleffic4, na.rm = TRUE)
message("Rows where Internal != poleffic2 + poleffic4: ", bad, " (expected 0)")
message("Range of Internal: ", paste(range(d$Internal, na.rm = TRUE), collapse = " to "), " (expected within 0 to 20)")

has_w1 <- !is.na(d$poleffic2_W1) & !is.na(d$poleffic4_W1)
has_w4 <- !is.na(d$poleffic2_W4) & !is.na(d$poleffic4_W4)
d$wave <- dplyr::case_when(has_w1 & !has_w4 ~ "W1",
                           has_w4 & !has_w1 ~ "W4",
                           has_w1 & has_w4  ~ "both",
                           TRUE             ~ NA_character_)
message("Participants by wave of efficacy measurement:")
print(table(d$wave, useNA = "ifany"))
if (any(d$wave == "both", na.rm = TRUE)) {
  warning("Some participants have efficacy in both W1 and W4. ",
          "Check how poleffic2/poleffic4 were derived upstream; treating them as W1.")
  d$wave[d$wave == "both"] <- "W1"
}

# --- Demographics ---------------------------------------------------------
# `age` looks like BIRTH YEAR (e.g. 1958), not age. Convert if so.
d$edu[d$edu == -77] <- NA
if (median(d$age, na.rm = TRUE) > 1900) {
  # Values <= 1900 (e.g. 0) are invalid birth years; they would otherwise stop the conversion
  n_bad_age <- sum(d$age <= 1900, na.rm = TRUE)
  if (n_bad_age > 0) message(n_bad_age, " invalid birth year(s) (<= 1900) set to missing")
  d$age[!is.na(d$age) & d$age <= 1900] <- NA
  message("`age` holds birth years; converting to age in years (survey year 2022 for W1, 2023 for W4).")
  d$birth_year <- d$age
  d$age <- ifelse(d$wave == "W1", 2022, 2023) - d$birth_year
}
# Gender: female (code 2) vs not female (code 1 = male; code 3 = other, only 2 people).
# Missing stays missing. The coefficient is labelled genderfemale in the models.
d$gender <- factor(ifelse(is.na(d$gender), NA_character_,
                          ifelse(d$gender == 2, "female", "not female")),
                   levels = c("not female", "female"))

# --- Analysis sample (complete cases on the core variables) ---------------
core <- c("Internal", "age", "gender", "edu", "FunRao", "log_unique_days",
          "offline_freq", "wave")
d <- d[stats::complete.cases(d[, core]), ]
d$wave <- factor(d$wave, levels = c("W1", "W4"))
message("Analysis sample: N = ", nrow(d), " (the manuscript reports 728)")

# --- Standardised predictors ----------------------------------------------
d$rao_z     <- zs(d$FunRao)          # diversity, per SD
d$age_z     <- zs(d$age)
d$edu_z     <- zs(d$edu)
d$offline_z <- zs(d$offline_freq)
# Other media-use / discussion measures available in the survey (names as in the data;
# `social_freq` is assumed to be interpersonal political talk, `socialmedia_freq` social media).
d$social_z      <- zs(d$social_freq)
d$socialmedia_z <- zs(d$socialmedia_freq)

analysis_data <- d
SD_OUTCOME <- sd(analysis_data$Internal)

# --- Summary statistics ---------------------------------------------------
d <- analysis_data
nrow(d)
table(d$wave)
summary(d$age); sd(d$age)
table(d$gender)
summary(d$edu)
summary(d$Internal); sd(d$Internal)
summary(d$FunRao)
summary(d$offline_freq)
summary(d$log_unique_days)
colSums(is.na(d[, c("social_z", "socialmedia_z", "unique_days")]))
round(cor(d[, c("FunRao", "age", "edu", "log_unique_days", "offline_freq", "Internal")]), 2)
