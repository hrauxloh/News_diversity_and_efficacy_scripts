# 01_prepare_data.R -------------------------------------------------------
# Builds `analysis_data` from `full_data_sample` (created by
# Full_pipeline_diversity_github.R, i.e. survey + tracking + Rao score).
# Run from the repository root. Nothing here reads raw tracking data.

source(file.path(if (exists("RA_DIR")) RA_DIR else "revision_analysis", "helpers.R"))
stopifnot(exists("full_data_sample"))

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
if (all(d$age[!is.na(d$age)] > 1900)) {
  message("`age` holds birth years; converting to age in years (survey year 2022 for W1, 2023 for W4).")
  d$birth_year <- d$age
  d$age <- ifelse(d$wave == "W1", 2022, 2023) - d$birth_year
}
d$gender <- factor(d$gender)

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
