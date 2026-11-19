# 00_import_clean_survey.R -------------------------------------------------
# Re-import and clean the survey data. Replaces the old import/cleaning code.
#
# Key corrections compared with the old version
#   1. Internal efficacy = poleffic2 + poleffic4 (own abilities / active role).
#      The old code used poleffic1 + poleffic2, which mixes an external-efficacy
#      item (poleffic1: "system gives people like you a say") with an internal one.
#      External efficacy is no longer computed.
#   2. Efficacy items are stored as codes 1-11 (labels 0-10). They are shifted to
#      0-10, so Internal runs 0-20. Value 0 is not a valid code and is handled by
#      ZERO_AS_MISSING (see below). Negative codes (-77 etc.) become NA.
#   3. "Exposure" indicators are aggregated across waves with "any exposure"
#      (old code summed them, so a person answering 1 in two waves had 2 and the
#      later `== 1` tests missed them). Frequencies are still summed across waves.
#
# Needs from your session: `con_survey` (database connection) and
# `SQL_query_function()` (your existing function). No data are written to the
# repository; the cleaned file is saved wherever you set OUT_FILE.

library(dplyr)

ZERO_AS_MISSING <- TRUE    # TRUE: invalid 0 -> NA (7 values seen in wave 1). FALSE: 0 -> lowest category.
OUT_FILE <- "survey_data_v2.Rds"   # choose a path OUTSIDE the git repository

stopifnot(exists("con_survey"), exists("SQL_query_function"))

# --- What to query -------------------------------------------------------------
exposure_bases <- c(
  "fernsehen", "nachrichtensender", "info_fernsehen", "radionachrichten",
  "nachrichtenpodcast", "infopodcasts", "radio", "radionachrichten_sender",
  "gedr_zeitung", "gedr_magazine", "andere_print",
  "instagram", "reddit", "signal", "pinterest", "youtube", "facebook", "tumblr",
  "discord", "tiktok", "twitch", "telegram", "threema", "twitter", "nebenan",
  "xing", "linkedin", "snapchat", "whatsapp", "andere_sozialemedien",
  "freunden", "familien", "kollegen", "andere_gesp")
exposure_vars <- paste0(exposure_bases, "_exposure")
freq_vars     <- paste0(exposure_bases, "_freq")

variables_to_query <- c("duration", "age", "edu", "gender", "polinterest", "newsinterest",
                        "poleffic2", "poleffic4", exposure_vars, freq_vars)
waves <- c(1, 2, 3, 4)

survey <- SQL_query_function(variables_to_query, waves, "YES")

# --- Basic cleaning ------------------------------------------------------------
survey[survey == "-77.0"] <- NA                          # "not answered"
survey <- survey[, colSums(is.na(survey)) != nrow(survey)]   # drop all-NA columns
# The participant ID column was called p_0002 in the old database and is id_survey now.
ID_COL <- intersect(c("id_survey", "p_0002"), names(survey))[1]
if (is.na(ID_COL)) stop("No ID column found (looked for id_survey, p_0002). Columns: ",
                        paste(head(names(survey), 10), collapse = ", "))
message("Using ID column: ", ID_COL)
survey <- survey %>% mutate(across(-all_of(ID_COL), ~ as.numeric(.)))

# --- Aggregate media-use variables across waves ----------------------------------
row_agg <- function(df, base, how) {
  m <- dplyr::select(df, dplyr::matches(paste0("^", base, "(_.*)?$")))
  if (ncol(m) == 0) return(rep(NA_real_, nrow(df)))
  m <- matrix(as.numeric(as.matrix(m)), nrow = nrow(m))
  if (how == "any") as.numeric(rowSums(m > 0, na.rm = TRUE) > 0) else rowSums(m, na.rm = TRUE)
}
new_cols <- c(
  setNames(lapply(exposure_vars, row_agg, df = survey, how = "any"), exposure_vars),
  setNames(lapply(freq_vars,     row_agg, df = survey, how = "sum"), freq_vars))
survey[names(new_cols)] <- new_cols

# --- Efficacy items --------------------------------------------------------------
# Raw codes 1-11 correspond to 0-10. Zeros are not valid codes.
clean_item <- function(x) {
  x <- as.numeric(x)
  x[!is.na(x) & x < 0] <- NA
  n_zero <- sum(x == 0, na.rm = TRUE)
  if (n_zero > 0) message("  ", n_zero, " value(s) of 0 found; ",
                          if (ZERO_AS_MISSING) "set to NA" else "recoded to 1 (lowest valid code)")
  x[!is.na(x) & x == 0] <- if (ZERO_AS_MISSING) NA else 1
  x - 1
}
item_cols <- c(outer(c("poleffic2", "poleffic4"), c("W1", "W4"), paste, sep = "_"))
stopifnot(all(item_cols %in% names(survey)))
for (col in item_cols) { message("Cleaning ", col); survey[[col]] <- clean_item(survey[[col]]) }

# --- Keep needed columns --------------------------------------------------------
columns_list <- c(ID_COL, "age", "edu", "gender", item_cols, exposure_vars, freq_vars)
survey_filtered <- survey %>% dplyr::select(all_of(columns_list))
# The pipeline renames the FIRST column to psid_survey, so the ID must stay first.

survey_filtered <- survey_filtered %>%
  mutate(
    offline_use = as.numeric(rowSums(across(all_of(paste0(exposure_bases[1:11], "_exposure"))) > 0) > 0),
    offline_freq = rowSums(across(all_of(paste0(exposure_bases[1:11], "_freq")))),
    social_use = as.numeric(rowSums(across(all_of(paste0(exposure_bases[31:34], "_exposure"))) > 0) > 0),
    social_freq = rowSums(across(all_of(paste0(exposure_bases[31:34], "_freq")))),
    socialmedia_use = as.numeric(rowSums(across(all_of(paste0(exposure_bases[12:30], "_exposure"))) > 0) > 0),
    socialmedia_freq = rowSums(across(all_of(paste0(exposure_bases[12:30], "_freq"))))
  )

# --- Internal efficacy (0-20) -----------------------------------------------------
# Item per wave -> one value per person (mean over the waves answered; mostly one).
nan_to_na <- function(x) ifelse(is.nan(x), NA_real_, x)
survey_filtered <- survey_filtered %>%
  mutate(
    poleffic2 = nan_to_na(rowMeans(dplyr::select(., poleffic2_W1, poleffic2_W4), na.rm = TRUE)),
    poleffic4 = nan_to_na(rowMeans(dplyr::select(., poleffic4_W1, poleffic4_W4), na.rm = TRUE)),
    Internal  = poleffic2 + poleffic4
  )

# --- Checks (read these) ------------------------------------------------------------
message("\nInternal efficacy, range (expected within 0 to 20): ",
        paste(range(survey_filtered$Internal, na.rm = TRUE), collapse = " to "))
message("Mean / SD: ", round(mean(survey_filtered$Internal, na.rm = TRUE), 2), " / ",
        round(sd(survey_filtered$Internal, na.rm = TRUE), 2))
message("Participants with Internal: ", sum(!is.na(survey_filtered$Internal)))
message("Correlation of the two items: ",
        round(cor(survey_filtered$poleffic2, survey_filtered$poleffic4, use = "complete.obs"), 2))
print(table(poleffic2 = survey_filtered$poleffic2, useNA = "ifany"))
print(table(poleffic4 = survey_filtered$poleffic4, useNA = "ifany"))

saveRDS(survey_filtered, OUT_FILE)
message("Saved: ", OUT_FILE, "  (point the pipeline's readRDS('survey_data.Rds') to this file)")
