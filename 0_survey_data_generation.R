#Packages

library(dplyr)
library(DBI)
source("C:/Users/rauxloha/poltrack2/Diversity_Paper/preprocessing/SQL_query_function.R")



#Draw sample
con_survey <- dbConnect(RMySQL::MySQL(), 
                        ###')
dbListTables(con_survey)

variables_to_query <- c('duration','age', 'edu', 'gender',
                        'polinterest', 
                        'newsinterest', 'poleffic1', 'poleffic2',
                        'poleffic3', 'poleffic4',
                        # 'political_engagement_1',
                        # 'political_engagement_2',
                        # 'political_engagement_3',
                        # 'political_engagement_4',
                        # 'political_engagement_5',
                        # 'political_engagement_6',
                        # 'political_engagement_7',
                        # 'political_engagement_none',
                        'fernsehen_exposure',
                          'nachrichtensender_exposure',
                         'info_fernsehen_exposure',
                          'radionachrichten_exposure',
                          'nachrichtenpodcast_exposure',
                          'infopodcasts_exposure',
                          'radio_exposure',
                          'radionachrichten_sender_exposure',
                          'gedr_zeitung_exposure',
                          'gedr_magazine_exposure',
                          'andere_print_exposure',
                        'fernsehen_freq' , 
                          'nachrichtensender_freq' , 
                          'info_fernsehen_freq' , 
                          'radionachrichten_freq' , 
                          'nachrichtenpodcast_freq' , 
                          'infopodcasts_freq' , 
                          'radio_freq' , 
                          'radionachrichten_sender_freq' , 
                          'gedr_zeitung_freq' , 
                          'gedr_magazine_freq' , 
                          'andere_print_freq',
                        'instagram_exposure', 
                          'reddit_exposure', 
                          'signal_exposure', 
                          'pinterest_exposure', 
                          'youtube_exposure', 
                          'facebook_exposure', 
                          'tumblr_exposure', 
                          'discord_exposure', 
                          'tiktok_exposure', 
                          'twitch_exposure', 
                          'telegram_exposure', 
                          'threema_exposure', 
                          'twitter_exposure', 
                          'nebenan_exposure', 
                          'xing_exposure', 
                          'linkedin_exposure', 
                          'snapchat_exposure', 
                          'whatsapp_exposure', 
                          'andere_sozialemedien_exposure',
                        'instagram_freq', 
                          'reddit_freq', 
                          'signal_freq', 
                          'pinterest_freq', 
                          'youtube_freq', 
                          'facebook_freq', 
                          'tumblr_freq', 
                          'discord_freq', 
                          'tiktok_freq', 
                          'twitch_freq', 
                          'telegram_freq', 
                          'threema_freq', 
                          'twitter_freq', 
                          'nebenan_freq', 
                          'xing_freq', 
                          'linkedin_freq', 
                          'snapchat_freq', 
                          'whatsapp_freq', 
                          'andere_sozialemedien_freq',
                        'freunden_exposure', 
                        'familien_exposure', 
                        'kollegen_exposure', 
                        'andere_gesp_exposure',
                        'freunden_freq', 
                        'familien_freq',
                        'kollegen_freq', 
                        'andere_gesp_freq')

waves <- c(1, 2, 3, 4)
dbListTables(con_survey)
survey <- SQL_query_function(variables_to_query, waves, 'YES')
survey[survey == "-77.0"] <- NA
survey <- survey[, colSums(is.na(survey)) != nrow(survey)] # 67 non null columns remain
survey <- survey %>%
  mutate(across(-p_0002, ~ as.numeric(.)))
survey <- survey %>%
  dplyr::select(where(~ !all(is.na(.)))) # dropping all columns where every row is NA

survey$info_fernsehen_exposure <- rowSums(
  dplyr::select(survey, starts_with("info_fernsehen_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$radionachrichten_exposure <- rowSums(
  dplyr::select(survey, starts_with("radionachrichten_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$nachrichtensender_exposure <- rowSums(
  dplyr::select(survey, starts_with("nachrichtensender_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$fernsehen_exposure <- rowSums(
  dplyr::select(survey, starts_with("fernsehen_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$nachrichtenpodcast_exposure <- rowSums(
  dplyr::select(survey, starts_with("nachrichtenpodcast_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$infopodcasts_exposure <- rowSums(
  dplyr::select(survey, starts_with("infopodcasts_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$radio_exposure <- rowSums(
  dplyr::select(survey, starts_with("radio_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$radionachrichten_sender_exposure <- rowSums(
  dplyr::select(survey, starts_with("radionachrichten_sender_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$gedr_zeitung_exposure <- rowSums(
  dplyr::select(survey, starts_with("gedr_zeitung_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$gedr_magazine_exposure <- rowSums(
  dplyr::select(survey, starts_with("gedr_magazine_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$andere_print_exposure <- rowSums(
  dplyr::select(survey, starts_with("andere_print_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)


survey$fernsehen_freq <- rowSums(
  dplyr::select(survey, starts_with("fernsehen_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$nachrichtensender_freq <- rowSums(
  dplyr::select(survey, starts_with("nachrichtensender_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$info_fernsehen_freq <- rowSums(
  dplyr::select(survey, starts_with("info_fernsehen_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$radionachrichten_freq <- rowSums(
  dplyr::select(survey, starts_with("radionachrichten_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$nachrichtenpodcast_freq <- rowSums(
  dplyr::select(survey, starts_with("nachrichtenpodcast_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$infopodcasts_freq <- rowSums(
  dplyr::select(survey, starts_with("infopodcasts_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$radio_freq <- rowSums(
  dplyr::select(survey, starts_with("radio_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$radionachrichten_sender_freq <- rowSums(
  dplyr::select(survey, starts_with("radionachrichten_sender_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$gedr_zeitung_freq <- rowSums(
  dplyr::select(survey, starts_with("gedr_zeitung_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$gedr_magazine_freq <- rowSums(
  dplyr::select(survey, starts_with("gedr_magazine_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$andere_print_freq <- rowSums(
  dplyr::select(survey, starts_with("andere_print_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$instagram_exposure <- rowSums(
  dplyr::select(survey, starts_with("instagram_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$reddit_exposure <- rowSums(
  dplyr::select(survey, starts_with("reddit_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$signal_exposure <- rowSums(
  dplyr::select(survey, starts_with("signal_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$pinterest_exposure <- rowSums(
  dplyr::select(survey, starts_with("pinterest_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$youtube_exposure <- rowSums(
  dplyr::select(survey, starts_with("youtube_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$facebook_exposure <- rowSums(
  dplyr::select(survey, starts_with("facebook_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$tumblr_exposure <- rowSums(
  dplyr::select(survey, starts_with("tumblr_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$discord_exposure <- rowSums(
  dplyr::select(survey, starts_with("discord_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$tiktok_exposure <- rowSums(
  dplyr::select(survey, starts_with("tiktok_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$twitch_exposure <- rowSums(
  dplyr::select(survey, starts_with("twitch_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$telegram_exposure <- rowSums(
  dplyr::select(survey, starts_with("telegram_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$threema_exposure <- rowSums(
  dplyr::select(survey, starts_with("threema_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$twitter_exposure <- rowSums(
  dplyr::select(survey, starts_with("twitter_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$nebenan_exposure <- rowSums(
  dplyr::select(survey, starts_with("nebenan_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$xing_exposure <- rowSums(
  dplyr::select(survey, starts_with("xing_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$linkedin_exposure <- rowSums(
  dplyr::select(survey, starts_with("linkedin_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$snapchat_exposure <- rowSums(
  dplyr::select(survey, starts_with("snapchat_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$whatsapp_exposure <- rowSums(
  dplyr::select(survey, starts_with("whatsapp_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$andere_sozialemedien_exposure <- rowSums(
  dplyr::select(survey, starts_with("andere_sozialemedien_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$instagram_freq <- rowSums(
  dplyr::select(survey, starts_with("instagram_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$reddit_freq <- rowSums(
  dplyr::select(survey, starts_with("reddit_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$signal_freq <- rowSums(
  dplyr::select(survey, starts_with("signal_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$pinterest_freq <- rowSums(
  dplyr::select(survey, starts_with("pinterest_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$youtube_freq <- rowSums(
  dplyr::select(survey, starts_with("youtube_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$facebook_freq <- rowSums(
  dplyr::select(survey, starts_with("facebook_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$tumblr_freq <- rowSums(
  dplyr::select(survey, starts_with("tumblr_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$discord_freq <- rowSums(
  dplyr::select(survey, starts_with("discord_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$tiktok_freq <- rowSums(
  dplyr::select(survey, starts_with("tiktok_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$twitch_freq <- rowSums(
  dplyr::select(survey, starts_with("twitch_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$telegram_freq <- rowSums(
  dplyr::select(survey, starts_with("telegram_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$threema_freq <- rowSums(
  dplyr::select(survey, starts_with("threema_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$twitter_freq <- rowSums(
  dplyr::select(survey, starts_with("twitter_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$nebenan_freq <- rowSums(
  dplyr::select(survey, starts_with("nebenan_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$xing_freq <- rowSums(
  dplyr::select(survey, starts_with("xing_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$linkedin_freq <- rowSums(
  dplyr::select(survey, starts_with("linkedin_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$snapchat_freq <- rowSums(
  dplyr::select(survey, starts_with("snapchat_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$whatsapp_freq <- rowSums(
  dplyr::select(survey, starts_with("whatsapp_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$andere_sozialemedien_freq <- rowSums(
  dplyr::select(survey, starts_with("andere_sozialemedien_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$freunden_exposure <- rowSums(
  dplyr::select(survey, starts_with("freunden_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$familien_exposure <- rowSums(
  dplyr::select(survey, starts_with("familien_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$kollegen_exposure <- rowSums(
  dplyr::select(survey, starts_with("kollegen_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$andere_gesp_exposure <- rowSums(
  dplyr::select(survey, starts_with("andere_gesp_exposure")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

survey$freunden_freq <- rowSums(
  dplyr::select(survey, starts_with("freunden_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$familien_freq <- rowSums(
  dplyr::select(survey, starts_with("familien_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$kollegen_freq <- rowSums(
  dplyr::select(survey, starts_with("kollegen_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)
survey$andere_gesp_freq <- rowSums(
  dplyr::select(survey, starts_with("andere_gesp_freq")) %>% 
    mutate(across(everything(), as.numeric)), 
  na.rm = TRUE
)

#Filter efficacy, and offline use
# survey_filtered <- survey %>%
#   filter(!if_any(c(age, edu, gender), ~ is.na(.))) #2674 participants remaining
# commented out the abouve to impute data from the participant table

columns_list <- c('p_0002', 'age', 'edu', 'gender',
  'poleffic1_W1', 'poleffic2_W1',
  'poleffic3_W1', 'poleffic4_W1',
  'poleffic1_W4', 'poleffic2_W4',
  'poleffic3_W4', 'poleffic4_W4',
  'fernsehen_exposure',
  'nachrichtensender_exposure',
  'info_fernsehen_exposure',
  'radionachrichten_exposure',
  'nachrichtenpodcast_exposure',
  'infopodcasts_exposure',
  'radio_exposure',
  'radionachrichten_sender_exposure',
  'gedr_zeitung_exposure',
  'gedr_magazine_exposure',
  'andere_print_exposure',
  'fernsehen_freq' , 
  'nachrichtensender_freq' , 
  'info_fernsehen_freq' , 
  'radionachrichten_freq' , 
  'nachrichtenpodcast_freq' , 
  'infopodcasts_freq' , 
  'radio_freq' , 
  'radionachrichten_sender_freq' , 
  'gedr_zeitung_freq' , 
  'gedr_magazine_freq' , 
  'andere_print_freq',
  'instagram_exposure', 
  'reddit_exposure', 
  'signal_exposure', 
  'pinterest_exposure', 
  'youtube_exposure', 
  'facebook_exposure', 
  'tumblr_exposure', 
  'discord_exposure', 
  'tiktok_exposure', 
  'twitch_exposure', 
  'telegram_exposure', 
  'threema_exposure', 
  'twitter_exposure', 
  'nebenan_exposure', 
  'xing_exposure', 
  'linkedin_exposure', 
  'snapchat_exposure', 
  'whatsapp_exposure', 
  'andere_sozialemedien_exposure',
  'instagram_freq', 
  'reddit_freq', 
  'signal_freq', 
  'pinterest_freq', 
  'youtube_freq', 
  'facebook_freq', 
  'tumblr_freq', 
  'discord_freq', 
  'tiktok_freq', 
  'twitch_freq', 
  'telegram_freq', 
  'threema_freq', 
  'twitter_freq', 
  'nebenan_freq', 
  'xing_freq', 
  'linkedin_freq', 
  'snapchat_freq', 
  'whatsapp_freq', 
  'andere_sozialemedien_freq',
  'freunden_exposure', 
  'familien_exposure', 
  'kollegen_exposure', 
  'andere_gesp_exposure',
  'freunden_freq', 
  'familien_freq',
  'kollegen_freq', 
  'andere_gesp_freq')

survey_filtered <- survey %>%
  dplyr::select(all_of(columns_list))

survey_filtered <- survey_filtered %>%
  mutate(
    # Offline use indicator
    offline_use = if_else(
      fernsehen_exposure == 1 | 
        nachrichtensender_exposure == 1 | 
        info_fernsehen_exposure == 1 | 
        radionachrichten_exposure == 1 | 
        nachrichtenpodcast_exposure == 1 | 
        infopodcasts_exposure == 1 | 
        radio_exposure == 1 | 
        radionachrichten_sender_exposure == 1 | 
        gedr_zeitung_exposure == 1 | 
        gedr_magazine_exposure == 1 | 
        andere_print_exposure == 1, 
      1, 0
    ),
    
    # Offline frequency sum
    offline_freq = fernsehen_freq + 
      nachrichtensender_freq + 
      info_fernsehen_freq + 
      radionachrichten_freq + 
      nachrichtenpodcast_freq + 
      infopodcasts_freq + 
      radio_freq + 
      radionachrichten_sender_freq + 
      gedr_zeitung_freq + 
      gedr_magazine_freq + 
      andere_print_freq,
    
    # Social use indicator
    social_use = if_else(
      freunden_exposure == 1 | 
        familien_exposure == 1 | 
        kollegen_exposure == 1 | 
        andere_gesp_exposure == 1, 
      1, 0
    ),
    
    # Social frequency sum
    social_freq = freunden_freq + 
      familien_freq + 
      kollegen_freq + 
      andere_gesp_freq,
    
    # Social media use indicator
    socialmedia_use = if_else(
      instagram_exposure == 1 | 
        reddit_exposure == 1 | 
        signal_exposure == 1 | 
        pinterest_exposure == 1 | 
        youtube_exposure == 1 | 
        facebook_exposure == 1 | 
        tumblr_exposure == 1 | 
        discord_exposure == 1 | 
        tiktok_exposure == 1 | 
        twitch_exposure == 1 | 
        telegram_exposure == 1 | 
        threema_exposure == 1 | 
        twitter_exposure == 1 | 
        nebenan_exposure == 1 | 
        xing_exposure == 1 | 
        linkedin_exposure == 1 | 
        snapchat_exposure == 1 | 
        whatsapp_exposure == 1 | 
        andere_sozialemedien_exposure == 1, 
      1, 0
    ),
    
    # Social media frequency sum
    socialmedia_freq = instagram_freq + 
      reddit_freq + 
      signal_freq + 
      pinterest_freq + 
      youtube_freq + 
      facebook_freq + 
      tumblr_freq + 
      discord_freq + 
      tiktok_freq + 
      twitch_freq + 
      telegram_freq + 
      threema_freq + 
      twitter_freq + 
      nebenan_freq + 
      xing_freq + 
      linkedin_freq + 
      snapchat_freq + 
      whatsapp_freq + 
      andere_sozialemedien_freq
  )

# 
# efficacy_sample <- survey_filtered %>%
#   filter(!if_all(starts_with('poleffic'), ~ is.na(.))) #2067

# participation_sample <- survey_filtered %>%
#   filter(!if_all(starts_with('political_engagement'), is.na)) #2629

## Replace 11 with NA in relevant columns before computing means
survey_filtered <- survey_filtered %>%
  mutate(
    poleffic1 = rowMeans(dplyr::select(., poleffic1_W1, poleffic1_W4), na.rm = TRUE),
    poleffic2 = rowMeans(dplyr::select(., poleffic2_W1, poleffic2_W4), na.rm = TRUE),
    poleffic3 = rowMeans(dplyr::select(., poleffic3_W1, poleffic3_W4), na.rm = TRUE),
    poleffic4 = rowMeans(dplyr::select(., poleffic4_W1, poleffic4_W4), na.rm = TRUE),
    
    # Creating the Internal and External variables
    Internal = poleffic1 + poleffic2,
    External = poleffic3 + poleffic4
  )



## Consolidating Participation Variables 
# survey_filtered <- survey_filtered %>%
#   mutate(
#     political_engagement_sum = 
#       (political_engagement_1 * 2) +
#       (political_engagement_2 * 2) +
#       (political_engagement_3 * 1) +
#       (political_engagement_4 * 1) +
#       (political_engagement_5 * 2) +
#       (political_engagement_6 * 1) +
#       (political_engagement_7 * 1) +
#       (political_engagement_none * 0)
#   )


# survey_filtered <- survey_filtered %>%
#   mutate(participation_ordinal = case_when(
#     political_engagement_1 == 1 | political_engagement_2 == 1 | political_engagement_3 == 1 | political_engagement_5 == 1 ~ 2,
#     political_engagement_4 == 1 | political_engagement_6 == 1 |
#       political_engagement_7 == 1 ~ 1,
#     political_engagement_none == 1 ~ 0,
#     TRUE ~ NA_real_  # if none of the conditions are met
#   ))


dbDisconnect(con_tracking)
saveRDS(survey_filtered, "survey_data.Rds")