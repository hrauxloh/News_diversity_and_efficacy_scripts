library(dplyr)
library(DBI)
library(readr)
library(tidyverse)
library(splines)
library(faraway)
library(broom)
library(dotwhisker)
library(SYNCSA)
library(tidyr)


# Tracking Data ----
#Step 1
#Collecting all domain, deeplink, political views, over 2 seconds = tracking_step_1
## Connecting to database ----
total_tracking_activity_0_total_june2025 <- read_csv( #######
                                                                               ,
  col_types = cols(panelist_id = col_character()))

colnames(total_tracking_activity_0_total_june2025)[1] <- "psid_tracking"


domains <- readRDS("GONDv3_domains.rds")
con_tracking <- dbConnect(RPostgres::Postgres(),
                          ########,
                          ########,
                          ########,
                          ########,
                          ########
                          )
dbListTables(con_tracking)

domains <- paste0("'", trimws(domains$domain), "'", collapse = ",")


query <- paste0("
  SELECT v.*
  FROM views_new v
  JOIN content_new c
    ON v.url_hash = c.url_hash
  WHERE domain IN (", domains, ")
    AND v.is_news_deep_link = TRUE
    AND v.active_seconds >= 2
    AND c.is_pol_ml = 'political'
")

# query and save the result as tracking_all_0
tracking_data_0 <- dbGetQuery(con_tracking, query) #450,473


dbDisconnect(con_tracking)


colnames(tracking_data_0)[1] <- "psid_tracking"

#Step 2
#Grouping participants by number of visits from tracking_data_0 = Participant_views
participant_views_freq <- tracking_data_0 %>%
  group_by(psid_tracking) %>%
  summarise(frequency = n()) %>%
  arrange(desc(frequency))

#Step 3
#Filtering tracking_step_1 by participants with over 30 views
tracking_data_step2 <- tracking_data_0 %>%
  group_by(psid_tracking) %>%
  filter(n() > 30) #433,831


#Calculating Rao ----

##Loading domain attributes ----
domain_attributes <- read_csv("domain_attributes_0_june2025.csv")

domain_attributes <- domain_attributes %>%
  filter(!(domain == "yahoo.com" & news_type == "portal"))

comm <- tracking_data_step2 %>%
  count(psid_tracking, domain) %>%
  pivot_wider(names_from = domain, values_from = n, values_fill = 0) %>%
  column_to_rownames("psid_tracking")

traits <- domain_attributes %>%
  distinct(domain, news_type, Orientation) %>%
  column_to_rownames("domain")

# Convert character columns to factors, then dummy variables
traits[] <- lapply(traits, as.factor)
traits_mat <- model.matrix(~ . -1, data = traits)

rao_result <- rao.diversity(
  comm = as.matrix(comm),
  traits = traits_mat,
  standardize = TRUE
)
rao_scores <- rao_result$FunRao
rao_scores_df <- data.frame(
  psid_tracking = names(rao_scores),
  rao_q = as.numeric(rao_scores)
)
colnames(rao_scores_df)[2] <- "FunRao"

# Survey data ----
#Step 1
#loading in survey data
survey_filtered <- readRDS("survey_data.Rds")
survey_data <- survey_filtered
colnames(survey_data)[1] <- "psid_survey"
psid_to_remove <- c(
  ########   ##removed for anonymity
)

# Drop survey_data rows
survey_data <- survey_data[!survey_data$psid_survey %in% psid_to_remove, ]

survey_data = survey_data %>%
  mutate(psid_survey = str_remove_all(psid_survey, ",00"))

# Combining Tracking & Survey ----
mapping_consolidated <- readRDS("mapping_consolidated.rds")
survey_data_mapped <- survey_data %>% left_join(mapping_consolidated,
                                                by= "psid_survey")


full_data_sample <- survey_data_mapped %>% inner_join(total_tracking_activity_0_total_june2025,
                                                      by = "psid_tracking") #954

full_data_sample <- full_data_sample %>% inner_join(rao_scores_df,
                                                    by= "psid_tracking") #954

##Centering data ----

full_data_sample$edu[full_data_sample$edu == -77] <- NA

full_data_sample <- full_data_sample %>%
  mutate(log_unique_days = log(unique_days))

#>0 Tracking Days (GROUP 1) ----

working_data_sample <- full_data_sample[, c("psid_tracking", "Internal",
                                            "age",
                                            "edu", "gender", "FunRao",
                                            "log_unique_days", "offline_freq")]

working_data_sample <- na.omit(working_data_sample) #728

working_data_sample <- working_data_sample %>%
  mutate(across(c(age, edu, FunRao, offline_freq),
                ~ scale(.)[, 1],
                .names = "{.col}_centered"))



##Null ----
null_model <- lm(
  formula = Internal ~  
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample
)
summary(null_model)

##Linear ----
linear_model <- lm(
  formula = Internal ~ FunRao_centered + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample
)
summary(linear_model)

##Cubic Spline ----
knots_quartiles <- quantile(working_data_sample$FunRao_centered, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Fit the model using the computed knots
quartiles_model <- lm(
  formula = Internal ~ ns(FunRao_centered, knots = knots_quartiles) + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample
)
summary(quartiles_model)

##Quartiles model summary statistics ----



working_data_sample_summary <- full_data_sample[, c("psid_tracking", "Internal",
                                                    "age",
                                                    "edu", "gender", "FunRao", "unique_days",
                                                    "log_unique_days", "offline_freq")]

working_data_sample_summary <- na.omit(working_data_sample_summary) #728

con_tracking <- dbConnect(RPostgres::Postgres(),
                          ########,
                          ########,
                          ########,
                          ########,
                          ########
)
dbListTables(con_tracking)

panelist_ids_summary <- paste0("'", trimws(working_data_sample_summary$psid_tracking), "'", collapse = ",")


query <- paste0("
  SELECT
    v.panelist_id,
    COUNT(*) AS row_count,
    COUNT(DISTINCT domain) AS unique_domain_count
  FROM views_new v
  JOIN content_new c
    ON v.url_hash = c.url_hash
  WHERE domain IN (", domains, ")
    AND v.is_news_deep_link = TRUE
    AND v.active_seconds >= 2
    AND c.is_pol_ml = 'political'
    AND v.panelist_id IN (", panelist_ids_summary, ")
  GROUP BY v.panelist_id
")

tracking_counts <- dbGetQuery(con_tracking, query)


dbDisconnect(con_tracking)

colnames(tracking_counts)[1] <- "psid_tracking"

working_data_sample_summary <- working_data_sample_summary %>% inner_join(tracking_counts,
                                                                          by = "psid_tracking") #954

knots_quartiles <- quantile(working_data_sample_summary$FunRao, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Create a quartile factor column
working_data_sample_summary <- working_data_sample_summary %>%
  mutate(
    FunRao_quartile = cut(
      FunRao,
      breaks = c(-Inf, knots_quartiles, Inf),
      labels = c("Q1", "Q2", "Q3", "Q4"),
      right = TRUE
    )
  )

# Create separate dataframes for each quartile
df_Q1 <- filter(working_data_sample_summary, FunRao_quartile == "Q1")
df_Q2 <- filter(working_data_sample_summary, FunRao_quartile == "Q2")
df_Q3 <- filter(working_data_sample_summary, FunRao_quartile == "Q3")
df_Q4 <- filter(working_data_sample_summary, FunRao_quartile == "Q4")
summary(df_Q1)
sd(df_Q1$row_count)
sd(df_Q1$unique_domain_count)

summary(df_Q2)
sd(df_Q2$row_count)
sd(df_Q2$unique_domain_count)

summary(df_Q3)
sd(df_Q3$row_count)
sd(df_Q3$unique_domain_count)

summary(df_Q4)
sd(df_Q4$row_count)
sd(df_Q4$unique_domain_count)


##Cublic Spline Cluster ----
wss <- sapply(1:10, function(k) {
  kmeans(working_data_sample$FunRao_centered, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, 
     xlab = "Number of Clusters K", ylab = "Within-cluster SS")


set.seed(123)  # for reproducibility

kmeans_result <- kmeans(working_data_sample$FunRao_centered, centers = 3, nstart = 25)

# Add the cluster labels to your dataframe
working_data_sample$rao_cluster <- as.factor(kmeans_result$cluster)

aggregate(FunRao_centered ~ rao_cluster, data = working_data_sample, mean)


ggplot(working_data_sample, aes(x = FunRao_centered, fill = rao_cluster)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_brewer(palette = "Set3", name = "Cluster") +
  theme_minimal()



# Step 1: Get cluster-based means
cluster_knots <- aggregate(FunRao_centered ~ rao_cluster, data = working_data_sample, mean)

# Step 2: Extract the means and sort them (important for spline)
knots_from_clusters <- sort(cluster_knots$FunRao_centered)

# Step 3: Fit the model using these knots
clusters_model <- lm(
  formula = Internal ~ ns(FunRao_centered, knots = knots_from_clusters)  + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample
)
summary(clusters_model)

#>50 Tracking Data (GROUP 2) ----

working_data_sample_subset_50plus <- full_data_sample %>%
  filter(unique_days >= 50)

working_data_sample_subset_50plus <- working_data_sample_subset_50plus[, c("psid_tracking", "Internal",
                                                                           "age",
                                                                           "edu", "gender", "FunRao",
                                                                           "log_unique_days", "offline_freq")]

working_data_sample_subset_50plus <- na.omit(working_data_sample_subset_50plus) #723

working_data_sample_subset_50plus <- working_data_sample_subset_50plus %>%
  mutate(across(c(age, edu, FunRao, offline_freq),
                ~ scale(.)[, 1],
                .names = "{.col}_centered"))


##Null ----
null_model_subset_50plus <- lm(
  formula = Internal ~  
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_50plus
)
summary(null_model_subset_50plus)

##Linear ----
linear_model_subset_50plus <- lm(
  formula = Internal ~ FunRao_centered + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_50plus
)
summary(linear_model_subset_50plus)



##Cubic Spline ----
knots_quartiles <- quantile(working_data_sample_subset_50plus$FunRao_centered, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Fit the model using the computed knots
quartiles_model_subset_50plus <- lm(
  formula = Internal ~ ns(FunRao_centered, knots = knots_quartiles) + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_50plus
)
summary(quartiles_model_subset_50plus)


#>150 Tracking Data (GROUP 3) ----

working_data_sample_subset_150plus <- full_data_sample %>%
  filter(unique_days >= 150)

working_data_sample_subset_150plus <- working_data_sample_subset_150plus[, c("psid_tracking", "Internal",
                                                                             "age",
                                                                             "edu", "gender", "FunRao",
                                                                             "log_unique_days", "offline_freq")]

working_data_sample_subset_150plus <- na.omit(working_data_sample_subset_150plus) #701

working_data_sample_subset_150plus <- working_data_sample_subset_150plus %>%
  mutate(across(c(age, edu, FunRao, offline_freq),
                ~ scale(.)[, 1],
                .names = "{.col}_centered"))
##Null ----
null_model_subset_150plus <- lm(
  formula = Internal ~  age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_150plus
)
summary(null_model_subset_150plus)

##Linear ----
linear_model_subset_150plus <- lm(
  formula = Internal ~ FunRao_centered + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_150plus
)
summary(linear_model_subset_150plus)



##Cubic Spline ----
knots_quartiles <- quantile(working_data_sample_subset_150plus$FunRao_centered, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Fit the model using the computed knots
quartiles_model_subset_150plus <- lm(
  formula = Internal ~ ns(FunRao_centered, knots = knots_quartiles) + 
    age_centered + gender + edu_centered + 
    log_unique_days + offline_freq_centered,
  data = working_data_sample_subset_150plus
)
summary(quartiles_model_subset_150plus)


#Saving Models ----
save(
  null_model,
  linear_model,
  quartiles_model,
  clusters_model,
  null_model_subset_50plus,
  linear_model_subset_50plus,
  quartiles_model_subset_50plus,
  null_model_subset_150plus,
  linear_model_subset_150plus,
  quartiles_model_subset_150plus,
  file = "regression_models.RData"
)
