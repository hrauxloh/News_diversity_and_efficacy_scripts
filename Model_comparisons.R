## Model_comparisons
library(stargazer)
library(ggplot2)
library(dplyr)
### Distribution of news types across sampling groups

# Extract psid_tracking for each sample group

# Extract psid_tracking for each sample group
psids_sample1 <- unique(working_data_sample$psid_tracking)
psids_sample2 <- unique(working_data_sample_subset_50plus$psid_tracking)
psids_sample3 <- unique(working_data_sample_subset_150plus$psid_tracking)

# Filter tracking_data_0 for each sample's psid_tracking
df1 <- tracking_data_0 %>% filter(psid_tracking %in% psids_sample1) %>% mutate(Sample = "Sample 1")
df2 <- tracking_data_0 %>% filter(psid_tracking %in% psids_sample2) %>% mutate(Sample = "Sample 2")
df3 <- tracking_data_0 %>% filter(psid_tracking %in% psids_sample3) %>% mutate(Sample = "Sample 3")

# Combine filtered dataframes
combined_df <- bind_rows(df1, df2, df3)

# Define nicer labels for news_type levels
news_type_labels <- c(
  "commercial broadcaster" = "Commercial Broadcaster",
  "digital-born news outlet" = "Digital-born News Outlet",
  "hyperpartisan news" = "Hyperpartisan News",
  "legacy press" = "Legacy Press",
  "portal" = "Portal",
  "public broadcaster" = "Public Broadcaster",
  "tabloid newspaper" = "Tabloid Newspaper"
)

# Replace factor levels with nicer labels
combined_df$news_type <- factor(combined_df$news_type,
                                levels = names(news_type_labels),
                                labels = news_type_labels)

# Calculate percentages of each news_type within each sample
percent_df <- combined_df %>%
  group_by(Sample, news_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Sample) %>%
  mutate(percent = count / sum(count) * 100)

# Plot with percentages
news_types_sample_plot <- ggplot(percent_df, aes(x = news_type, y = percent, fill = Sample)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Sample 1" = "#08306B",  # dark blue
                               "Sample 2" = "#2171B5",  # medium blue
                               "Sample 3" = "#6BAED6")) + # light blue
  labs(title = "Percentage Distribution of News Type Across Samples",
       x = "News Type",
       y = "Percentage (%)",
       fill = "Sample") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("news_types_sample_plot.png", plot = news_types_sample_plot, width = 10, height = 6, dpi = 600)


################################Coefficient plots ----
# Create a new environment
model_env <- new.env()

# Load the models into that environment
load("regression_models.RData", envir = model_env)

#>0 Tracking Data (GROUP 1) ----
stargazer(
  model_env$null_model,
  model_env$linear_model,
  model_env$quartiles_model,
  type = "latex",
  title = "Comparison of Linear and Spline Models – Full Sample",
  column.labels = c("Null","Linear", "Spline (Quartiles)"),
  dep.var.caption = "Dependent variable: [Insert Your DV]",
  covariate.labels = c(
    "Rao (Linear)",
    "Rao Spline 1", "Rao Spline 2", 
    "Rao Spline 3", "Rao Spline 4",
    "Age", "Gender", "Education", 
    "Log Total Visits", "Offline News Use"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  out = "comparison_sample_1.tex"
)


#>50 Tracking Data (GROUP 2) ----
stargazer(
  #nodel_env$null_model_subset_50plus,
  model_env$linear_model_subset_50plus,
  model_env$quartiles_model_subset_50plus,
  type = "latex",
  title = "Comparison of Linear and Spline Models – Sample 2",
  column.labels = c("Null","Linear", "Spline (Quartiles)"),
  dep.var.caption = "Dependent variable: [Insert Your DV]",
  covariate.labels = c(
    "Rao (Linear)",
    "Rao Spline 1", "Rao Spline 2", 
    "Rao Spline 3", "Rao Spline 4",
    "Age", "Gender", "Education", 
    "Log Total Visits", "Offline News Use"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  out = "comparison_sample_2.tex"
)

#>150 Tracking Data (GROUP 3) ----
stargazer(
  model_env$null_model_subset_150plus,
  model_env$linear_model_subset_150plus,
  model_env$quartiles_model_subset_150plus,
  type = "latex",
  title = "Comparison of Linear and Spline Models – Sample 3",
  column.labels = c("Null","Linear", "Spline (Quartiles)"),
  dep.var.caption = "Dependent variable: [Insert Your DV]",
  covariate.labels = c(
    "Rao (Linear)",
    "Rao Spline 1", "Rao Spline 2", 
    "Rao Spline 3", "Rao Spline 4",
    "Age", "Gender", "Education", 
    "Log Total Visits", "Offline News Use"
  ),
  omit.stat = c("f", "ser"),
  digits = 3,
  out = "comparison_sample_3.tex"
)


#Model Plotting ----
library(dplyr)

# Model names in model_env
model_names <- c(
  "null_model", "linear_model", "quartiles_model",
  "null_model_subset_50plus", "linear_model_subset_50plus", "quartiles_model_subset_50plus",
  "null_model_subset_150plus", "linear_model_subset_150plus", "quartiles_model_subset_150plus"
)

# Function to extract model info
get_model_info <- function(name, model) {
  restriction <- case_when(
    grepl("subset_150plus", name) ~ ">= 150 (N=701)",
    grepl("subset_50plus", name) ~ ">= 50 (N=723)",
    TRUE ~ ">= 0 (N=728)"
  )
  
  model_type <- case_when(
    grepl("null", name) ~ "Null",
    grepl("linear", name) ~ "Linear",
    grepl("quartiles", name) ~ "Splines",
    grepl("clusters", name) ~ "Splines"  # Or use "Clusters" if you want to separate them
  )
  
  data.frame(
    Model = name,
    Restriction = restriction,
    Pol = "Pol",  # You said we can ignore Non-Pol, so assume all are Pol
    Model_Type = model_type,
    Adj_R2 = summary(model)$adj.r.squared
  )
}

# Apply the function to all models in model_env
model_list <- mget(model_names, envir = model_env)

# Build the data frame
df_r2 <- do.call(rbind, Map(get_model_info, names(model_list), model_list))

# Order factor levels
df_r2$Model_Type <- factor(df_r2$Model_Type, levels = c("Null", "Linear", "Splines"))
df_r2$Restriction <- factor(df_r2$Restriction, levels = c(">= 0 (N=728)", ">= 50 (N=723)", ">= 150 (N=701)"))

library(ggplot2)

# Assign color manually
color_map <- c(
  ">= 0 (N=728)" = "lightblue",
  ">= 50 (N=723)" = "deepskyblue3",
  ">= 150 (N=701)" = "blue4"
)

r2_plot <- ggplot(df_r2, aes(x = Model_Type, y = Adj_R2, group = Restriction, color = Restriction)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = color_map, name = "Active Tracking Days") +
  scale_y_continuous(limits = c(0.15, 0.2)) +
  theme_minimal(base_size = 15) +
  labs(
    title = "Adjusted R² Across Sample Groups",
    x = "Model Type",
    y = "Adjusted R²"
  )

ggsave("adjusted_r2_plot.png", plot = r2_plot, width = 10, height = 6, dpi = 600)
