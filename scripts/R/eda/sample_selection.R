# PROJECT: eBird Valuation
# PURPOSE: Explore sample selection
####################################
# Sections:

####################################

### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# Read ebird trips
ebird <- readRDS(config$ebird_trip_clean_path)

# Number of trips per year per user 
ebird_trips_per_year <- ebird %>%
  group_by(user_id, year) %>%
  summarize(trips = n(), .groups = "drop")

quantile(ebird_trips_per_year$trips, probs = seq(0, 1, 0.1))

ggplot(ebird_trips_per_year[ebird_trips_per_year$trips > 6,], aes(x = trips)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Trip Frequency Density",
       x = "Number of Trips per Year",
       y = "Density") +
  theme_minimal()

# How many users are active by month?
user_month_counts <- ebird %>%
  group_by(user_id, year) %>%
  summarize(months_with_activity = n_distinct(month), .groups = "drop")
# categorize
user_month_counts <- user_month_counts %>%
  mutate(activity_category = case_when(
    months_with_activity == 12 ~ "Every Month",
    months_with_activity >= 6 ~ "Every Other Month",
    months_with_activity >= 4 ~ "Every Three Months",
    months_with_activity >= 2 ~ "Every Six Months",
    months_with_activity >= 1 ~ "Once a Year",
    TRUE ~ "None"
  ))
# count users in each category
user_month_counts %>%
  group_by(activity_category) %>%
  summarize(users = n_distinct(user_id)) %>%
  arrange(factor(activity_category, 
                 levels = c("Every Month", "Every Other Month", "Every Three Months", 
                            "Every Six Months", "Once a Year", "None")))
