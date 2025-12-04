# scripts/R/2_filter_users.R
#############################################
#  - Loads cleaned eBird trips file
#  - Filters users based on trip frequency
#  - Saves: data/scenario_dir/ebird_trips_filtered.parquet
#############################################

#-----------------------------------------------------
# Sample Selection: Restrict to users with at least 1 trip every 6 months 
#-----------------------------------------------------

# Load eBird trips
ebird <- read_parquet(inputs$ebird_trips)

# Define sample selection parameters
interval_months <- params$interval_months    # 2 for bi-monthly, 3 for quarterly, 6 for semi-annual
min_years_active <- params$min_years_active

# Compute time periods: month 1–3 = period 1, 4–6 = period 2, etc.
monthly_activity <- ebird %>%
  mutate(
    year = year(date),
    month = month(date),
    period = ceiling(month / interval_months)
  ) %>%
  distinct(user_id, year, period)

# Filter users who are active in every period within a year
# Total number of periods in a year
n_periods <- ceiling(12 / interval_months)

# Count how many unique periods each user was active per year
active_per_year <- monthly_activity %>%
  group_by(user_id, year) %>%
  summarize(n_periods_active = n_distinct(period), .groups = "drop") %>%
  filter(n_periods_active == n_periods)

# Keep users with at least N qualifying years
active_users <- active_per_year %>%
  count(user_id, name = "n_years") %>%
  filter(n_years >= min_years_active)

# Subset the main data
ebird <- ebird %>%
  semi_join(active_users, by = "user_id")

# Save filtered trips
write_parquet(ebird, outputs$ebird_trips_filtered)