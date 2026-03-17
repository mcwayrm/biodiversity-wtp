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
setDT(ebird)

# Calculate number of unique months per user
user_months <- ebird[, .(n_months = uniqueN(yearmonth)), by = user_id]

# Calculate activity rate (out of 24 months)
user_months[, user_activity_rate := n_months / 24]

# Calculate the mean activity rate across all users
mean_rate <- mean(user_months$user_activity_rate, na.rm = TRUE)

# Get all user activity rates that are greater than or equal to the mean
above_mean <- user_months$user_activity_rate[user_months$user_activity_rate >= mean_rate]

# Calculate the median of the above-mean activity rates
med_above_mean <- median(above_mean, na.rm = TRUE)

# Assign user_type:
#   - "L" for users below the mean activity rate
#   - "M" for users between the mean and the median of above-mean rates
#   - "H" for users at or above the median of above-mean rates
user_months[, user_type := fifelse(
  user_activity_rate < mean_rate, "L",
  fifelse(user_activity_rate < med_above_mean, "M", "H")
)]

# Merge user_type back to main data
ebird <- merge(ebird, user_months[, .(user_id, user_activity_rate, user_type)], by = "user_id", all.x = TRUE)

# Subset the main data
ebird <- ebird[user_type == params$user_type]

# Save filtered trips
write_parquet(ebird, outputs$ebird_trips_filtered)