# scripts/R/2_filter_users.R
#############################################
#  - Loads cleaned eBird trips file
#  - Filters users based on trip frequency
#  - Saves: data/scenario_dir/ebird_trips_filtered.parquet
#############################################

#-----------------------------------------------------
# Sample Selection: User stratification by activity rate
#-----------------------------------------------------

# Load eBird trips
ebird <- read_parquet(inputs$ebird_trips)
setDT(ebird)

# Calculate number of unique months per user
user_months <- ebird[, .(n_months = uniqueN(yearmonth)), by = user_id]

# Calculate activity rate (out of 24 months)
user_months[, user_activity_rate := n_months / 24]

# Calculate the median activity rate across all users
median_rate <- median(user_months$user_activity_rate, na.rm = TRUE)

# Assign user_type:
#   - "above_median" for users at or above median activity rate
#   - "below_median" for users below median activity rate
user_months[, user_type := fifelse(
  user_activity_rate >= median_rate, "above_median", "below_median"
)]

# Merge user_type back to main data
ebird <- merge(ebird, user_months[, .(user_id, user_activity_rate, user_type)], by = "user_id", all.x = TRUE)

# Subset the main data based on parameter
if (params$user_type != "full") {
  ebird <- ebird[user_type == params$user_type]
}

# Save filtered trips
write_parquet(ebird, outputs$ebird_trips_filtered)