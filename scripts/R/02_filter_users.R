# scripts/R/2_filter_users.R
#############################################
#  - Loads cleaned eBird trips file
#  - Filters users based on trip frequency (user_type)
#  - Optionally filters to users active in the panel's baseline year
#    (baseline_2015_users_only), for the stable-sample robustness check
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

# Subset the main data based on parameter. Computed against the FULL
# population's activity rate/median above, before any other filtering --
# this keeps "above_median"/"below_median" meaning the same thing (relative
# to the true full population) regardless of whether baseline_2015_users_only
# is also applied below.
if (params$user_type != "full") {
  ebird <- ebird[user_type == params$user_type]
}

# Optional: stable-sample robustness check -- restrict to users who were
# already active in the panel's baseline year (2015), regardless of
# user_type. Moved here from a later pipeline stage so every downstream
# task (distance calc, choice-set construction, site attributes,
# biodiversity metrics, travel cost) operates on the smaller filtered
# sample directly, rather than carrying the full population through the
# whole pipeline only to discard most of it at the very last step.
#
# Year is derived from yearmonth's leading 4 characters rather than
# assuming a specific date type -- works whether yearmonth is stored as a
# "YYYY-MM" character string or a Date (as.character(Date) also yields
# "YYYY-MM-DD", same leading 4 characters either way).
if (isTRUE(params$baseline_2015_users_only)) {
  ebird_year <- substr(as.character(ebird$yearmonth), 1, 4)
  baseline_users <- unique(ebird$user_id[ebird_year == "2015"])
  n_before <- uniqueN(ebird$user_id)
  ebird <- ebird[user_id %in% baseline_users]
  n_after <- uniqueN(ebird$user_id)
  message(sprintf("baseline_2015_users_only=TRUE: kept %d/%d users active in 2015", n_after, n_before))
}

# Save filtered trips
write_parquet(ebird, outputs$ebird_trips_filtered)