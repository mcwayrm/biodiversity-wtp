# PROJECT: eBird Valuation
# PURPOSE: Explore sample selection
####################################
# Sections:

####################################

### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# # Number of trips per year per user 
# ebird <- readRDS(config$ebird_trip_clean_path)
# ebird_trips_per_year <- ebird %>%
#   group_by(user_id, year) %>%
#   summarize(trips = n(), .groups = "drop")
# quantile(ebird_trips_per_year$trips, probs = seq(0, 1, 0.1))

#-----------------------------------------------------
# 1. Load district shapefile
dist <- st_read(config$district_path) %>%
  dplyr::select(c_code_11) %>%
  rename(c_code_2011 = c_code_11)

# 2. Load ebird data
ebird <- readRDS(config$ebird_trip_clean_path)

# 3. Extract month info
monthly_activity <- ebird %>%
  mutate(month = month(date)) %>%
  distinct(user_id, year, month)

# 4. Classify user activity by temporal frequency
user_year_activity <- monthly_activity %>%
  group_by(user_id, year) %>%
  summarize(
    months = list(sort(unique(month))),
    n_months = n_distinct(month),
    has_all_months = all(1:12 %in% month),
    has_jan_june = any(month %in% 1:6),
    has_july_dec = any(month %in% 7:12),
    .groups = "drop"
  ) %>%
  mutate(activity_category = case_when(
    has_all_months ~ "Every Month",
    has_jan_june & has_july_dec ~ "Every Six Months",
    n_months >= 1 ~ "Once a Year",
    TRUE ~ "None"
  ))

# 5. Add average trip distance
user_year_activity <- user_year_activity %>%
  left_join(
    ebird %>%
      group_by(user_id, year) %>%
      summarize(avg_distance = mean(distance, na.rm = TRUE), .groups = "drop"),
    by = c("user_id", "year")
  )

# 6. Summarize for bar chart
activity_summary <- user_year_activity %>%
  group_by(activity_category) %>%
  summarize(
    users = n_distinct(user_id),
    mean_distance = mean(avg_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(activity_category = factor(
    activity_category,
    levels = c("Every Month", "Every Six Months", "Once a Year", "None")
  ))

# 7. Plot bar chart
max_users <- max(activity_summary$users)
max_dist <- max(activity_summary$mean_distance)
scaling_factor <- max_users / max_dist

ggplot(activity_summary, aes(x = activity_category)) +
  geom_col(aes(y = users), fill = "darkolivegreen3", width = 0.6) +
  geom_text(aes(y = users, label = users), 
          vjust = -0.5, size = 5, fontface = "bold") + 
  geom_line(aes(y = mean_distance * scaling_factor, group = 1),
            color = "darkgreen", size = 1.2) +
  geom_point(aes(y = mean_distance * scaling_factor),
             color = "darkgreen", size = 2) +
  scale_y_continuous(
    name = "Number of User-Years",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Avg Trip Distance (km)")
  ) +
  labs(
    title = "User Activity Categories with Avg Trip Distance",
    x = "Activity Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen")
  )
bar_chart_path <- file.path("output", "fig", "sample_selection_bar_chart_useryears.png")
ggsave(bar_chart_path, width = 10, height = 6)

# 8. Merge with ebird to get district info
user_year_district <- ebird %>%
  distinct(user_id, year, c_code_2011) %>%
  right_join(user_year_activity, by = c("user_id", "year")) %>%
  filter(!is.na(c_code_2011))

# 9. Count user-years per district and activity category
district_activity_summary <- user_year_district %>%
  group_by(c_code_2011, activity_category) %>%
  summarize(user_years = n(), .groups = "drop")

# 10. Merge with district shapefile
dist_summary <- dist %>%
  left_join(district_activity_summary, by = "c_code_2011")

# 11. Plot map: Facet by category
dist_summary %>%
  filter(!is.na(activity_category)) %>%
  ggplot() +
  geom_sf(aes(fill = user_years), color = NA) +
  scale_fill_viridis_c(na.value = "grey90", option = "C") +
  facet_wrap(~ activity_category, ncol = 3) +
  labs(
    title = "User-Years per District by Activity Frequency",
    fill = "User-Years"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )
map_path <- file.path("output", "fig", "sample_selection_map_useryears.png")
ggsave(map_path, width = 10, height = 6)




#### NOW WITH AVG TRIPS
# 1. Calculate Avg Trips Per Year per User
user_avg_trips <- ebird %>%
  group_by(user_id, year) %>%
  summarize(
    n_trips = n(),
    avg_distance_year = mean(distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(user_id) %>%
  summarize(
    avg_trips_per_year = mean(n_trips),
    avg_distance = mean(avg_distance_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(trip_category = case_when(
    avg_trips_per_year >= 50 ~ "Fifty or more per Year",
    avg_trips_per_year >= 25 ~ "Twenty Five per Year",
    avg_trips_per_year >= 10 ~ "Ten per Year",
    avg_trips_per_year >= 5 ~ "Five per Year",
    avg_trips_per_year >= 2 ~ "Two per Year",
    avg_trips_per_year >= 1 ~ "Once a Year",
    TRUE ~ "None"
  )) %>%
  mutate(trip_category = factor(
    trip_category,
    levels = c("Fifty or more per Year", "Twenty Five per Year", "Ten per Year", 
               "Five per Year", "Two per Year", "Once a Year", "None")
  ))

# Summarize for Plotting
activity_summary <- user_avg_trips %>%
  group_by(trip_category) %>%
  summarize(
    users = n(),
    mean_distance = mean(avg_distance, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Bar Chart with Labels
# Rescale distance for plotting
max_users <- max(activity_summary$users)
max_dist <- max(activity_summary$mean_distance)
scaling_factor <- max_users / max_dist

ggplot(activity_summary, aes(x = trip_category)) +
  geom_col(aes(y = users), fill = "darkolivegreen3", width = 0.6) +
  geom_text(aes(y = users, label = users), vjust = -0.5, fontface = "bold") +
  geom_line(aes(y = mean_distance * scaling_factor, group = 1), 
            color = "darkgreen", size = 1.2) +
  geom_point(aes(y = mean_distance * scaling_factor), 
             color = "darkgreen", size = 2) +
  scale_y_continuous(
    name = "Number of Users",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Avg Trip Distance (km)")
  ) +
  labs(
    title = "User Activity Groups by Avg Trips and Trip Distance",
    x = "Trip Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.text.y.right = element_text(color = "darkgreen")
  )
bar_chart_path <- file.path("output", "fig", "sample_selection_bar_chart.png")
ggsave(bar_chart_path, width = 10, height = 6)

# 4. Map by District and Category 
ebird_with_districts <- ebird %>%
  filter(!is.na(c_code_2011)) %>%
  distinct(user_id, c_code_2011)

# Assign users to their *primary* district
user_district_assignment <- ebird %>%
  filter(!is.na(c_code_2011)) %>%
  group_by(user_id, c_code_2011) %>%
  tally(sort = TRUE) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup()

# Merge this assignment with the trip categories
user_trips_with_districts <- user_avg_trips %>%
  right_join(user_district_assignment, by = "user_id") %>%
  filter(!is.na(c_code_2011))

# Create a full grid of all districts × all categories
all_districts <- dist %>% st_drop_geometry() %>% select(c_code_2011)
all_categories <- user_avg_trips %>% distinct(trip_category)

full_grid <- expand_grid(c_code_2011 = all_districts$c_code_2011,
                         trip_category = all_categories$trip_category)

# 
summary_raw <- user_trips_with_districts %>%
  group_by(c_code_2011, trip_category) %>%
  summarize(users = n_distinct(user_id), .groups = "drop")

# Join with full grid and replace missing with 0
dist_summary <- full_grid %>%
  left_join(summary_raw, by = c("c_code_2011", "trip_category")) %>%
  mutate(users = replace_na(users, 0)) %>%
  left_join(dist, by = "c_code_2011") %>%
  st_as_sf()

dist_summary %>%
  ggplot() +
  geom_sf(aes(fill = users), color = NA) +
  scale_fill_viridis_c(na.value = "grey90", option = "C") +
  facet_wrap(~ trip_category, ncol = 3) +
  labs(
    title = "Average Trip Frequency per Year by District and Category",
    fill = "Users"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )
map_path <- file.path("output", "fig", "sample_selection_map.png")
ggsave(map_path, width = 10, height = 6)
