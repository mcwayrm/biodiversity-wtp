#!/usr/bin/env Rscript
# scripts/R/10_compute_iv.R
#############################################
#  - Generates the instrumental variable for flu outbreak exposure
#  - Saves: outputs$master_data_with_iv
#
#  Required params:
#    - projection_crs: CRS for spatial operations

#  Required inputs:
#    - master_data_with_travel_cost: Output from stage 9

#############################################



#                                 PART 1
#===============================================================================
# SECTION 1: Load eBirdLife Asia polygons and clean geometries
#   - Read GPKG
#   - Ensure sf object
#   - Fix invalid geometries
#   - Collapse seasonal polygons to annual range per species
#===============================================================================

birdlife_path <- "C:/Users/laswa/Downloads/birdlife_asia.gpkg"
sf::sf_use_s2(FALSE) # Turn off s2 by avoiding issues with messy polygons/topology
birdlife_asia <- st_read(birdlife_path, layer = "birdlife_asia")
birdlife_asia <- st_as_sf(birdlife_asia) # sf object

# Fix invalid geometries
birdlife_asia <- birdlife_asia |>
  sf::st_make_valid()
# Check that geometries are now valid
all_valid <- all(sf::st_is_valid(birdlife_asia))
cat("All geometries valid? ", all_valid, "\n")

#-----------------------------
# Collapse seasons -> annual:
#     - One geometry per species (sci_name)
#     - Union of all seasonal geometries per species
#     - Work in planar CRS to avoid topology warnings, then back to WGS84
#-----------------------------
birdlife_asia <- suppressWarnings(
  birdlife_asia |>
    group_by(sci_name) |>
    summarise(
      seasonal = "annual",
      .groups = "drop"
    )
)

# Check duplicates by species
birdlife_asia %>%
  count(sci_name) %>%
  filter(n > 1) %>%
  arrange(desc(n))


#===============================================================================
# SECTION 2: Load and clean species migratory-status data
#   - Read classified migratory data
#   - Recode to "Resident" / "Migrant"
#   - Clean scientific names
#   - Deduplicate by sci_name
#===============================================================================

# Load migratory data
migration_path <- "C:/Users/laswa/Downloads/species_list_categorized.csv"
species_migration <- read_csv(migration_path)

#-----------------------------
# Recode migratory_status into "Resident" / "Migrant"
#     - Resident: resident/local/india/altitudinal/nomadic
#     - Migrant: passage/winter/summer migrant
#     - Otherwise: keep original label
#-----------------------------
species_migration <- species_migration %>%
  mutate(
    migratory_status = case_when(
      grepl("resident|local|india|altitudinal|nomadic", 
            migratory_status, ignore.case = TRUE) ~ "Resident",
      
      grepl("passage migrant|winter migrant|summer migrant", 
            migratory_status, ignore.case = TRUE) ~ "Migrant",
      
      TRUE ~ migratory_status
    )
  )

#-----------------------------
# Spelling cleaning of sci_name
#     - Remove extra spaces
#     - Lowercase, then capitalize genus
#     - Split genus/species and recombine
#-----------------------------
species_migration <- species_migration %>%
  mutate(
    # Clean extra spaces + lowercase everything
    sci_name = str_squish(str_to_lower(sci_name)),
    
    # Split genus + species
    genus   = str_extract(sci_name, "^[^ ]+"),
    species = str_extract(sci_name, "(?<= )[A-Za-z\\-]+"),
    
    # Capitalize genus only (first letter uppercase)
    genus = str_to_title(genus),
    
    # Recombine into standardized "Genus species"
    sci_name = paste(genus, species)
  )

# Check duplicates by sci_name
species_migration |>
  count(sci_name) |>
  filter(n > 1) |>
  arrange(desc(n))

species_migration <- species_migration %>%
  group_by(sci_name) %>%
  slice(1) %>%     # Keep the first occurrence
  ungroup()


#===============================================================================
# SECTION 3: Merge migratory-status data with BirdLife polygons
#   - Left join keeps all spatial polygons
#   - Then incorporate manually classified missing species
#===============================================================================

merged_data <- birdlife_asia %>%
  left_join(species_migration, by = "sci_name", relationship = "one-to-one")
st_crs(merged_data)

# Load additional manual classifications for missing species
missing_small_path <- "C:/Users/laswa/Downloads/missing_migration.csv"
missing_small <- readr::read_csv(missing_small_path, show_col_types = FALSE)

# Recode new_migratory_status:
#   - Altitudinal Migrant / Nomadic / Residdent -> "Resident"
missing_small <- missing_small %>%
  mutate(
    new_migratory_status = case_when(
      grepl("Altitudinal Migrant|Nomadic|Residdent", 
            new_migratory_status, ignore.case = TRUE) ~ "Resident",
      TRUE ~ new_migratory_status
    )
  )

# Remove duplicates by sci_name in this manual list
missing_small <- missing_small %>%
  dplyr::distinct(sci_name, .keep_all = TRUE)

# Quick table of new_migratory_status
table(missing_small$new_migratory_status[ !is.na(missing_small$new_migratory_status) ] )

#-----------------------------
# Merge manual classifications into merged_data
#-----------------------------
merged_data <- merged_data %>%
  left_join(missing_small, by = "sci_name")

# Replace missing migratory_status with new_migratory_status where available
merged_data <- merged_data %>%
  mutate(
    migratory_status = if_else(
      is.na(migratory_status) & !is.na(new_migratory_status),
      new_migratory_status,
      migratory_status
    )
  )

# Drop helper column
merged_data <- merged_data %>%
  select(-new_migratory_status)

# Check counts of final migratory_status
table(merged_data$migratory_status[ !is.na(merged_data$migratory_status) ] )
table(is.na(merged_data$migratory_status))

#-----------------------------
# Ensure merged_data is sf with geometry column "geom"
#     (if already sf, this will be skipped)
#-----------------------------
if (!inherits(merged_data, "sf")) {
  merged_data <- st_as_sf(merged_data, sf_column_name = "geom")
}


#===============================================================================
# SECTION 4: Compute polygon-based range areas and fill missing "range"
#   - Reproject to equal-area CRS (6933)
#   - Compute area in km² for each polygon
#   - Fill missing "range" with polygon area
#   - Save final merged_data
#===============================================================================

#-----------------------------
# Transform to equal-area CRS and compute area
#     - EPSG:6933 = World Cylindrical Equal Area
#-----------------------------
merged_data_aea <- st_transform(merged_data, crs = 6933)

# Area of each polygon in km²
merged_data_aea$range_from_poly_km2 <- as.numeric(st_area(merged_data_aea)) / 1e6

#-----------------------------
# Attach area back to original data and construct range_filled
#     - range_from_poly_km2: area from geometry
#     - range_filled: original "range" with NAs replaced by polygon area
#-----------------------------
merged_data <- merged_data %>%
  mutate(
    # Store polygon-based area
    range_from_poly_km2 = merged_data_aea$range_from_poly_km2,
    # Keep original range but fill NAs with polygon area
    range_filled = if_else(
      is.na(range),
      range_from_poly_km2,
      range
    )
  )

merged_data <- merged_data[, c("sci_name",
                               "seasonal",
                               "migratory_status",
                               "range_from_poly_km2",
                               "geom")]

saveRDS(
  merged_data,
  file = "C:/Users/laswa/Downloads/merged_data.rds"
)

# Quick checks
table(merged_data$migratory_status[ !is.na(merged_data$migratory_status) ] )
unique(merged_data$migratory_status)

#===============================================================================
# SECTION 5: Restrict to migratory species and compute
#            share of range overlapping India
#   - Keep migratory species only
#   - Assign species_id
#   - Build India union polygon
#   - Compute overlap area with India
#   - Compute frac_india = area_overlap_km2 / India area
#===============================================================================

india_districts <- st_read(
  "C:/Users/laswa/OneDrive/Documents/GitHub/biodiversity-wtp/data/shp/district-2011/district-2011.shp"
)

india_union <- india_districts %>%
  st_make_valid() %>%
  st_union()

india_union_sf <- st_sf(india_id = 1, geometry = india_union)
sf::sf_use_s2(FALSE)

# Rebuild species_sf (fresh, from merged_data)
species_sf <- merged_data %>%
  filter(migratory_status == "Migrant") %>%
  select(sci_name, migratory_status, geom)

species_sf <- species_sf[!st_is_empty(species_sf), ] # # Drop empty geometries
valid_flag <- st_is_valid(species_sf)
table(valid_flag)  # count valid vs invalid
species_sf_valid <- species_sf[valid_flag, ]

target_crs <- 6933  # Equal-area CRS
species_aea <- species_sf_valid %>%
  st_transform(target_crs) %>%
  mutate(species_id = dplyr::row_number())

india_aea <- india_union_sf %>%
  st_make_valid() %>%           
  st_transform(target_crs)

india_area_km2 <- as.numeric(st_area(india_aea)) / 1e6

#-----------------------------
# Species–India overlap: compute area_overlap_km2 and share frac_india
#-----------------------------
make_valid <- function(x) {
  if ("st_make_valid" %in% getNamespaceExports("sf")) {
    return(sf::st_make_valid(x))
  }
  if ("st_make_valid" %in% getNamespaceExports("lwgeom")) {
    return(lwgeom::st_make_valid(x))
  }
  g  <- lwgeom::lwgeom_make_valid(sf::st_geometry(x))  # this is sfc
  sf::st_set_geometry(x, g)
}

target_crs <- 6933  # equal-area CRS

species_aea <- species_sf_valid %>%
  make_valid() %>%               # fix before transform
  st_transform(target_crs) %>%
  make_valid() %>%               # fix again after transform
  mutate(species_id = dplyr::row_number())

# India polygon in equal-area CRS, all valid
india_aea <- india_union_sf %>%
  make_valid() %>%
  st_transform(target_crs) %>%
  make_valid()

# Intersection between each species range and India polygon
species_india_int <- st_intersection(
  species_aea %>% select(species_id, sci_name),
  india_aea
)

# Summarize overlap area per species (km²)
species_india_overlap <- species_india_int %>%
  mutate(area_overlap_km2 = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  group_by(species_id, sci_name) %>%
  summarise(area_overlap_km2 = sum(area_overlap_km2), .groups = "drop")

#-----------------------------
#     - area_overlap_km2: overlap area in India
#     - frac_india: overlap share relative to total India area
#-----------------------------
species_with_share <- species_aea %>%
  st_drop_geometry() %>%
  select(species_id, sci_name, migratory_status) %>%
  left_join(species_india_overlap, by = c("species_id", "sci_name")) %>%
  mutate(
    area_overlap_km2 = ifelse(is.na(area_overlap_km2), 0, area_overlap_km2),
    frac_india       = area_overlap_km2 / india_area_km2
  )



#                                 PART 2
#==============================================================
# gamma_j and Shift_t construction
# Steps:
#   1) Build gamma_j for each origin country j:
#        gamma_j = (1 / S_j) * sum_s frac_india_s
#      where S_j is # species occurring in country j
#   2) Build H5N1_{jt} = outbreak intensity by country-year
#   3) Build Shift_t = sum_j H5N1_{jt} * gamma_j (yearly series)
#==============================================================

sf::sf_use_s2(FALSE)     # turn off s2 for more stable geometry ops (intersections, unions, etc.)
target_crs <- 6933       # equal-area CRS (match what you used for species_aea / India)

flu_raw <- readr::read_csv(
  "C:/Users/laswa/Downloads/flu_outbreaks_with_country.csv",
  col_types = readr::cols(.default = readr::col_character())
) # Load and clean flu outbreak data

# Parse dates + convert numeric/id columns
flu_df <- flu_raw %>%
  dplyr::mutate(
    startDate_chr = trimws(startDate),
    endDate_chr   = trimws(endDate),
    # Parse ISO dates "YYYY-MM-DD"
    startDate = lubridate::ymd(startDate_chr),
    endDate   = lubridate::ymd(endDate_chr),
    # Convert IDs and outbreak/case counts
    outbreakId     = as.integer(outbreakId),
    reportId       = as.integer(reportId),
    totalOutbreaks = as.numeric(totalOutbreaks),
    totalCases     = as.numeric(totalCases)
  ) %>%
  dplyr::select(
    outbreakId, reportId, startDate, endDate,
    totalOutbreaks, totalCases, locationName,
    animalCategory,   # <-- confirm this matches your CSV column name
    country
  )

# Create year variable (country-year panel for outbreaks)
flu_df_year <- flu_df %>%
  dplyr::mutate(year = lubridate::year(startDate))

# Sanity checks: years should be real values (not all NA)
flu_df_year %>% dplyr::select(startDate, year) %>% head()
unique(flu_df_year$year)

# List unique flu countries (non-missing)
flu_countries <- flu_df %>%
  dplyr::distinct(country) %>%
  dplyr::filter(!is.na(country))

flu_countries

# Load Natural Earth countries (admin-0)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Clean geometries + keep only name + geometry
world_countries <- world %>%
  sf::st_make_valid() %>%
  dplyr::select(country = name, geometry)

# Subset to only countries that appear in the flu data
flu_countries_sf <- world_countries %>%
  dplyr::filter(country %in% flu_countries$country)

# Checking mismatches (flu country names that don't match Natural Earth)
setdiff(flu_countries$country, flu_countries_sf$country)

# Reproject to the same equal-area CRS as species_aea
countries_aea <- flu_countries_sf %>%
  sf::st_transform(target_crs)

#==============================================================
# Species–country incidence (which species occur in each country j)
#    - intersect species polygons with country polygons
#    - produce a unique mapping: (species_id, sci_name, country)
#==============================================================


species_country_int <- sf::st_intersection(
  species_aea %>% dplyr::select(species_id, sci_name),
  countries_aea
) # Spatial intersection

# Drop geometry and keep unique species-country pairs
species_country <- species_country_int %>%
  sf::st_drop_geometry() %>%
  dplyr::distinct(species_id, sci_name, country)

#==============================================================
# Compute gamma_j for each origin country j (excluding India)
#    gamma_j = mean(frac_india) across species present in j
#    where frac_india is species' share of range overlapping India
#==============================================================

gamma_j_df <- species_with_share %>%
  dplyr::inner_join(species_country, by = c("species_id", "sci_name")) %>%
  dplyr::filter(country != "India") %>%  # exclude India as origin j
  dplyr::group_by(country) %>%
  dplyr::summarise(
    S_j     = dplyr::n_distinct(sci_name),  # number of distinct species in country j
    gamma_j = mean(frac_india),             # = sum(frac_india)/S_j
    .groups = "drop"
  )
gamma_j_df

#==============================================================
# Build H5N1_{jt}: outbreak intensity by country-year
#    - sum totalOutbreaks within each country-year
#    - exclude India as origin and drop missing years
#==============================================================

H5N1_jt <- flu_df_year %>%
  dplyr::filter(country != "India", !is.na(year)) %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(
    H5N1_jt = sum(totalOutbreaks, na.rm = TRUE),
    .groups = "drop"
  )
H5N1_jt

#==============================================================
# Merge gamma_j with H5N1_{jt}, then compute Shift_t
#    Shift_t = sum_j [ H5N1_{jt} * gamma_j ]
#==============================================================

# Merge gamma_j into country-year outbreak panel
H5N1_gamma <- H5N1_jt %>%
  dplyr::left_join(gamma_j_df, by = "country")
head(H5N1_gamma)

# Aggregate to year: Shift_t exposure series
Shift_t <- H5N1_gamma %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    Shift_t = sum(H5N1_jt * gamma_j, na.rm = TRUE),
    .groups = "drop"
  )
Shift_t  #final yearly exposure series


#                                 PART 3
#==============================================================
# Construct Share_d (migratory overlap for each hotspot) and IV_dt
# Steps:
#   1) Compute Share_d for each hotspot polygon d:
#        Share_d = (# migratory species overlapping hotspot d) / (# total species overlapping d)
#   2) Prepare hotspot polygons with Share_d merged in
#==============================================================

sf::sf_use_s2(FALSE)      # helps avoid s2 geometry issues; often faster/more stable for intersections
crs_target <- 6933        # Equal-area CRS (meters); keep consistent across spatial operations
path <- "C:/Users/laswa/Downloads/ebird_hotspots_voronoi.gpkg"
layers_info <- sf::st_layers(path)
print(layers_info)
ebird_hotspot <- sf::st_read(path, layer = "cluster_voronoi_limited") # Load eBird hotspot Voronoi polygons (hotspots)

merged_proj <- sf::st_transform(merged_data, crs_target)
ebird_proj  <- sf::st_transform(ebird_hotspot, crs_target)

# Sanity check CRS consistency
sf::st_crs(merged_proj)
sf::st_crs(ebird_proj)    # both should be EPSG:6933 (units = meters)

#==============================================================
# Build species–hotspot overlap table using spatial index
# Approach:
#   - st_intersects() returns, for each species polygon i,
#     the indices of hotspot polygons it overlaps.
#==============================================================


idx <- sf::st_intersects(merged_proj, ebird_proj) # Spatial intersection index: for each species polygon, which hotspots overlap
lengths(idx) |> summary() # Quick diagnostics: number of hotspot overlaps per species polygon

# Expand the index into a long species–hotspot table
#     Output columns:
#       sci_name, migratory_status, cluster_id
species_hotspot <- lapply(seq_along(idx), function(i) {
  # If species i overlaps no hotspot, skip it
  if (length(idx[[i]]) == 0L) return(NULL)

  data.frame(
    sci_name         = merged_proj$sci_name[i],
    migratory_status = merged_proj$migratory_status[i],
    cluster_id       = ebird_proj$cluster_id[idx[[i]]],
    stringsAsFactors = FALSE
  )
}) |>
  dplyr::bind_rows()

#==============================================================
# HOTSPOT METADATA: bring in n_hotspots (one per cluster_id)
#     We assume ebird_proj has columns: cluster_id, n_hotspots, geom
#==============================================================
hotspot_meta <- ebird_proj |>
  sf::st_drop_geometry() |>
  dplyr::select(cluster_id, n_hotspots) |>
  dplyr::distinct()

# Quick check
head(hotspot_meta)
nrow(hotspot_meta)          # should be number of unique hotspots
dplyr::n_distinct(hotspot_meta$cluster_id)

#==============================================================
# Compute Share_d for each hotspot d
#
# Definitions:
#   S_d               = number of distinct species overlapping hotspot d
#   migratory_species = number of distinct migratory species overlapping d
#   share_migratory_d = migratory_species / S_d
#   n_hotspots        = number of raw hotspots in cluster d (from hotspot_meta)
#==============================================================
share_by_hotspot <- species_hotspot %>%
  dplyr::group_by(cluster_id) %>%
  dplyr::summarise(
    S_d               = dplyr::n_distinct(sci_name),
    migratory_species = dplyr::n_distinct(sci_name[migratory_status == "Migrant"]),
    share_migratory_d = migratory_species / S_d,
    .groups = "drop"
  ) %>%
  # attach n_hotspots here so it propagates forward
  dplyr::left_join(hotspot_meta, by = "cluster_id")

summary(share_by_hotspot$share_migratory_d)

#==============================================================
# Attach Share_d back to hotspot polygons (for mapping / later merges)
#     (ebird_proj already has n_hotspots, so we just add the share stats)
#==============================================================

ebird_hotspot_share <- ebird_proj %>%
  dplyr::left_join(
    share_by_hotspot %>%
      dplyr::select(cluster_id, S_d, migratory_species, share_migratory_d),
    by = "cluster_id"
  )

# quick check
ebird_hotspot_share %>%
  dplyr::select(cluster_id, n_hotspots, S_d, migratory_species, share_migratory_d) %>%
  head()

#==============================================================
# Load India polygon and reproject (for later IV steps)
#==============================================================

india <- rnaturalearth::ne_countries(scale = "medium",
                                     country = "India",
                                     returnclass = "sf")
india_6933 <- sf::st_transform(india, sf::st_crs(ebird_hotspot_share))
sf::st_crs(india_6933)

###############################################################################
# SHIFT–SHARE IV CONSTRUCTION: IV_dt = Share_d × Shift_t
#   1) share_by_hotspot  (hotspot-level exposure)
#        - One row per hotspot d
#        - Key columns:
#            cluster_id
#            share_migratory_d   = Share_d (migratory species share in hotspot d)
#
#   2) Shift_t           (time-varying global shock)
#        - One row per year t
#        - Key columns:
#            year
#            Shift_t            = global migratory shock in year t
#
# OUTPUT:
#   IV_dt (hotspot-year panel)
#        - One row per (cluster_id, year)
#        - Key columns:
#            cluster_id, year, share_migratory_d, Shift_t, IV_dt
#
# INTERPRETATION:
#   - Share_d (exposure): how “migrant-heavy” hotspot d is (time-invariant)
#   - Shift_t (shock):   annual global migratory shock (common across hotspots)
#   - IV_dt: predicted hotspot-level shock from global variation, scaled by exposure
###############################################################################
dplyr::n_distinct(share_by_hotspot$cluster_id)
dplyr::n_distinct(Shift_t$year)

share_by_hotspot %>%
  dplyr::count(cluster_id) %>%
  dplyr::filter(n > 1)

Shift_t %>%
  dplyr::count(year) %>%
  dplyr::filter(n > 1)

# Create the full hotspot-year panel
IV_dt <- tidyr::expand_grid(
  cluster_id = share_by_hotspot$cluster_id,
  year       = Shift_t$year
)

# Merge hotspot exposure (Share_d) AND n_hotspots into panel
IV_dt <- IV_dt %>%
  dplyr::left_join(
    share_by_hotspot %>%
      dplyr::select(cluster_id, share_migratory_d, n_hotspots),
    by = "cluster_id"
  )

# Merge annual shocks (Shift_t) into panel
IV_dt <- IV_dt %>%
  dplyr::left_join(Shift_t, by = "year")

# Construct the shift–share IV: IV_dt = Share_d × Shift_t
IV_dt <- IV_dt %>%
  dplyr::mutate(
    IV_dt = share_migratory_d * Shift_t
  )

head(IV_dt)
dplyr::glimpse(IV_dt)       # <-- should now show cluster_id, year, n_hotspots, ...

summary(IV_dt$share_migratory_d)
summary(IV_dt$Shift_t)
summary(IV_dt$IV_dt)

dplyr::n_distinct(IV_dt$cluster_id)
dplyr::n_distinct(IV_dt$year)

sum(is.na(IV_dt$share_migratory_d))
sum(is.na(IV_dt$Shift_t))
sum(is.na(IV_dt$IV_dt))

#-----------------------------
# 6) Save final data output
#-----------------------------
saveRDS(IV_dt, "C:/Users/laswa/Downloads/IV_dt.rds")




#                                 PART 4
###############################################################################
# MAPS OF MIGRATORY SHARE AND IV VOLATILITY ACROSS EBIrd HOTSPOTS IN INDIA
###############################################################################

#==============================================================================
# MAP A — Migratory Share (Share_d) across eBird Hotspot Voronoi Polygons
#   - Hotspots appear as Voronoi polygons, not points
#   - Fill: share_migratory_d = share of species that are migratory
#==============================================================================

p_share <- ggplot() +
  geom_sf(
    data  = india_6933,
    fill  = "grey90",
    color = "white",
    size  = 0.1
  ) +
  geom_sf(
    data  = ebird_hotspot_share,
    aes(fill = share_migratory_d),
    color = NA,
    alpha = 0.9
  ) +
  scale_fill_viridis_c(
    name      = "Migratory Share",
    option    = "plasma",
    direction = -1,
    limits    = c(
      min(share_by_hotspot$share_migratory_d, na.rm = TRUE),
      max(share_by_hotspot$share_migratory_d, na.rm = TRUE)
    )
  ) +
  labs(
    title    = "Migratory Exposure of Birding Hotspots in India"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank()
  )

ggsave(
  filename = "p_share.png",
  plot     = p_share,
  device   = "png",
  width    = 10,
  height   = 8,
  dpi      = 300,
  bg       = "white",
  path     = "C:/Users/laswa/Downloads/"
)

p_share

# Interpretation:
# Between 0.2 and 0.5, a value of 0.2 represents a lower migratory share
# (low exposure, with only about 20% of species being migratory), while 0.5
# represents a higher migratory share (high exposure, with around 50% of
# species being migratory). On the map, yellow areas correspond to low
# migratory shares (≈0.2), whereas purple/blue areas indicate high
# migratory shares (≈0.5). Thus, purple areas host more migratory species,
# while yellow areas host fewer.


#==============================================================================
# VOLATILITY OF IV_dt OVER TIME BY HOTSPOT
#   - Compute volatility (SD of IV_dt) for each hotspot over 2005–2025
#   - Rank hotspots into deciles of volatility
#==============================================================================

iv_volatility <- IV_dt %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(cluster_id) %>%
  summarise(
    iv_sd   = sd(IV_dt, na.rm = TRUE),   # volatility (standard deviation)
    iv_mean = mean(IV_dt, na.rm = TRUE), # mean exposure 
    n_years = sum(!is.na(IV_dt)),        # number of years with non-missing IV_dt
    .groups = "drop"
  ) %>%
  mutate(
    vol_rank = ntile(iv_sd, 10),         # decile rank of volatility
    high_vol = vol_rank == 10            # TRUE if top 10% (most volatile)
  )

# JOIN VOLATILITY METRICS BACK TO VORONOI POLYGONS
hotspot_volatility <- ebird_hotspot_share %>%
  left_join(iv_volatility, by = "cluster_id")


#==============================================================================
# MAP B — Continuous Volatility Map
#   - Fill: iv_sd (standard deviation of IV_dt over 2005–2025)
#==============================================================================

p_volatility <- ggplot() +
  geom_sf(
    data  = india_6933,
    fill  = "grey90",
    color = "white",
    size  = 0.1
  ) +
  geom_sf(
    data  = hotspot_volatility,
    aes(fill = iv_sd),
    color = NA,
    alpha = 0.9
  ) +
  scale_fill_viridis_c(
    name   = "Volatility (measured in SD)",
    option = "plasma"
  ) +
  labs(
    title    = "Volatility of Global Migratory Exposure of Birding Hotspots in India"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank()
  )

ggsave(
  filename = "p_volatility.png",
  plot     = p_volatility,
  device   = "png",
  width    = 10,
  height   = 8,
  dpi      = 300,
  bg       = "white",
  path     = "C:/Users/laswa/Downloads/"
)

p_volatility

# Interpretation:
# The map reveals substantial geographic heterogeneity in the volatility of
# global migratory exposure across birding hotspots in India between 2005 and
# 2025. Hotspots in the northern Himalayas, the northeastern states, and the
# western coastal belt exhibit the highest volatility (shown in yellow and
# orange). This indicates that the number or composition of migratory species
# visiting these regions fluctuates more dramatically over time, likely because
# these areas lie along major international flyways and serve as seasonal
# stopover or bottleneck zones that are sensitive to global climatic, ecological,
# and habitat changes along migration routes.
#
# By contrast, hotspots in central and southern peninsular India show much lower
# volatility (shown in deep purple). These areas tend to host more resident
# species and fewer long-distance migrants, meaning their exposure to global
# migratory dynamics is more stable and less influenced by shocks originating
# outside India.

#==============================================================================
# MAP C — Top 10% Most Volatile Hotspots
#   - Show only hotspots in the top volatility decile (high_vol == TRUE)
#==============================================================================

p_top_decile <- ggplot() +
  geom_sf(
    data  = india_6933,
    fill  = "grey90",
    color = "white",
    size  = 0.1
  ) +
  geom_sf(
    data  = hotspot_volatility %>% filter(high_vol),
    aes(fill = iv_sd),
    color = NA,
    alpha = 0.95
  ) +
  scale_fill_viridis_c(
    name   = "Top 10% volatility",
    option = "plasma"
  ) +
  labs(
    title    = "Hotspots Facing the Most Volatile Global Migratory Exposure"
   ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text       = element_blank(),
    axis.title      = element_blank(),
    panel.grid      = element_blank()
  )

ggsave(
  filename = "p_top_decile.png",
  plot     = p_top_decile,
  device   = "png",
  width    = 10,
  height   = 8,
  dpi      = 300,
  bg       = "white",
  path     = "C:/Users/laswa/Downloads/"
)

p_top_decile

# Interpretation:
# This map isolates only the birding hotspots that fall within the top 10% of
# volatility in global migratory exposure between 2005 and 2025. These highly
# volatile hotspots are concentrated primarily in the Himalayan belt,
# northeastern India, and parts of the western coastal region. The high
# volatility indicates that the presence of migratory species in these locations
# fluctuates sharply from year to year, consistent with regions positioned along
# major international flyways where migratory routes and timing respond quickly
# to climate shocks, habitat changes, or ecological disruptions occurring outside
# India.
#
# The absence of high-volatility hotspots across most of central and southern
# India suggests that these areas host a more stable composition of largely
# resident bird communities, making them less sensitive to global migratory
# shifts.


