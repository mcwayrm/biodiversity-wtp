#!/usr/bin/env Rscript
# scripts/R/10_compute_iv.R
#############################################
#  - Builds a hotspot-year shift-share IV from species ranges and flu outbreaks
#  - Merges the IV into the stage-9 master dataset
#  - Saves: outputs$master_data_with_iv
#
#  Required inputs:
#    - master_data_with_travel_cost: Output from stage 9
#    - district_shp: District boundaries shapefile for India union geometry
#    - species_ranges: BirdLife range geopackage
#    - migratory_status: Species migratory status CSV
#    - missing_migration: Manual migratory status fixes CSV
#    - flu_outbreaks: Flu outbreaks by country CSV
#    - voronoi_shp: Hotspot Voronoi polygons
#############################################

cat(paste0("=", strrep("=", 70), "\n"))
cat("10: COMPUTE INSTRUMENTAL VARIABLE\n")
cat(paste0("=", strrep("=", 70), "\n"))
cat(sprintf("Scenario: %s\n", scenario_name))

# Turn off Spherical Geometry
sf::sf_use_s2(FALSE)

# Helper Function: Name Normalizations
normalize_species_name <- function(x) {
    x <- stringr::str_squish(stringr::str_to_lower(x))
    pieces <- stringr::str_split_fixed(x, " ", 2)
    genus <- stringr::str_to_title(pieces[, 1])
    species <- pieces[, 2]
    out <- ifelse(nzchar(species), paste(genus, species), genus)
    out[!nzchar(out)] <- NA_character_
    out
}

# Helper Function: Encoding Resident vs. Migrant classification
recode_migratory_status <- function(x) {
    dplyr::case_when(
        grepl("resident|local|india|altitudinal|nomadic|residdent", x, ignore.case = TRUE) ~ "Resident",
        grepl("passage migrant|winter migrant|summer migrant|migrant", x, ignore.case = TRUE) ~ "Migrant",
        TRUE ~ x
    )
}

# Helper Function: Coordinate System Validation
make_valid_sf <- function(x) {
    if (all(sf::st_is_valid(x))) {
        return(x)
    }
    sf::st_make_valid(x)
}

# Load master data with travel cost
message("Loading master data with travel cost...")
master_data <- read_parquet(inputs$master_data_with_travel_cost)
setDT(master_data)

master_data[, cluster_id := as.character(cluster_id)]

# Correctly encode year
if (!"year" %in% names(master_data)) {
    if ("date" %in% names(master_data)) {
        master_data[, year := lubridate::year(as.Date(date))]
    } else if ("observation_date" %in% names(master_data)) {
        master_data[, year := lubridate::year(as.Date(observation_date))]
    } else {
        stop("master_data_with_travel_cost must contain either 'year', 'date', or 'observation_date'")
    }
}
master_data[, year := as.integer(year)]

# Load Species Range Maps
message("Loading species ranges...")
species_ranges <- st_read(inputs$species_ranges, quiet = TRUE)
species_ranges <- st_as_sf(species_ranges)
species_ranges <- make_valid_sf(species_ranges)

if (!"sci_name" %in% names(species_ranges)) {
    stop("species_ranges input must contain a 'sci_name' column")
}

# Clean species name given scientific name
species_ranges <- suppressWarnings(
    species_ranges |>
        dplyr::mutate(sci_name = normalize_species_name(sci_name)) |>
        dplyr::filter(!is.na(sci_name)) |>
        dplyr::group_by(sci_name) |>
        dplyr::summarise(.groups = "drop")
)

# Load Species Migratory Status Data
message("Loading migratory status data...")
species_migration <- readr::read_csv(inputs$migratory_status, show_col_types = FALSE) |>
    dplyr::transmute(
        sci_name = normalize_species_name(sci_name),
        migratory_status = recode_migratory_status(migratory_status)
    ) |>
    dplyr::filter(!is.na(sci_name)) |>
    dplyr::distinct(sci_name, .keep_all = TRUE)

# Encode Migratory status when missing. 
missing_migration <- readr::read_csv(inputs$missing_migration, show_col_types = FALSE) |>
    dplyr::transmute(
        sci_name = normalize_species_name(sci_name),
        new_migratory_status = recode_migratory_status(new_migratory_status)
    ) |>
    dplyr::filter(!is.na(sci_name)) |>
    dplyr::distinct(sci_name, .keep_all = TRUE)

# Merge migratory status to species with known species range maps. 
species_ranges <- species_ranges |>
    dplyr::left_join(species_migration, by = "sci_name") |>
    dplyr::left_join(missing_migration, by = "sci_name") |>
    dplyr::mutate(
        migratory_status = dplyr::coalesce(migratory_status, new_migratory_status)
    ) |>
    dplyr::select(sci_name, migratory_status, geometry) |>
    dplyr::filter(!sf::st_is_empty(geometry)) |>
    make_valid_sf()

# Load Indian district polygons
message("Building India union geometry...")
india_districts <- st_read(inputs$district_shp, quiet = TRUE) |>
    make_valid_sf() |>
    st_transform(6933)

india_union <- india_districts |>
    dplyr::summarise(geometry = sf::st_union(geometry)) |>
    make_valid_sf()

# Intersection of migrant species range with Indian polygons 
message("Computing species overlap with India...")
migrant_species <- species_ranges |>
    dplyr::filter(migratory_status == "Migrant") |>
    st_transform(6933) |>
    make_valid_sf() |>
    dplyr::mutate(species_id = dplyr::row_number())

# Constant: Indian state size. 
india_area_km2 <- as.numeric(st_area(india_union)) / 1e6

# Overlapping range with polygons
species_india_overlap <- st_intersection(
    migrant_species |>
        dplyr::select(species_id, sci_name),
    india_union
    ) |>
    dplyr::mutate(area_overlap_km2 = as.numeric(st_area(.)) / 1e6) |>
    st_drop_geometry() |>
    dplyr::group_by(species_id, sci_name) |>
    dplyr::summarise(area_overlap_km2 = sum(area_overlap_km2), .groups = "drop")

# Calculating migrant species overlap % with polygons (e.g., spatial coverage)
species_with_share <- migrant_species |>
    st_drop_geometry() |>
    dplyr::select(species_id, sci_name, migratory_status) |>
    dplyr::left_join(species_india_overlap, by = c("species_id", "sci_name")) |>
    dplyr::mutate(
        area_overlap_km2 = dplyr::coalesce(area_overlap_km2, 0),
        frac_india = area_overlap_km2 / india_area_km2
    )

# Loading Bird Flu Outbreak data
message("Loading and aggregating flu outbreaks...")
flu_df <- readr::read_csv(inputs$flu_outbreaks, show_col_types = FALSE) |>
    dplyr::mutate(
        startDate = lubridate::ymd(startDate),
        totalOutbreaks = as.numeric(totalOutbreaks),
        year = lubridate::year(startDate)
    ) |>
    dplyr::filter(!is.na(country), !is.na(year), country != "India")

# Load country polygons
world_countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    make_valid_sf() |>
    dplyr::select(country = name, geometry) |>
    dplyr::filter(country %in% unique(flu_df$country)) |>
    st_transform(6933)

# Keep only countries with source of migrant birds to India
matched_countries <- unique(world_countries$country)
unmatched_countries <- sort(setdiff(unique(flu_df$country), matched_countries))
if (length(unmatched_countries) > 0) {
    message("Dropping unmatched flu countries: ", paste(unmatched_countries, collapse = ", "))
}

# Calculate migrant species from countries with flu outbreaks
species_country <- st_intersection(
    migrant_species |>
        dplyr::select(species_id, sci_name),
    world_countries
    ) |>
    st_drop_geometry() |>
    dplyr::distinct(species_id, sci_name, country)

# Calculate \gamma_j of the instrument: species exposure to flu shock.
gamma_j <- species_with_share |>
    dplyr::inner_join(species_country, by = c("species_id", "sci_name")) |>
    dplyr::group_by(country) |>
    dplyr::summarise(
        S_j = dplyr::n_distinct(sci_name),
        gamma_j = mean(frac_india, na.rm = TRUE),
        .groups = "drop"
    )

# Calculate \shift_t of the instrument: The shift component in the instrument. 
shift_t <- flu_df |>
    dplyr::group_by(country, year) |>
    dplyr::summarise(H5N1_jt = sum(totalOutbreaks, na.rm = TRUE), .groups = "drop") |>
    dplyr::inner_join(gamma_j, by = "country") |>
    dplyr::group_by(year) |>
    dplyr::summarise(
        Shift_t = sum(H5N1_jt * gamma_j, na.rm = TRUE),
        .groups = "drop"
    )

# Calculate hotspot visitation site exposure with species range. 
message("Computing hotspot exposure share...")
hotspots_voronoi <- st_read(inputs$voronoi_shp, quiet = TRUE) |>
    st_transform(6933) |>
    make_valid_sf()

hotspots_voronoi$cluster_id <- as.character(hotspots_voronoi$cluster_id)

species_for_hotspots <- species_ranges |>
    st_transform(6933) |>
    make_valid_sf()

species_hotspot_idx <- st_intersects(species_for_hotspots, hotspots_voronoi)

species_hotspot <- rbindlist(lapply(seq_along(species_hotspot_idx), function(i) {
    hit_idx <- species_hotspot_idx[[i]]
    if (length(hit_idx) == 0L) {
        return(NULL)
    }

    data.table(
        sci_name = species_for_hotspots$sci_name[i],
        migratory_status = species_for_hotspots$migratory_status[i],
        cluster_id = hotspots_voronoi$cluster_id[hit_idx]
    )
    }), use.names = TRUE, fill = TRUE)

# Calculate Share component of IV: species range overlap with hotspots. 
share_by_hotspot <- species_hotspot[
    , .(
        S_d = uniqueN(sci_name),
        migratory_species = uniqueN(sci_name[migratory_status == "Migrant"])
    ),
    by = cluster_id
]
share_by_hotspot[, share_migratory_d := fifelse(S_d > 0, migratory_species / S_d, NA_real_)]

if ("n_hotspots" %in% names(hotspots_voronoi)) {
    hotspot_meta <- unique(as.data.table(st_drop_geometry(hotspots_voronoi))[, .(cluster_id, n_hotspots)])
    share_by_hotspot <- hotspot_meta[share_by_hotspot, on = "cluster_id"]
}

# Consruct the panel of IV exposure to migrant species with flu outbreak. 
message("Constructing hotspot-year IV panel...")
iv_panel <- CJ(
    cluster_id = unique(share_by_hotspot$cluster_id),
    year = unique(as.integer(shift_t$year))
)

iv_panel <- merge(iv_panel, share_by_hotspot, by = "cluster_id", all.x = TRUE)
iv_panel <- merge(iv_panel, as.data.table(shift_t), by = "year", all.x = TRUE)
iv_panel[, IV_dt := share_migratory_d * Shift_t]

# Merge IV data into the master data 
message("Merging IV into master data...")
setkey(iv_panel, cluster_id, year)
setkey(master_data, cluster_id, year)
master_data <- iv_panel[master_data]

# The key newly constructed variables added to the master data. 
priority_cols <- c("share_migratory_d", "Shift_t", "IV_dt", "S_d", "migratory_species", "n_hotspots")
setcolorder(
    master_data,
    c(intersect(priority_cols, names(master_data)), setdiff(names(master_data), priority_cols))
)

# Save updated data with IV info.
write_parquet(master_data, outputs$master_data_with_iv)

if (!is.null(outputs$iv_panel) && nzchar(outputs$iv_panel)) {
    write_parquet(iv_panel, outputs$iv_panel)
}

message("Saved IV-enriched data: ", basename(outputs$master_data_with_iv))
message("IV availability: ", master_data[!is.na(IV_dt), .N], " / ", nrow(master_data), " rows")
cat(paste0("=", strrep("=", 70), "\n"))