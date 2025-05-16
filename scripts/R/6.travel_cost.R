# PROJECT: RUM Model
# PURPOSE: Estimate travel cost for each counterfactual
####################################
# Sections:
# 1. Estimate travel cost
####################################


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)


# ----------------------------------------------------
# 1. Estimate travel cost
# ----------------------------------------------------

# Bring in user's home location 
# user_impute <- readRDS(config$user_home_impute_path) # I think this has both real and imputed. 

# Bring in list of counterfactuals from the choice set
counterfactuals <- readRDS(config$choice_set_path) 
    # Counterfactual hotspots clusters across time for each user. This does not vary by time  



trips <- readRDS(config$ebird_trip_hotspots_path) 
    # The observed trips to hotspots that the user took.
    # This needs to be collapsed to hotspot clusters (some mapping)

# NOTE: We are comparing observed hotspot trips to counterfactual hotspot clusters. 
    # We do this because it computationally expensive to calculate for each hotspot. 
    # Could be done with hotspots in the future when transition to MSI. 


# TODO: Want to duplicate clusters for each year and month. 
    # all unique years in trips data
    # 1-12 months 
    # eg. should be roughly 8040000 at the end


# GDP annual data 
    # bring in gdp data
    # gdp merge in trips and counterfactuals on year


# TODO: 
# Take the hotspots with distance. 
# Then merge in GDP by year for each trip
# Then do the calculation. 
#     Lloyd-Smith and Cameron/Kolstoe
#             - Travel cost = 2 (round trip) x distance x 1/3 x wage (from GDP)


# Save a master trip and counterfactual data set with travel cost

