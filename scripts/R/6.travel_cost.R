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
user_home_impute.rds # I think this has both real and imputed. 
user_home_real.rds

# Bring in list of counterfactuals from the choice set
choice_set_50km_clust_10km.rds