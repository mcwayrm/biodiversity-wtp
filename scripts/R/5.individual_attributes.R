# PROJECT: RUM Model
# PURPOSE: Construct data on user attributes (push factors)
####################################
# Sections:
# 1. 
####################################


### SET-UP
# Load config
config_path <- file.path("scripts", "R", "0.load_config.R")
source(config_path)

# ----------------------------------------------------
# 1. 
# ----------------------------------------------------

# Potential:
    # Income
    # Education
    # Gender
    # Age


# ----------------------------------------------------
# 2. Travel Cost estimates
# ----------------------------------------------------

Calculate travel cost
    - Start with linear distance for now
    - Calculate cost
        - Refer to Lloyd-Smith and Cameron/Kolstoe
        - Travel cost = 2 (round trip) x distance x 1/3 x wage (from GDP)