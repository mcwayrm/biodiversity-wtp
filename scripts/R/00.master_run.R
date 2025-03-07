# PROJECT: Replication
# PURPOSE: This master script calls all the other scripts in order.
#          This acts as the push-button replication scipt.

# Running scripts in order.
source('/scripts/R/1.ebd_process.R') # Process e-bird data
source('/scripts/R/2.distance_to_hotspots.R') # Determine travel distance to birding hotspots
source('/scripts/R/3.make_choice_set.R') # Creates counterfactuals choice for bird options compared to observed data
