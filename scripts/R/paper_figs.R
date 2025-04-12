# PROJECT: WTP Biodiversity
# PURPOSE: Paper Plots

# Directories
rm(list=ls())
LOCAL <- '/Users/rmadhok/Dropbox/biodiversity-wtp/'

# Load Packages
packages <- c('tidyverse', 'sf', 'units', 'scattermore')
pacman::p_load(packages, character.only = TRUE, install = FALSE)
font_add('latex', 'latinmodern-math.otf')
showtext_auto()

# Load maps
india_dist <- st_read('./data/shp/district-2011/district-2011.shp') # districts
#-----------------------------------------------
# Distance b/w real and imputed homes
#-----------------------------------------------
real_home <- readRDS('./data/intermediate/ebird/user_home_real.rds')
imputed_home <- readRDS('./data/intermediate/ebird/user_home_impute.rds')
homes <- merge(real_home, imputed_home, by='user_id')

# Offset
homes$offset <- st_distance(st_as_sf(homes, 
                                     coords = c('lon_home_real', 'lat_home_real'),
                                     crs=4326), 
                            st_as_sf(homes, 
                                     coords = c('lon_home', 'lat_home'),
                                     crs=4326), 
                            by_element = T) %>% set_units(km)
homes$offset <- as.numeric(homes$offset)

# Plot
ggplot(homes, aes(x=offset)) + 
  stat_bin(aes(y=..count..), color='black', fill='orange') +
  ylab('Number of Users\n') +
  geom_vline(aes(xintercept = median(offset)), col='purple') +
  annotate(x=300,y=150,label="Median = 21.7 km", 
           vjust=3, family='latex', geom="text", size = 25/.pt) +
  scale_x_continuous(name='Distance Between Actual\n and Imputed Home (km)', 
                     breaks = seq(0, 1500, by = 200)) +
  theme_minimal() +
  theme(text = element_text(family='latex', size=40),
        axis.line = element_blank(), 
        axis.text.x = element_text(angle = 45),
        axis.ticks = element_blank())

ggsave('./output/fig/home_km_offset.png')

#-----------------------------------------------
# eBird Trips
#-----------------------------------------------

# read trip points
trips <- readRDS('./data/intermediate/ebird/ebird_trip_hotspots.rds') %>%
  dplyr::select(lat, lon)

# plot
ggplot() +
  geom_sf(data=india_dist, fill='black', color=NA) +
  geom_scattermore(data=trips, 
                   aes(x=lon,y=lat, 
                       fill='Hotspot Trips'), 
                   color='orange') +
  coord_sf(datum = NA) +
  labs(fill=' ') +
  theme(text = element_text(family='latex'),
        panel.background = element_blank(),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        legend.position='bottom',
        legend.key=element_blank(),
        legend.text=element_text(size=30))
ggsave('./output/fig/hospot_trip_map.png', width = 10, height = 8, dpi = 150, units = "in")