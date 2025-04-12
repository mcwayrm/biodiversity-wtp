# PROJECT: eBird WTP
# PURPOSE: Clean ebird data

### SET-UP
# Directories
rm(list=ls())
LOCAL <- '/Users/rmadhok/Dropbox/biodiversity-wtp/'

# Load Packages
pacman::p_load('data.table', 'tidyverse', 'sf', 'raster', 'exactextractr', 
               'showtext', 'weights', 'stargazer')
font_add('latex', 'latinmodern-math.otf')
showtext_auto()

#-------------------------------------------------------------
# 1. CHARACTERIZE USERS BY RURAL/URBAN
#-------------------------------------------------------------
setwd(LOCAL)

# Load data
india_dist <- st_read('./data/shp/district-2011/district-2011.shp') # districts
home <- readRDS('./data/intermediate/ebird/user_home_impute.rds') # homrd

# Set up grid
grid <- raster(india_dist)
res(grid) <- .2
crs(grid) <- crs(india_dist)

# Number of homes per cell
home_grid <- rasterize(home[, c('lon_home', 'lat_home')], grid, fun='count')
home_grid[is.na(home_grid)] <- 0 
home_grid <- mask(home_grid, india_dist)

# Plot
df <- as.data.frame(home_grid, xy = T) %>%
  filter(!is.na(layer))
df$cuts <- cut(df$layer, breaks=c(0, 1, 20, 40, 60, 80, 100, 1027), 
               labels=c('0', '1-20', '20-40', '40-60', '60-80', '80-100', '100+'),
               right=F,
               include.lowest=T)

ggplot() + 
  ggtitle("B") +
  geom_tile(data=df, mapping = aes(x = x, y = y, fill = cuts)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='User Homes',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position='bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20)) 
setwd(LOCAL)
ggsave('./output/fig/ebird_home_density.png')

# Read India population density (source: WorldPop)
r <- raster('./data/raster/ind_pd_2015_1km.tiff')
r <- crop(r, extent(india_dist))
r <- mask(r, india_dist)

# resample population raster to home grid resolution
r_ag <- raster::aggregate(r, fact = res(home_grid)[1]/res(r)[1], fun=sum) # population per 20km cell

# Plot
df <- as.data.frame(r_ag, xy = T) %>%
  filter(!is.na(ind_pd_2015_1km))
df$cuts <- cut(df$ind_pd_2015_1km, breaks=c(0, 1000, 200000, 400000, 600000, 800000, 1000000, 8000000), 
               labels=c('0-1','1-200', '200-400', '400-600', '600-800', '800-1000', '1000+'),
               right=F,
               include.lowest=T)

ggplot() + 
  ggtitle("A") +
  geom_tile(data=filter(df, !is.na(cuts)), mapping = aes(x = x, y = y, fill = cuts)) + 
  coord_sf(datum = NA) +
  scale_fill_viridis_d(name='2015 Population Density (Thousands)',
                       direction=-1,
                       guide=guide_legend(title.position='top')) +
  theme_minimal() +
  theme(text = element_text(family = 'latex'),
        plot.title = element_text(size = 20, face = "bold"),
        axis.line = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.key.width=unit(1.3,'cm'),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20))
setwd(LOCAL)
ggsave('./output/fig/pop_density_2015.png')

#-------------------------------------------------------------
# % of Population in cities
#-------------------------------------------------------------
{
# Read city polygons (source: GRUMP; we add 3km buffer around each city)
city_buf <- st_read('./data/shp/india-city/city_buffer.shp')

# Extract population over city polygons
city_pop <- cbind(city_buf, exact_extract(r, city_buf, 'sum'))
names(city_pop)[2] <- 'population'

# Get city of user home
# Note: NA means user does not live in city
user_city <- st_join(st_as_sf(home, 
                              coords=c('lon_home', 'lat_home'),
                              crs=4326),
                     city_pop, 
                     join = st_intersects) %>% 
  st_drop_geometry()

# users in cities (NA means home does not overlap a city)
user_stats <- user_city %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(city_users = n())
#19717/49206 = 40% of users live in cities > 1million

# city stats
city_stats <- city_pop %>%
  st_drop_geometry() %>%
  mutate(city = ifelse(population > 1000000, 1, 0)) %>%
  group_by(city) %>%
  summarize(population = sum(population, na.rm=T))
# 461667051/819813582 (56.3%) of ppl live in cities > 1 mil pop
# sum of population raster cells = 819813582
}
#-------------------------------------------------------------
# 2. MATCH USER HOMES TO DHS CLUSTERS
#-------------------------------------------------------------

# Load DHS clusters (n=28,526)
dhs <- st_read('./data/shp/dhs/IAGE71FL.shp')

# Clean dhs (n=28,395 villages)
colnames(dhs) <- tolower(colnames(dhs))
dhs <- dhs %>%
  dplyr::select(dhsid, dhsclust, urban_rura, latnum, longnum) %>%
  rename(ru_dhs = urban_rura,
         lat = latnum,
         lon = longnum) %>%
  filter(lat!=0 & lon!=0)

# Classify eBird homes as rural/urban
home$ru_home <- st_join(st_as_sf(home, 
                                 coords=c('lon_home', 'lat_home'), 
                                 crs=4326),
                        city_buf, 
                        join = st_intersects)$FID

# Classify rural/urban
home <- home %>% mutate(ru_home = ifelse(is.na(ru_home), 'R', 'U'))

# Nearest rural DHS cluster to rural home
home_r <- filter(home, ru_home == 'R') %>%
  st_as_sf(coords=c('lon_home', 'lat_home'), crs=4326)
dhs_r <- filter(dhs, ru_dhs == 'R')
dhs_match_r <- st_join(home_r, dhs_r, join=st_nearest_feature)

# Nearest urban DHS cluster to urban home
home_u <- filter(home, ru_home == 'U') %>%
  st_as_sf(coords=c('lon_home', 'lat_home'), crs=4326)
dhs_u <- filter(dhs, ru_dhs == 'U')
dhs_match_u <- st_join(home_u, dhs_u, join=st_nearest_feature)

# append
dhs_match <- rbind(dhs_match_r, dhs_match_u) %>%
  dplyr::select(!c('lat', 'lon', 'c_code_2011_home')) %>%
  st_drop_geometry()
rm(list=c('home_r', 'dhs_r', 'home_u', 'dhs_u', 
          'dhs_match_r', 'dhs_match_u', 'user_city', 'user_stats',
          'city_buf', 'city_pop', 'city_stats'))

#-------------------------------------------------------------
# 3. Compare eBird demographics to India Average
#-------------------------------------------------------------

# Load DHS household data (n=601,509 households)
dhs <- haven::read_stata('./data/intermediate/dhs/dhs_hh.dta')

# Clean DHS
dhs <- dhs %>%
  rename(dhsclust = hv001,
         hh_id = hv002,
         wt = hv005,
         hh_size = hv009,
         children = hv014,
         ruralurban = hv025,
         water_source = hv201,
         toilet = hv205,
         elec = hv206,
         radio = hv207,
         fridge = hv209,
         car = hv212,
         rooms_sleep = hv216,
         relation = hv217,
         hh_head_age = hv220,
         kitchen_separate = hv242,
         cellphone = hv243a,
         wealth_idx = hv270,
         tv_colour = sh37j,
         internet = sh37n,
         washer = sh37r) %>%
  mutate(water_piped = ifelse(water_source %in% c('10', '11'), 1, 0),
         rooms_per_adult = rooms_sleep / (hh_size - children),
         momdad = ifelse(relation == 2, 1, 0),
         flush_toilet = ifelse(toilet >= 10 & toilet <= 15, 1, 0),
         wealth_idx_z = (wealth_idx - mean(wealth_idx))/sd(wealth_idx),
         wt = wt/1000000) # scaling factor to make bootstrap work.

#--------------------------------------------------------
# Note: some clusters paired to multiple users 
# i.e.  duplicates in matched sample
# construct weighted subset of unique hh with
# weights = num users the cluster is matched to
#--------------------------------------------------------
match <- dhs_match %>%
  group_by(dhsclust) %>%
  summarise(n_users = n(),
            ru_dhs = first(ru_dhs)) %>%
  ungroup()

# DHS data for households in user home locations
match <- inner_join(dhs, match, by='dhsclust')

#-------------------------------------------------------------
# T-TESTS
#-------------------------------------------------------------
varlist <- c('wealth_idx', 'hh_size', 'cellphone', 'fridge', 'car', 
             'kitchen_separate', 'tv_colour', 'internet', 
             'washer', 'flush_toilet')

# ttest function
t_test <- function(i, sample = 'All') {
  
  # select variables
  if(sample == 'All') {
    x <- match %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- dplyr::select(match, n_users) %>% as_vector()
    ywt <- dplyr::select(dhs, wt) %>% as_vector()
  }
  
  if(sample == 'U') {
    x <- match %>% filter(ruralurban == 1) %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% filter(ruralurban == 1) %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- match %>% filter(ruralurban == 1) %>% dplyr::select(n_users) %>% as_vector()
    ywt <- dhs %>% filter(ruralurban == 1) %>% dplyr::select(wt) %>% as_vector()
  }
  
  if(sample == 'R') {
    x <- match %>% filter(ruralurban == 2) %>% dplyr::select(varlist[i]) %>% as_vector()
    y <- dhs %>% filter(ruralurban == 2) %>% dplyr::select(varlist[i]) %>% as_vector()
    xwt <- match %>% filter(ruralurban == 2) %>% dplyr::select(n_users) %>% as_vector()
    ywt <- dhs %>% filter(ruralurban == 2) %>% dplyr::select(wt) %>% as_vector()
  }
  
  # ttest
  ttest <- wtd.t.test(x, y, 
                      weight = xwt, 
                      weighty = ywt, 
                      samedata = F, 
                      mean1 = F, 
                      bootse = T, 
                      bootn = 20) # update to bootn=1000 later (runs slow)
  
  # build output dataframe
  item <- data.frame(
    list(
      Variable = varlist[i],
      `Matched eBird` = ttest$additional[[2]],
      DHS = ttest$additional[[3]],
      Difference = ttest$additional[[1]],
      `p-value` = ttest$coefficients[[3]]
    )
  )
  
  return(item)
}
idx <- 1:length(varlist)

# difference in means for all samples
df_all <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'All')))
df_r <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'R')))
df_u <- as.data.frame(rbindlist(lapply(idx, t_test, sample = 'U')))

#--------------------------
# FINAL TABLE
#--------------------------

# Prepare Tables function
clean <- function(df) {
  
  # variable labels
  df$Variable <- c('Wealth Index', 'HH Size', 'Cellphone (=1)', 'Fridge (=1)', 'Car (=1)', 
                   'Sep. Kitchen (=1)', 'Colour TV (=1)',  'Internet (=1)', 
                   'Washing Machine (=1)', 'Flush Toilet (=1)')
  
  # Columns
  names(df) <- c('Variable', 'Matched eBird', 'DHS', 'Difference', 'pvalue')
  
  # trailing zeros
  df$Difference <- as.numeric(sprintf("%.3f", df$Difference))
  
  # Significance stars
  df$Difference[df$pvalue <= 0.01] <- paste(df$Difference[df$pvalue <= 0.01], '***', sep='')
  df$Difference[df$pvalue > 0.01 & df$pvalue <= 0.05] <- paste(df$Difference[df$pvalue > 0.01 & df$pvalue <= 0.05], '**', sep='')
  df$Difference[df$pvalue > 0.05 & df$pvalue <= 0.1] <- paste(df$Difference[df$pvalue > 0.05 & df$pvalue <= 0.1], '*', sep='')
  
  return(df)
  
}

# Prep tables
dflist <- list(df_all = df_all, df_u = df_u, df_r = df_r)
df_clean <- lapply(dflist, clean)
df_clean <- lapply(df_clean, function(x) x %>% dplyr::select(Variable, Difference))

# Merge tables
table <- df_clean %>% 
  reduce(full_join, by='Variable')
names(table) <- c('Variable', 'All', 'Urban', 'Rural')

# Write out
stargazer(table, summary=F, rownames=F,float=F, column.sep.width = '1.8cm',
          out='./output/table/ebird_dhs_ttest.tex')

