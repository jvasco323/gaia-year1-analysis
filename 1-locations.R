
library(tidyverse)
library(sf)

# ------------------------------------------------------------------------------
# load data
df <- read.csv(paste0(dir, '/first-year/gaia-trials-year1.csv'))
df <- subset(df, trial_type == 'Shotgun')
# make map

path <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/#-bisrat-clean/data/final_data/year_1/'
rwa <- read.csv(paste0(path, 'gaia_trials_Rwanda_season1_shotgun.csv'))


map <- 
  rwa[!is.na(rwa$lng),] %>%
  st_as_sf(coords = c("lng", "lat")) %>%
  st_set_crs(4326)

mapview::mapview(map)

rwa <- subset(map, country=='Rwanda')
plot(st_geometry(rwa[rwa$district_gadm=='Nyaruguru',]))

# ------------------------------------------------------------------------------
# spatial data
dir <- 'D:/# Jvasco/Working Papers/GAIA Guiding Acid Soil Investments/2-yield-response/input-data/'
eth <- geodata::gadm('ETH', path=dir, level=2)
eth <- subset(eth, eth$NAME_2 == 'Jimma' | eth$NAME_2 == 'Misraq Gojjam')
terra::plot(eth)
tza <- geodata::gadm('TZA', path=dir, level=2)
tza <- subset(tza, tza$NAME_2 == 'Geita' | tza$NAME_2 == 'Mbozi')
terra::plot(tza)
rwa <- geodata::gadm('RWA', path=dir, level=2)
rwa <- subset(rwa, rwa$NAME_2 == 'Burera' | rwa$NAME_2 == 'Ngororero' | rwa$NAME_2 == 'Nyaruguru')
terra::plot(rwa)
all <- rbind(eth, tza, rwa)
terra::plot(all)
df_master <- data.frame('ID'=seq(1,7,1), 'site'=c("Misraq Gojjam", "Jimma", "Geita", "Mbozi", "Burera", "Nyaruguru", "Ngororero"))
# elevation
elev <- geodata::elevation_global(res=2.5, path=dir)
elev <- terra::crop(elev, all, mask=T)
terra::plot(elev)
elev_summary <- terra::extract(elev, all, ID=T)
elev_summary <- data.frame(elev_summary)
elev_summary <- aggregate(elev_summary[,2], elev_summary[,1,drop=FALSE], mean, na.rm=T)
elev_summary <- merge(df_master, elev_summary, by='ID')
colnames(elev_summary)[3] <- 'elev'
# soils data
soil <- terra::rast(paste0(dir, 'ex-ante/soilgrids_properties_cropland.tif'))
soil <- soil[[c('ph', 'ecec', 'hp_sat')]]
soil_summary <- terra::extract(soil, all, ID=T)
soil_summary <- data.frame(soil_summary)
soil_summary <- aggregate(soil_summary[,2:4], soil_summary[,1,drop=FALSE], mean, na.rm=T)
soil_summary <- merge(df_master, soil_summary, by='ID')
# rainfall
rain <- terra::rast(paste0(dir, 'chirps/rast_avg_tot.tif'))
terra::plot(rain)
rain_summary <- terra::extract(rain, all, ID=T)
rain_summary <- data.frame(rain_summary)
rain_summary <- aggregate(rain_summary[,2], rain_summary[,1,drop=FALSE], mean, na.rm=T)
rain_summary <- merge(df_master, rain_summary, by='ID')
colnames(rain_summary)[3] <- 'rain'
# merge all
final_df <- Reduce(function(x, y) merge(x, y, by=c('ID', 'site'), all=TRUE), list(elev_summary, soil_summary, rain_summary))
final_df[c(3:7)] <- round(final_df[c(3:7)], 1)
