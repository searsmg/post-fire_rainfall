library(tidyverse)

# all_avg <- read_csv('./data/final/mrms/mrms_catchments_raw/all_combined.csv')
# 
# all_avg <- all_avg %>%
#   filter(ID == 'dry') %>%
#   mutate(datetime = with_tz(datetime, tzone='MST')) %>%
#   mutate(year =year(datetime)) %>%
#   filter(year == 2022)

mean_exact <- read_csv('/Users/megansears/Documents/MRMS/mrms_data/mean_exact_catchment21_2min.csv')

mean_exact <- mean_exact %>%
  filter(catchment == 'dry') %>%
  mutate(datetime = force_tz(datetime, tzone='MST'))

# compare <- left_join(all_avg, max, by ='datetime')

# compare <- left_join(all_avg, mean_exact, by='datetime')
# 
# compare <- compare %>%
#   mutate(diff = MI2_mmhr.x - MI2_mmhr.y)
# 
# mean_exact <- read_csv('/Users/megansears/Documents/MRMS/mrms_data/mean_exact_catchment22_2min.csv') %>%
#   filter(catchment == 'dry') %>%
#   mutate(datetime = force_tz(datetime, tzone='MST')) %>%
#   mutate(year =year(datetime)) %>%
#   filter(year == 2022)


library(stars)
library(mapview)

#2022-06-29 17:22:00
test <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/2min_2021/multiplied_20210529-213400.tif')

catchments_all <- st_read('/Users/megansears/Library/CloudStorage/OneDrive-Colostate/PhD/post-fire_rain_response/data/GIS/catchments_all/catchments_all_lidar.shp') 

mapview(test) + mapview(catchments_all)

library(terra)
test1<-rast(test)
catch1<-vect(catchments_all) %>%
  terra::project(., 'EPSG:4326')

#extract <- terra::zonal(test1, catch1, 'max',na.rm=T) # max of the pixels that the center is w/in polygon
#extract_mean_exact <- terra::zonal(test1, catch1, 'mean', exact=T)
extract2 <- terra::zonal(test1, catch1, 'mean', exact=T)

#extract_mean_exact1 <- terra::extract(test1, catch1, 'mean', exact=T)

# extract_test <- terra::extract(test1, catch1, 'mean', weights=T) # faster computation
# extract_test <- terra::extract(test1, catch1, 'mean', exact=T) # exact but slower computation
#extract_max_touc <- terra::extract(test1, catch1, 'max', touches=T) # includes all touches raster cells
#extract_max_touc1 <- terra::zonal(test1, catch1, 'max', touches=T) 

#write_stars(test, '/Users/megansears/Desktop/test_20220706-2040.tif')
