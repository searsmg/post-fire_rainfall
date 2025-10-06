
library(tidyverse)
library(mapview)
library(stars)
library(terra)


#norm <- read_stars('/Users/megansears/Downloads/prism_ppt_us_30s_2020_avg_30y/prism_ppt_us_30s_2020_avg_30y.tif')
junNorm <- rast('/Users/megansears/Downloads/prism_normals_JJAS/prism_ppt_us_30s_202009_avg_30y.tif')

#sum_all23 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/summertotal_2023_fil01.tif')
sum_all23 <- rast('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/summertotal_2023_fil01.tif')

junNorm1 <- project(junNorm, sum_all23)

junNorm_stars1 <- st_as_stars(junNorm1)

huc <- st_read('./data/GIS/WBDHU8.shp') %>%
  filter(!huc8 == 10180001) %>%
  filter(states %in% c('CO', 'CO,WY'))

huc8 <- st_union(huc) %>%
  st_sf()

huc8 <- st_transform(huc8, crs = 4326)

mapview(huc8) + mapview(junNorm_stars1)

right <- st_crop(junNorm_stars1, huc8)
mapview(right)

right

##
left <- rast(junNorm_stars1)
left <- mask(left, huc8, inverse=T)
plot(left)
left
summary(left)
