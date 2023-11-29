
library(sp)
library(sf)
library(terra)
library(mapview)
library(tidyverse)

# ETF watersheds - don't need right now
et_sheds <- vect('./data/watershed_bnd_UTM.shp') %>%
  terra::project(., 'EPSG:26913') %>% 
  as(., 'Spatial') %>%
  st_as_sf() %>% 
  dplyr::select(Name)

benn_sheds <- vect('./data/mulch_study_watersheds.shp') %>%
  terra::project(., 'EPSG:26913') %>%
  as(., 'Spatial') %>% 
  st_as_sf() %>% 
  dplyr::select(Name)

benn_sheds$Name[1] <- 'me'
benn_sheds$Name[2] <- 'mm'
benn_sheds$Name[3] <- 'mw'
benn_sheds$Name[4] <- 'ue'
benn_sheds$Name[5] <- 'um'
benn_sheds$Name[6] <- 'uw'


cpf_sheds <- vect('./data/cpf_watersheds_20210618.shp') %>%
  terra::project(., 'EPSG:26913') %>%
  as(., 'Spatial') %>% 
  st_as_sf() %>% 
  dplyr::select(Name) %>%
  mutate(Name = replace_na(Name, 'bl4'))


mapview(et_sheds) +
  mapview(benn_sheds) +
  mapview(cpf_sheds)

bighorn_shed<- vect('./data/globalwatershed.shp') %>%
  terra::project(., 'EPSG:26913') %>%
  as(., 'Spatial') %>%
  st_as_sf() %>% 
  dplyr::select(Name) %>%
  mutate(Name = 'Bighorn')


sheds <- rbind(et_sheds, benn_sheds, cpf_sheds, bighorn_shed)

mapview(sheds)

st_write(sheds, './data/catchments_combined.shp')

bbox_all <- st_bbox(sheds)

mapview(bbox_all) + mapview(sheds)


bbox_all <- st_as_sfc(bbox_all)
bbox_all <- st_sf(geometry = bbox_all)



st_write(bbox_all, './data/catchments_all.shp')

mapview(bbox_all)

