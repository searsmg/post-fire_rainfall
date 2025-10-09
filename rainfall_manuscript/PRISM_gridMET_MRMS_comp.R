library(stars)
library(tidyverse)
library(mapview)

sum_all21 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/summertotal_2021_fil01.tif')
sum_all22 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/summertotal_2022_fil01.tif')
sum_all23 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/summertotal_2023_fil01.tif')

p_df21<- as.data.frame(sum_all21, xy = TRUE) %>%
  rename(sum = summertotal_2021_fil01.tif) %>%
  mutate(year = 2021,
         source = 'MRMS')

p_df22<- as.data.frame(sum_all22, xy = TRUE) %>% 
  rename(sum = summertotal_2022_fil01.tif) %>%
  mutate(year = 2022,
         source = 'MRMS')

p_df23<- as.data.frame(sum_all23, xy = TRUE) %>%
  rename(sum = summertotal_2023_fil01.tif) %>%
  mutate(year = 2023,
         source = 'MRMS')


p_df <- bind_rows(p_df21, p_df22, p_df23)

###

gridmet21 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/2021_gridmet_summerp.tif') %>%
  st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2021,
         source = 'Gridmet')


gridmet22 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/2022_gridmet_summerp.tif') %>%
  st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2022,
         source = 'Gridmet')

gridmet23 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/2023_gridmet_summerp.tif') %>%
  st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2023,
         source = 'Gridmet')


gridmet_all <- bind_rows(gridmet21, gridmet22, gridmet23)

###

prism21 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/prism800mJJAS2021.tif') %>%
  #st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2021,
         source = 'PRISM')

#mapview(gridmet21) + mapview(sum_all21)

prism22 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/prism800mJJAS2022.tif') %>%
  #st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2022,
         source = 'PRISM')

prism23 <- read_stars('/Users/megansears/Documents/MRMS/mrms_data/bbox_2fires/prism800mJJAS2023.tif') %>%
  #st_warp(., sum_all23) %>%
  as.data.frame(., xy = TRUE) %>%
  rename(sum = 3) %>%
  mutate(year = 2023,
         source = 'PRISM')

prism_all <- bind_rows(prism21, prism22, prism23)


max(prism_all$sum)
max(gridmet_all$sum)
max(p_df$sum)

median(prism_all$sum)
median(gridmet_all$sum)
median(p_df$sum)


