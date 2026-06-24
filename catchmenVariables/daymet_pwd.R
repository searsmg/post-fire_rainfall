# Daymet PWD (Precipitation Water Deficit) for catchments
# PWD = cumulative(prcp - peth) by water year, lagged 1 day
# Hargreaves PET using Daymet prcp, tmax, tmin; Ra from lat/doy
# Point download at catchment centroids via daymetr

library(tidyverse)
library(lubridate)
library(terra)
library(sf)
library(daymetr)
library(dataRetrieval)

# ── catchments ────────────────────────────────────────────────────────────────

catch <- vect('./data/GIS/catchments_all/catchments_all_lidar.shp')
catch$site[catch$site == "mm"] <- "mm_et"

catch_sf <- st_as_sf(catch) %>%
  st_make_valid() %>%
  st_transform(4326)

catch_sf$site <- catch$site

# catchment centroid lat/lon
catch_pts <- catch_sf %>%
  mutate(
    lon = st_coordinates(suppressWarnings(st_centroid(.)))[, 1],
    lat = st_coordinates(suppressWarnings(st_centroid(.)))[, 2]
  ) %>%
  st_drop_geometry() %>%
  select(site, lat, lon)

# ── helpers ───────────────────────────────────────────────────────────────────

calc_ra <- function(doy, lat_deg) {
  lat_rad <- lat_deg * pi / 180
  dr      <- 1 + 0.033 * cos(2 * pi * doy / 365)
  delta   <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  ws      <- acos(-tan(lat_rad) * tan(delta))
  (24 * 60 / pi) * 0.0820 * dr *
    (ws * sin(lat_rad) * sin(delta) + cos(lat_rad) * cos(delta) * sin(ws))
}

hargreaves_pet <- function(tmax, tmin, ra) {
  tmean <- (tmax + tmin) / 2
  0.0023 * (tmean + 17.8) * sqrt(pmax(tmax - tmin, 0)) * ra * 0.408
}

# Download Daymet for one catchment centroid, return tidy daily tibble
pull_site_daymet <- function(site_name, lat, lon, start_yr, end_yr) {
  message("Pulling Daymet for: ", site_name)
  d <- download_daymet(
    site    = site_name,
    lat     = lat,
    lon     = lon,
    start   = start_yr,
    end     = end_yr,
    silent  = TRUE,
    internal = TRUE
  )

  d$data %>%
    as_tibble() %>%
    mutate(
      date    = as.Date(paste(year, yday, sep = "-"), "%Y-%j"),
      site    = site_name
    ) %>%
    select(site, date,
           prcp_mm = `prcp..mm.day.`,
           tmax_c  = `tmax..deg.c.`,
           tmin_c  = `tmin..deg.c.`)
}

# ── pull Daymet for all catchments ────────────────────────────────────────────

start_yr <- 2020
end_yr   <- 2023

daymet_raw <- pmap(
  list(catch_pts$site, catch_pts$lat, catch_pts$lon),
  \(s, la, lo) pull_site_daymet(s, la, lo, start_yr, end_yr)
) %>%
  list_rbind()

# ── compute PET and PWD ───────────────────────────────────────────────────────

pwd <- daymet_raw %>%
  left_join(catch_pts %>% select(site, lat), by = "site") %>%
  mutate(
    doy    = yday(date),
    ra     = calc_ra(doy, lat),
    pet_mm = hargreaves_pet(tmax_c, tmin_c, ra)
  ) %>%
  mutate(Date = date) %>%
  filter(Date >= as.Date("2020-10-01"), Date <= as.Date("2023-09-30")) %>%
  addWaterYear() %>%
  arrange(site, Date) %>%
  group_by(site, waterYear) %>%
  mutate(
    p_cumu_mm   = cumsum(prcp_mm),
    pet_cumu_mm = cumsum(pet_mm),
    pwd         = p_cumu_mm - pet_cumu_mm,
    lag_pwd     = lag(pwd)
  ) %>%
  ungroup() %>%
  select(site, Date, waterYear, prcp_mm, pet_mm, p_cumu_mm, pet_cumu_mm, pwd, lag_pwd)

write_csv(pwd, './data/final_model_inputs/pwd_daymet.csv')

message("Done — saved to ./data/final_model_inputs/pwd_daymet.csv")
