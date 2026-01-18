
library(tidyverse)
library(terra)
library(here)

sp <- rast('./data/GIS/LooseItems/MOD10A2_SCI_2020.tif') %>%
  terra::project(., 'EPSG:26913')


all_sites <- vect('./data/GIS/LooseItems/catchments_all_lidar.shp') %>%
  terra::project(., crs(sp))

bbox <- ext(all_sites)

# buffer distance in map units (e.g., meters if projected)
buf <- 10000 

bbox_buf <- ext(
  xmin(bbox) - buf,
  xmax(bbox) + buf,
  ymin(bbox) - buf,
  ymax(bbox) + buf
)


sp_crop <- crop(sp, bbox_buf)

plot(sp_crop)
plot(all_sites, add=T)


class <- ifel(sp_crop > 61, 2, 1)

plot(class)
plot(all_sites, add =T)

# Raster to dataframe
class_df <- as.data.frame(class, xy = TRUE, na.rm = TRUE)
names(class_df)[3] <- "class"

# Ensure factor labels are correct
class_df$class <- factor(
  class_df$class,
  levels = c(1, 2),
  labels = c("intermittent", "seasonal")
)

# Vector to sf
sites_sf <- st_as_sf(all_sites)

#-----------------------------
# 4. ggplot map
#-----------------------------
ggplot() +
  geom_raster(
    data = class_df,
    aes(x = x, y = y, fill = class)
  ) +
  geom_sf(
    data = sites_sf,
    color = "black",
    linewidth=1,
    fill=NA
  ) +
  coord_sf(
    xlim = c(xmin(bbox_buf), xmax(bbox_buf)),
    ylim = c(ymin(bbox_buf), ymax(bbox_buf)),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c(
      intermittent = "lightblue",
      seasonal = "blue"
    ),
    name = "Snow zone"
  ) +
  theme_bw(base_size=20) +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = "black"),
    axis.text =element_text(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"))

ggsave('/Users/megansears/Library/CloudStorage/OneDrive-Colostate/PhD/post-fire_rain_response/figures/snowzone60.png',
       dpi=800)
