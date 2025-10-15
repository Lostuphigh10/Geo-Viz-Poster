install.packages("ggplot2")
install.packages("leaflet")
install.packages("tmap")
install.packages("sf")
install.packages("maps")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("geojsonsf")
install.packages("viridis")
install.packages("cartogram")
install.packages("pals")
install.packages("RColorBrewer")
install.packages(c("ggspatial", "rnaturalearth", "rnaturalearthdata"))



library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library("ggplot2")
library("leaflet")
library("tmap")
library("sf")
library("maps")
library("dplyr")
library("tidyverse")
library("reshape2")
library("sf")
library("geojsonsf")
library("tmap")
library("ggplot2")
library("viridis")
library("cartogram")
library("pals")
require(classInt)
require(lattice)
require(grid)

# plot tmap basemap 
library(maptiles)


setwd("G:/Cate Trey/GeoViz/Poster")
getwd()


roads_ln <- st_read("roads_ln/GRSM_PAVEMENT.shp")
roads_ln <- st_make_valid(roads_ln)

world <- ne_countries(scale = "medium", returnclass = "sf")
road_ln <-st_read("roads_ln/GRSM_PAVEMENT.shp")
park_pt<- st_read("parking_pt/GRSM_PARKING.shp")
bac_cam_pt <- st_read("backcountry_campsites/GRSM_BACKCOUNTRY_CAMPSITES.shp")
build_pt <- st_read("buildings_pt/BUILDING_PT.shp")
reservable_fac_pt <- st_read("reservable_facilities/GRSM_RESERVABLE_FACILITIES.shp")
vis_cen_pt <- st_read("visitor_centers_pt/GRSM_VISITOR_CENTERS.shp")
tra_ln <- st_read("trails/GRSM_TRAILS.shp")
cem_py <- st_read("cemetery_pt/CEMETERY_PT.shp")
cem_pt <- st_read("cemetery_pt/CEMETERY_PT.shp")
cem_py <- st_read("cemetery_py/CEMETERY_PY.shp")
cem_pt <- st_read("cemetery_pt/CEMETERY_PT.shp")
cem_py <- st_read("cemetery_py/CEMETERY_PY.shp")
dis_pt <- st_read("district_pt/DISTRICT_PT.shp")
dis_py <- st_read("district_py/DISTRICT_PY.shp")
bou_py <- st_read("bound/cb_2022_us_state_500k.shp")
his_py <- st_read("historic_py/HISTORIC_STRUCTURE_PY.shp")
his_pt <- st_read("historic_pt/HISTORIC_STRUCTURE_PT.shp")
camp_pt <- st_read("G:/Cate Trey/GeoViz/Poster/campsites/GRSM_BACKCOUNTRY_CAMPSITES.shp")
top_py <- st_read("G:/Cate Trey/GeoViz/Poster/topo/GRSM_100K_TOPO_BOUNDARIES.shp")
park_boundary <- st_read("park_boundaries_py/BOUNDARY_LN.shp")
park_boundary <- st_transform(park_boundary, st_crs(bou_py))
library(sf)
his_pt_crop <- st_make_valid(his_pt_crop)

#changing the park boundaries for all the layers

py_crop <- st_intersection(bou_py, park_boundary)
tra_ln_crop <- st_intersection(tra_ln, park_boundary)
dis_py_crop <- st_intersection(dis_py, park_boundary)
his_py_crop <- st_intersection(his_py, park_boundary)
his_pt_crop <- st_intersection(his_pt, park_boundary)
cem_pt_crop <- st_intersection(cem_pt, park_boundary)
camp_pt_crop <- st_intersection(camp_pt, park_boundary)
bac_cam_pt_crop <- st_intersection(bac_cam_pt, park_boundary)
build_pt_crop <- st_intersection(build_pt, park_boundary)
park_pt_crop <- st_intersection(park_pt, park_boundary)
reservable_fac_pt_crop <- st_intersection(reservable_fac_pt, park_boundary)
vis_cen_pt_crop <- st_intersection(vis_cen_pt, park_boundary)

View(dis_py)
View(dis_pt)
View(cem_py)
View(cem_pt)
View(cem_py)
view(his_py)
view(his_pt)


#map
plot(tra_ln$geometry)
plot(cem_pt$geometry)
plot(cem_py$geometry)
plot(dis_pt$geometry)
plot(dis_py$geometry)
plot(camp_pt$geometry)

bbox_smokies <- st_bbox(c(xmin = -84, ymin = 35, xmax = -82.5, ymax = 36), crs = st_crs(bou_py))


bou_py_crop <- st_crop(bou_py, bbox_smokies)
tra_ln_crop <- st_crop(tra_ln, bbox_smokies)
dis_py_crop <- st_crop(dis_py, bbox_smokies)
his_py_crop <- st_crop(his_py, bbox_smokies)
his_pt_crop <- st_crop(his_pt, bbox_smokies)
cem_pt_crop <- st_crop(cem_pt, bbox_smokies)
camp_pt_crop <- st_crop(camp_pt, bbox_smokies)
bac_cam_pt_crop <- st_crop(bac_cam_pt, bbox_smokies)
build_pt_crop <- st_crop(build_pt, bbox_smokies)
park_pt_crop <- st_crop(park_pt, bbox_smokies)
reservable_fac_pt_crop <- st_crop(reservable_fac_pt, bbox_smokies)
vis_cen_pt_crop <- st_crop(vis_cen_pt, bbox_smokies)
                           


##Code removed
#tm_shape(park_pt) + tm_symbols(shape = 15, size = 0.1, fill = "black", fill_alpha = 0.5) +
#tm_shape(build_pt) + tm_symbols(shape = 0, size = 1, fill = "black", fill_alpha = 0.5) +
#tm_shape(reservable_fac_pt) + tm_symbols(shape = 14, size = 0.5, fill = "grey", fill_alpha = 0.5, col = "black") +
#  tm_shape(his_py) + tm_polygons(fill = "red", fill_alpha = 1, col = "black", lwd = 4) +
##Ai updated map
map_cem_dis <-
  tm_shape(dis_py) + tm_polygons(fill = "orange", fill_alpha = 0.5, col = "blue", lwd = 2) +
  tm_shape(tra_ln) + tm_lines(col = "black", lwd = 1.5) +
  tm_shape(vis_cen_pt) + tm_symbols(shape = 22, size = 0.5, fill = "green", col = "black", fill_alpha = 0.5) +
  tm_shape(bac_cam_pt) + tm_symbols(shape = 24, size = 0.5, fill = "brown", fill_alpha = 0.5, col = "black") +
  tm_shape(cem_pt) + tm_symbols(shape = 23, size = 0.5, fill = "violetred", fill_alpha = 0.5, col = "black") +
  tm_shape(camp_pt) + tm_symbols(shape = 24, size = 0.5, fill = "burlywood", fill_alpha = 0.5, col = "black") +
  tm_shape(bou_py) + tm_borders(col = "grey60", lwd = 1) +
  tm_shape(his_pt) + tm_symbols(shape = 21, size = 0.35, fill = "aquamarine", col = "black", fill_alpha = 0.5) +
  tm_compass(north = 0, size = 1) +
  tm_scalebar(position = c("right", "bottom"), width = 9) +
  tm_add_legend(
    type = "symbols",
    labels = c("Cemetery", "Historic Site", "Campsites", "Visitor Center"),
    col = c("violetred", "aquamarine", "burlywood", "green"),
    shape = c(23, 21, 24, 22),
    title = "Map Symbols") +
  tm_add_legend(
    type = "lines",
    labels = "Trails",
    col = "black",
    lwd = 1.5,
    title = "Hiking Trails") +
  tm_add_legend(
    type = "polygons",
    labels = "District",
    fill = "orange",
    col = "blue",
    title = "Historic District") +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.text.size = 0.6,
    legend.title.size = 0.7) + 
  tm_basemap("Esri.NatGeoWorldMap")
  
map_cem_dis

## changing the mao background

map_cem_dis <-
  tm_shape(dis_py) + tm_polygons(fill = "orange", fill_alpha = 0.5, col = "blue", lwd = 2) +
  tm_shape(tra_ln) + tm_lines(col = "black", lwd = 1.5) +
  tm_shape(vis_cen_pt) + tm_symbols(shape = 22, size = 0.5, fill = "green", col = "black", fill_alpha = 0.5) +
  tm_shape(bac_cam_pt) + tm_symbols(shape = 24, size = 0.5, fill = "brown", fill_alpha = 0.5, col = "black") +
  tm_shape(cem_pt) + tm_symbols(shape = 23, size = 0.5, fill = "violetred", fill_alpha = 0.5, col = "black") +
  tm_shape(camp_pt) + tm_symbols(shape = 24, size = 0.5, fill = "burlywood", fill_alpha = 0.5, col = "black") +
  tm_shape(bou_py) + tm_borders(col = "grey60", lwd = 1) +
  tm_shape(his_pt) + tm_symbols(shape = 21, size = 0.35, fill = "aquamarine", col = "black", fill_alpha = 0.5) +
  tm_compass(north = 0, size = 1) +
  tm_scalebar(position = c("right", "bottom"), width = 9) +
  tm_add_legend(
    type = "symbols",
    labels = c("Cemetery", "Historic Site", "Campsites", "Visitor Center"),
    col = c("violetred", "aquamarine", "burlywood", "green"),
    shape = c(23, 21, 24, 22),
    title = "Map Symbols") +
  tm_add_legend(
    type = "lines",
    labels = "Trails",
    col = "black",
    lwd = 1.5,
    title = "Hiking Trails") +
  tm_add_legend(
    type = "polygons",
    labels = "District",
    fill = "orange",
    col = "blue",
    title = "Historic District") +
  tm_layout(
    legend.outside = FALSE,
    legend.position = c("left", "bottom"),
    legend.text.size = 0.6,
    legend.title.size = 0.7) + 
  tm_basemap("Esri.WorldTerrain")

map_cem_dis

tmap_mode("plot")
map_cem_dis

tm_shape(dis_py) + tm_polygons(fill = "orange", fill_alpha = 0.5, col = "blue", lwd = 2) + 
  tm_basemap("OpenTopoMap")

tmap_mode("view")
map_cem_dis

view(dist_df)

pdf("map1.pdf", width = 10, height = 5)
print(map_cem_dis)
dev.off()

library(tmap)
tm_shape(vis_cen_pt) + tm_symbols(shape = 0, size = 1, fill = "green", fill_alpha = 0.5)

# Set to static plot mode
tmap_mode("plot")

# Run your map object
map_cem_dis

##calculating distances
library(sf)
library(dplyr)

# Transform to projected CRS (meters)
camp_pt_proj <- st_transform(camp_pt, 3857)
his_pt_proj <- st_transform(his_pt, 3857)

# Calculate pairwise distances (in meters)
dist_matrix <- st_distance(camp_pt_proj, his_pt_proj)

# Convert to kilometers
dist_km <- as.data.frame(dist_matrix) / 1000

# Add row and column names
rownames(dist_km) <- paste0("Campsite_", seq_len(nrow(camp_pt_proj)))
colnames(dist_km) <- paste0("HistoricSite_", seq_len(nrow(his_pt_proj)))

# Find nearest historic site for each campsite
nearest_site <- apply(dist_km, 1, function(x) {
  site <- names(x)[which.min(x)]
  dist <- min(x)
  return(c(site, dist))
})

# Create summary table
nearest_df <- data.frame(
  Campsite = rownames(dist_km),
  Nearest_Historic_Site = nearest_site[1, ],
  Distance_km = as.numeric(nearest_site[2, ])
)

print(nearest_df)

view(dist_km)


# Histogram 
nearest_df %>%
  ggplot( aes(x=Distance_km)) +
  geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distance Between Campsites and Historic Sites") + 
  theme_bw()

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)
library(viridis)


nearest_df %>%
  ggplot(aes(x = Distance_km)) +
  geom_histogram(
    binwidth = 4,
    aes(fill = ..count..),
    color = "white",
    alpha = 0.6   # <-- Transparency added here
  ) +
  scale_fill_viridis(option = "plasma", direction = -1) +
  labs(
    title = "Distance Between Campsites and Historic Sites",
    x = "Distance (km)",
    y = "Count",
    fill = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

##background change

nearest_df %>%
  ggplot(aes(x = Distance_km)) +
  geom_histogram(
    binwidth = 4,
    aes(fill = ..count..),
    color = "white",
    alpha = 0.6
  ) +
  scale_fill_viridis(option = "plasma", direction = -1) +
  labs(
    title = "Distance Between Campsites and Historic Sites",
    x = "Distance (km)",
    y = "Count",
    fill = "Frequency"
  ) +
  theme_dark(base_size = 14) +  # Dark background
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major = element_line(color = "grey40"),
    panel.grid.minor = element_blank()
  )







###scatter plot
ggplot(nearest_df, aes(x = Campsite, y = Distance_km, color = Nearest_Historic_Site)) +
  geom_point(size = 4) +
  labs(title = "Distances Between Campsites and Historic Sites",
       x = "Campsite",
       y = "Distance (km)") +
  theme_minimal()

##improved Scatter plot
ggplot(nearest_df, aes(x = Campsite, y = Distance_km, color = Nearest_Historic_Site)) +
  geom_point(size = 4) +
  labs(title = "Distances Between Campsites and Historic Sites",
       x = "Campsite",
       y = "Distance (km)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 20, 10)
  )

##make smaller
nearest_df_subset <- nearest_df[1:15, ]  # Keep only first 15 rows

ggplot(nearest_df_subset, aes(x = Campsite, y = Distance_km, color = Nearest_Historic_Site)) +
  geom_point(size = 4) +
  labs(title = "Distances Between Campsites and Historic Sites",
       x = "Campsite",
       y = "Distance (km)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.margin = margin(10, 10, 20, 10)
  )

