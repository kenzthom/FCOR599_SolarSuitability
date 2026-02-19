#######################################################################
#Project: FCOR599 Capstone Project                                    #
#Title: Site Suitability Analysis for Utility Scale Solar Farms in BC #
#Created By: Mackenzie Thomson                                        #
#Date Created: September 26, 2026                                     #
#######################################################################

#Packages used:
library(sf)
library(dplyr)
library(terra)
library(bcmaps)
library(ggplot2)
library(AHPtools)
library(raster)
library(exactextractr)
library(tidyverse)
library(showtext)
library(tidyterra)

setwd("C:/MGEM/FCOR599/geodatabase/project_working.gdb")

#STEP 1########################################
#Data pre-processing for constraints layer ----

######BC Boundary ----

#list layers within gdb
sf::st_layers("C:/MGEM/FCOR599/raw_data/vector/gadm41_CAN.gpkg")

#choose appropriate layer
can_boundary <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/gadm41_CAN.gpkg", layer = "ADM_ADM_1")

#select BC from Canada polygon 
bc <- can_boundary %>% 
  filter(NAME_1 == "British Columbia")

#export sf polygon of bc boundary to working folder
sf::st_write(bc,  "C:/MGEM/FCOR599/geodatabase/project_working.gdb/bc_boundary.shp")

#read in bc boundary so you can delete big gpkg
bc_boundary <- sf::st_read("C:/MGEM/FCOR599/geodatabase/project_working.gdb/bc_boundary.shp")

#create bc environmental albers for later use
bc_albers <- sf::st_transform(bc_boundary, st_crs(final_mask))

#make spatvect version
bc_vect <- vect(bc_albers)

#create canada alberts equal area conic version for later use
targ <- crs(canopy_cover)
bc_lambcoco <- sf::st_transform(bc, targ)

######Land Cover Register ----

#read in LCR raster
lcr <- rast("C:/MGEM/FCOR599/raw_data/raster/LCR_RCT_2020.tif")

#make bc boundary take lcr crs
target_crs <- crs(lcr)
bc_boundary <- project(bc_vect, target_crs)

#crop raster to bc box
lcr_crop <- crop(lcr, bc_boundary) 

#mask cells outside polygon
lcr_mask <- mask(lcr_crop, bc_boundary)

#export lcr spatrast
writeRaster(lcr_mask, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/lcr_mask.tif")

#reclassify BC LCR 

#create binary mask; suitable = 1, not suitable = 0
m <- c(1, 0, #built
       2, 1, #cropland
       3, 0, #water
       4, 1, #treed
       5, 0, #treed wetland
       6, 1, #disturbance 
       7, 1, #grass/shrubland
       8, 0, #wetland
       9, 1, #sparse veg
       10, 1, #barren
       11, 0) #permanent snow 

lcr_matrix <- matrix(m, ncol = 2, byrow = TRUE)
lcr_binary <- terra::classify(lcr_mask, lcr_matrix, include.lowest = TRUE, right = TRUE)

#export
writeRaster(lcr_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/lcr_binary.tif", overwrite = TRUE)

######Canopy Cover ----

#read in canopy cover raster
canopy_cover <- rast("C:/MGEM/FCOR599/raw_data/raster/CA_canopy_cover_2022.tif")

#make bc boundary take canopy crs
target_crs <- crs(canopy_cover)
bc_boundary <- project(bc_vect, target_crs)

#crop raster to bc box
canopy_crop <- crop(canopy_cover, bc_boundary) 

#mask cells outside polygon
canopy_mask <- mask(canopy_crop, bc_boundary)

#export lcr spatrast
writeRaster(canopy_mask, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_mask.tif")

#reclassify canopy cover

#create matrix; 1 = suitable (0-40%), 0 = not suitable (40-100%)
m <- c(0, 40, 1,
       40, 100, 0)
canopy_matrix <- matrix(m, ncol = 3, byrow = TRUE)
canopy_binary <- terra::classify(canopy_mask, canopy_matrix, include.lowest = TRUE, right = TRUE)
plot(canopy_binary)

writeRaster(canopy_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary.tif", overwrite = TRUE)
canopy_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary.tif")

######Agri Land Reserve ----

#read in data 
alr <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/OATS_ALR_POLYS.gdb")

#convert sf to terra
alr_vect <- vect(alr_merged)

#make alr take canopy crs
target_crs <- crs(canopy_cover)
alr_proj <- project(alr_vect, target_crs)

#use template raster that defines the extent, resolution, and coordinate system
template_raster <- canopy_binary

#make alr a raster with polygon values assigned as 0 (not suitable)
alr_raster <- rasterize(alr_proj, template_raster, field = 0, background = 1)

#mask to bc boundary 
alr_binary <- mask(alr_raster, bc_lambcoco)

#export
writeRaster(alr_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/alr_binary.tif", overwrite = TRUE)

######Major Roads ----

#read in roads layer
roads <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_7H_MIL_ROADS_LINE.gdb")

#clip roads to bc
roads_clip <- sf::st_intersection(roads, bc_albers)

#buffer 100m
roads_buffer <- sf::st_buffer(roads_clip, 100) 

#make spatvect for terra
roads_vect <- vect(roads_buffer)

#make roads take canopy crs
target_crs <- crs(canopy_cover)
roads_proj <- project(roads_vect, target_crs)

#use template raster that defines the extent, resolution, and coordinate system
template_raster <- rast("canopy_binary.tif")

#make alr a raster with polygon values assigned as 0 (not suitable)
roads_binary <- rasterize(roads_proj, template_raster, field = 0, background = 1)

#mask to bc boundary 
roads_binary <- mask(roads_binary, bc_lambcoco)

#export
writeRaster(roads_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_binary.tif", overwrite = TRUE) 

######Major Cities ----

cities <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_BC_7H_MIL_POPULATION_POINT.gdb")

#clip to bc
cities_clip <- sf::st_intersection(cities, bc_albers)

#buffer 1000m
cities_buffer <- sf::st_buffer(cities_clip, 1000)

#make spatvect for terra
cities_vect <- vect(cities_buffer)

#make cities take canopy crs
target_crs <- crs(canopy_cover)
cities_proj <- project(cities_vect, target_crs)

#use template raster that defines the extent, resolution, and coordinate system
template_raster <- rast("canopy_binary.tif")

#make alr a raster with polygon values assigned as 0 (not suitable)
cities_binary <- rasterize(cities_proj, template_raster, field = 0, background = 1)

#mask to bc boundary 
cities_binary <- mask(cities_binary, bc_lambcoco)

#export
writeRaster(cities_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_binary.tif", overwrite = TRUE) 
cities <- ("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_binary.tif")

######Protected and Conserved Areas ----

#view layers
st_layers("C:/MGEM/FCOR599/raw_data/vector/ProtectedConservedArea_2024.gdb")

#read in appropriate layer 
pca <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/ProtectedConservedArea_2024.gdb", layer = "ProtectedConservedArea_2024")

#project to bc crs
target_crs <- crs(pca)
bc_boundary <- sf::st_transform(bc, target_crs)

#clip pca to bc
pca_clip <- sf::st_intersection(pca, bc_boundary)

#buffer 1000m
pca_buffer <- sf::st_buffer(pca_clip, 1000) 

#convert to terra
pca_buffer_vect <- vect(pca_buffer)

#prepare layer to be rasterized 
target_crs <- crs(canopy_cover)
pca_proj <- project(pca_buffer_vect, target_crs)

#use template raster that defines the extent, resolution, and coordinate system
template_raster <- rast("canopy_binary.tif")

#make alr a raster with polygon values assigned as 0 (not suitable)
pca_raster <- rasterize(pca_proj, template_raster, field = 0, background = 1)

#mask to boundary
pca_binary <- mask(pca_raster, bc_lambcoco)

writeRaster(pca_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/pca_binary.tif", overwrite = TRUE)

######Aspect ----

#REMOVED FROM CONSTRAINTS#

#create a binary raster for constraints layer, ruling out north facing aspects
m <- c(0, 45, 0, #NE
       45, 315, 1, #E,S,W
       315, 360, 0, #NW
       NA, NA, 1) #NA = flat = good
aspect_matrix <- matrix(m, ncol = 3, byrow = TRUE)

#reclassify to make binary 
aspect_binary <- terra::classify(bc_aspect, aspect_matrix, include.lowest = TRUE, right = TRUE)

#mask again?
aspect_binary <- mask(aspect_binary, bc_albers)

#export
writeRaster(aspect_binary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/aspect_binary.tif")

######ALL CONSTRAINTS BINARY MASKS ----

lcr_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/lcr_binary.tif") 
canopy_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary.tif")
alr_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/alr_binary.tif") 
pca_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/pca_binary.tif") 
roads_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_binary.tif") 
cities_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_binary.tif") 
#aspect_binary <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/aspect_binary.tif")

#reproject all to BC Albers by making a template
#will make all rasters 30m resolution, BC albers projection, and BC extent
canopy_binary_3005 <- project(rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary.tif"), "EPSG:3005", method="near") 

#use template to project the rest
lcr_binary_3005 <- project(lcr_binary, canopy_binary_3005, method = "near")
alr_binary_3005 <- project(alr_binary, canopy_binary_3005, method = "near")
pca_binary_3005 <- project(pca_binary, canopy_binary_3005, method = "near")
roads_binary_3005 <- project(roads_binary, canopy_binary_3005, method = "near")
cities_binary_3005 <- project(cities_binary, canopy_binary_3005, method = "near")

#export them all
writeRaster(lcr_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/lcr_binary_3005.tif")
writeRaster(alr_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/alr_binary_3005.tif")
writeRaster(pca_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/pca_binary_3005.tif", overwrite = TRUE)
writeRaster(roads_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_binary_3005.tif")
writeRaster(cities_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_binary_3005.tif")
writeRaster(canopy_binary_3005, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary_3005.tif")

#read em in
lcr_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/lcr_binary_3005.tif")
alr_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/alr_binary_3005.tif")
pca_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/pca_binary_3005.tif")
roads_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_binary_3005.tif")
cities_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_binary_3005.tif")
canopy_binary_3005 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary_3005.tif")

#name the layers and plot together
names(lcr_binary_3005) <- "LCR"
names(alr_binary_3005) <- "ALR"
names(pca_binary_3005) <- "Protected and Conserved Areas"
names(roads_binary_3005) <- "Major Roads"
names(cities_binary_3005) <- "Major Cities"
names(canopy_binary_3005) <- "Dense Forests"

#plot all constraints together (6 pack)
plot(c(lcr_binary_3005, alr_binary_3005, pca_binary_3005, roads_binary_3005, cities_binary_3005, canopy_binary_3005))

#merge them all together into final constraints mask!!!!
#logic:
#if any layer = 0, final mask = 0
#only if all layers = 1, final mask = 1

#combine into rast stack
all_cons <- c(lcr_binary_3005, alr_binary_3005, pca_binary_3005, roads_binary_3005, cities_binary_3005, canopy_binary_3005)

sum_cons <- app(all_cons, fun = sum) #layers represents number of layers that had 1 at that cell

#bring back to 0/1
final_mask <- sum_cons == nlyr(all_cons) 
final_mask <- as.numeric(final_mask)
plot(final_mask)

#export her keep her safe
writeRaster(final_mask, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/final_mask.tif", overwrite = TRUE)

final_mask <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/final_mask.tif")

#or...
#calculate constraints raster
#final_available <- 
#(lcr_binary_3005 *
# alr_binary_3005 *
#pca_binary_3005 *
#roads_binary_3005 *
#cities_binary_3005 *
#canopy_binary_3005)

#Correct alignment of suitability surface 
#Fix slope raster to include SAL

#STEP 2########################################
#Data pre-processing for sub-criteria ----

######Slope ----

bc_maps_avail <- available_layers()

#must download by ecoregion, check layer names
ecoprov <- ecoprovinces() 
plot(st_geometry(ecoprov))

#export ecoprov data
sf::st_write(ecoprovinces,"C:/MGEM/FCOR599/geodatabase/project_working.gdb/ecoprovinces.shp")

# load libraries
library(terra)
library(sf)
library(dplyr)

out_dir <- "C:/MGEM/FCOR599/geodatabase/project_working.gdb/SlopeByEcoprov"

# define ecoprovinces to process
ecoprovs <- c(
  "SAL", "NBM", "TAP", "BOP", "SBI",
  "SIM", "SOI", "COM", "GED", "NEP", "CEI"
)

#make final mask template
final_mask <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/final_mask.tif")
template <- final_mask

# loop through ecoprovinces
for (prov in ecoprovs) {
  
  # status message
  message("Processing ", prov)
  
  # get ecoprov boundary
  bound_sf <- ecoprovinces() %>%
    filter(ECOPROVINCE_CODE == prov)
  
  # convert to SpatVector and project
  bound_vect <- vect(bound_sf)
  bound_vect <- project(bound_vect, "EPSG:3005")
  
  # get DEM for ecoprovince
  aoi <- ecoprovinces()[ecoprovinces()$ECOPROVINCE_CODE == prov, ]
  dem <- cded_raster(aoi)
  
  # project DEM to template grid (critical step)
  dem_3005 <- project(
    rast(dem),
    template,
    method = "bilinear"
  )
  
  # calculate slope AFTER alignment
  slope <- terrain(
    dem_3005,
    v = "slope",
    neighbors = 8,
    unit = "degrees"
  )
  
  # mask slope to ecoprovince boundary
  slope_clipped <- mask(slope, bound_vect)
  
  # write slope raster
  writeRaster(
    slope_clipped,
    file.path(out_dir, paste0(prov, "_slope_v2.tif")),
    overwrite = TRUE
  )
  
  # clean memory
  rm(dem, dem_3005, slope, slope_clipped, bound_vect)
  gc()
}

# list all slope rasters
slope_files <- list.files(
  path = out_dir,
  pattern = "_slope_v2\\.tif$",
  full.names = TRUE
)

# load slope rasters
slope_list <- lapply(slope_files, rast)

# merge all slopes into BC-wide raster
bc_slope <- do.call(merge, slope_list)

bc_slope_final <- mask(bc_slope, final_mask)

# write merged BC slope raster
writeRaster(bc_slope_final, "C:/MGEM/FCOR599/geodatabase/second_version/bc_slope_full.tif") #IT WORKEDDD

bc_slope_final <- rast("C:/MGEM/FCOR599/geodatabase/second_version/bc_slope_full.tif")

#reclassify slope to 5 bin scale
m <- c(0, 2, 5, #best
       2, 5, 4, 
       5, 10, 3, 
       10, 15, 2, 
       15, 90, 1) #worst 
slope_matrix <- matrix(m, ncol = 3, byrow = TRUE)
slope_reclass <- terra::classify(bc_slope_final, slope_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(slope_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/slope_reclassified.tif", overwrite = TRUE)

######Aspect ----

#process exact same as slope, using same template (final_mask)

out_dir <- "C:/MGEM/FCOR599/geodatabase/project_working.gdb/AspectByEcoprov"

ecoprovs <- c(
  "SAL", "NBM", "TAP", "BOP", "SBI",
  "SIM", "SOI", "COM", "GED", "NEP", "CEI"
)

template <- final_mask  

for (prov in ecoprovs) {
  
  message("Processing ", prov)
  
  # get ecoprov boundary
  bound_sf <- ecoprovinces() %>%
    filter(ECOPROVINCE_CODE == prov)
  
  bound_vect <- vect(bound_sf)
  bound_vect <- project(bound_vect, "EPSG:3005")
  
  # get DEM
  aoi <- ecoprovinces()[ecoprovinces()$ECOPROVINCE_CODE == prov, ]
  dem <- cded_raster(aoi)
  
  # project DEM to TEMPLATE GRID (critical)
  dem_3005 <- project(
    rast(dem),
    template,
    method = "bilinear"
  )
  
  # calculate aspect AFTER alignment
  aspect <- terrain(
    dem_3005,
    v = "aspect",
    neighbors = 8,
    unit = "degrees"
  )
  
  # mask to ecoprovince
  aspect_clipped <- mask(aspect, bound_vect)
  
  # write aspect raster
  writeRaster(
    aspect_clipped,
    file.path(out_dir, paste0(prov, "_aspect_v2.tif")),
    overwrite = TRUE
  )
  
  # clean memory
  rm(dem, dem_3005, aspect, aspect_clipped, bound_vect)
  gc()
}

# list all aspect rasters
aspect_files <- list.files(
  path = out_dir,
  pattern = "_aspect_v2\\.tif$",
  full.names = TRUE
)

# load rasters
aspect_list <- lapply(aspect_files, rast)

# merge (already aligned)
bc_aspect <- do.call(merge, aspect_list)

# mask to final BC mask (same as slope)
bc_aspect_final <- mask(bc_aspect, final_mask)

# write BC-wide aspect
writeRaster(bc_aspect_final, "C:/MGEM/FCOR599/geodatabase/second_version/bc_aspect_full.tif", overwrite = TRUE)

#reclassify 
m <- c(315, 360, 1, #N
       0, 45, 1, #N (wrap-around)
       45, 135, 3, #E
       135, 225, 5, #S
       225, 315, 3) #W
aspect_matrix <- matrix(m, ncol = 3, byrow = TRUE)
aspect_reclass <- terra::classify(bc_aspect_final, aspect_matrix, include.lowest = TRUE, right = TRUE)

writeRaster(aspect_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/aspect_reclassified.tif", overwrite = TRUE)

######GHI ----

#read in raster
GHI <- rast("C:/MGEM/FCOR599/raw_data/raster/PVOUT_data/Canada_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif")

#project to template raster
ghi_proj <- project(GHI, template, method = "bilinear")

#clip to bc
ghi_crop <- crop(ghi_proj, template)

#mask to bc
ghi_mask <- mask(ghi_crop, template) 

#save
writeRaster(ghi_mask, "C:/MGEM/FCOR599/geodatabase/second_version/ghi_cleaned.tif", overwrite = TRUE)

#reclassify to 1-5 scale
m <- c(0, 600, 1, #worst
       600, 800, 2,
       800, 1000, 3,
       1000, 1200, 4,
       1200, 2000, 5) #best
ghi_matrix <- matrix(m, ncol = 3, byrow = TRUE)
ghi_reclass <- terra::classify(ghi_mask, ghi_matrix, include.lowest = TRUE, right = TRUE)

#export
writeRaster(ghi_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/ghi_reclassified.tif", overwrite = TRUE)

######Proximity to Roads ----

#read in roads layer
roads <- st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_7H_MIL_ROADS_LINE.gdb") 

#reproject first 
roads_proj <- st_transform(roads, st_crs(final_mask))

#clip
roads_clip <- st_intersection(roads_proj, bc_albers)

#convert to spatvect 
roads_vect <- vect(roads_clip)

#rastertize using template
roads_rast <- rasterize(roads_vect, final_mask, field = 1, background = NA)

#create continuous distance from roads
dist_roads <- distance(roads_rast) 

#classify into 5 bins --> rescaled to be linear and wider ranges 
m <- c(0, 1000, 5,       #best
       1000, 5000, 4, 
       5000, 15000, 3,
       15000, 30000, 2,
       30000, Inf, 1)     #worst
roads_matrix <- matrix(m, ncol = 3, byrow = TRUE)
roads_reclass <- terra::classify(dist_roads, roads_matrix, include.lowest = TRUE, right = FALSE)

#mask output
roads_reclass <- mask(roads_reclass, final_mask) 

#export
writeRaster(roads_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/preprocessed_v2/roads_reclassified_v2.tif", overwrite = TRUE)

######Proximity to Cities ----

cities <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_BC_7H_MIL_POPULATION_POINT")

#reproject first 
cities_proj <- st_transform(cities, st_crs(template))

#clip
cities_clip <- st_intersection(cities_proj, bc_albers)

#convert to spatvect 
cities_vect <- vect(cities_clip)

#rastertize using template
#crashed R - skipped this step because you can create a distance raster from a spatvect 
cities_rast <- rasterize(cities_vect, template, field = 1, filename = "C:/MGEM/FCOR599/geodatabase/second_version/cities_rast.tif", background = NA)

#create continuous distance from roads
dist_cities <- distance(template, cities_vect)  

#mask
cities_mask <- mask(dist_cities, template) 

#classify into 5 bins
m <- c(0, 1000, NA, #within buffer
       1000, 5000, 5, #best within 1-5km
       5000, 15000, 4, #5-15km
       15000, 30000, 3, #15-30km
       30000, 45000, 2, #30-45km
       45000, Inf, 1)
cities_matrix <- matrix(m, ncol = 3, byrow = TRUE)
cities_reclass <- terra::classify(cities_mask, cities_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(cities_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/cities_reclassified.tif", overwrite = TRUE)

######Proximity to Existing Infrastructure ----

substations <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/substations.shp")

#drop z coordinate
substations_2d <- st_zm(substations, drop = TRUE)

#reproject first 
subs_proj <- st_transform(substations_2d, st_crs(template))

#clip
subs_clip <- st_intersection(subs_proj, bc_albers)

#convert to spatvect 
subs_vect <- vect(subs_clip)

#rasterize
subs_raster <- rasterize(subs_vect, template, field = 1, background = NA)

#create continuous distance from road
dist_subs <- distance(template, subs_vect)

#mask to bc boundary 
subs_mask <- mask(dist_subs, template)

#classify into 5 bins
m <- c(0, 500, 5, 
       500, 1500, 4, 
       1500, 3000, 3, 
       3000, 6000, 2, 
       6000, Inf, 1)
subs_matrix <- matrix(m, ncol = 3, byrow = TRUE)
subs_reclass <- classify(subs_mask, subs_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(subs_reclass, "C:/MGEM/FCOR599/geodatabase/second_version/substations_reclassified.tif", overwrite = TRUE)

######ALL RECLASSIFIED SUB CRITERA ----

slope_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed/slope_reclassified.tif")
aspect_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed/aspect_reclassified.tif")
ghi_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed/ghi_reclassified.tif")
roads_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed_v2/roads_reclassified_v2.tif")
cities_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed/cities_reclassified.tif")
substations_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/second_version/preprocessed/substations_reclassified.tif") 

names(slope_reclassified) <- "Slope"
names(aspect_reclassified) <- "Aspect"
names(ghi_reclassified) <- "GHI"
names(roads_reclassified) <- "Major Roads"
names(cities_reclassified) <- "Major Cities"
names(substations_reclassified) <- "Substations"

plot(c(slope_reclassified, aspect_reclassified, ghi_reclassified, roads_reclassified, cities_reclassified, substations_reclassified))

###TEST GEOMETRY
compareGeom(slope_reclassified, aspect_reclassified, ghi_reclassified, roads_reclassified, cities_reclassified, substations_reclassified)
datatype(c(slope_reclassified, aspect_reclassified, ghi_reclassified, roads_reclassified, cities_reclassified, substations_reclassified))

#STEP 3########################################
#AHP ----

criteria <- c("GHI", "Slope", "Aspect", "Roads", "Cities", "Energy")

#use 9-point scale to create top triangle (bottom = recipricals)
pcm_values <- c(
  9, 3, 3, 1, 1,                #GHI 
  (1/5), (1/3), (1/5), (1/7),   #slope 
  1, (1/3), (1/3),              #aspect 
  (1/3), (1/3),                 #roads 
  1                             #cities 
)

#build the matrix
pcm <-createPCM(pcm_values)
pcm

#get weights and consistency ratio
CR(pcm)

#isolate weights
weights_raw <- CR(pcm)[3]

#change from list to df
weights_raw <- as.data.frame(weights_raw)

#normalize 
weights_norm <- weights_raw / sum(weights_raw) 

#convert to percent
weights_percent <- weights_norm * 100

sum(weights_norm) #must = 1

AHP_FinalWeights <- as.data.frame(weights_percent, criteria)
AHP_FinalWeights

#STEP 4########################################
#Weighted Sum Overlay ----


#pull normalized weights in decimal form
ahp_DecWeights <- as.data.frame(weights_norm, criteria)

#assign normalized weights
w <- c(GHI = 0.26966158, 
       slope = 0.03273008,       
       aspect = 0.10063181,      
       roads = 0.08988719,       
       cities = 0.24817236,       
       energy = 0.25891697)

#multiply layer by respective weight, add together (weighted sum)
suitability_raw <-
  ghi_reclassified * w["GHI"] +
  slope_reclassified * w["slope"] +
  aspect_reclassified * w["aspect"] +
  roads_reclassified * w["roads"] +
  cities_reclassified * w["cities"] +
  substations_reclassified * w["energy"]

#suitability across province - gradient 1-5 - not masked
writeRaster(suitability_raw, "C:/MGEM/FCOR599/geodatabase/second_version/suitabilitySurfaces_v2/suitability_raw.tif", overwrite = TRUE)
suitability_raw <- rast("C:/MGEM/FCOR599/geodatabase/second_version/suitabilitySurfaces_v2/suitability_raw.tif")

#transform into categorical raster (1-5) using equal intervals

#get min/max of raw data
suit_min <- global(suitability_raw, "min", na.rm = TRUE)[1,1] #add 1,1 to pull number from output table
suit_max <- global(suitability_raw, "max", na.rm = TRUE)[1,1]

#create 5 equal interval breaks between min and max
breaks <- seq(suit_min, suit_max, length.out = 6)

#reclassify using interval breaks
m <- c(breaks[1], breaks[2], 1,
       breaks[2], breaks[3], 2,
       breaks[3], breaks[4], 3,
       breaks[4], breaks[5], 4,
       breaks[5], breaks[6], 5)
suit_matrix <- matrix(m, ncol = 3, byrow = TRUE)
suitability_scaled <- terra::classify(suitability_raw, suit_matrix, include.lowest = TRUE, right = FALSE)

#save scaled 1-5 with no 0 for layer

#add mask 
suitability_scaled[final_mask == 0] <- 0

#export final categorical layer
writeRaster(suitability_scaled, "C:/MGEM/FCOR599/geodatabase/second_version/suitabilitySurfaces_v2/FinalSuitability.tif")

#STEP 5########################################
#Exploring Results ----

#read in final layers
suitability_scaled <- rast("C:/MGEM/FCOR599/geodatabase/second_version/suitabilitySurfaces_v2/FinalSuitability.tif")

#BC total area division summary generation:

#make summary table
suitability_summary <- as.data.frame(freq(suitability_scaled))

#get area of 1 pixel
pixel_area_km2 <- prod(res(suitability_scaled)) / 1e6

#add and populate area in km2 column 
suitability_summary$area_km2 <- suitability_summary$count * pixel_area_km2

#add acres column 
suitability_summary$acres <- suitability_summary$area_km2 * 247.105 

#add percent of total land each class occupies
sum(suitability_summary$area_km2) #947010.8 km2
suitability_summary$percent <- suitability_summary$area_km2 / 947010.8 * 100

#save for later
write.csv(suitability_summary, "C:/MGEM/FCOR599/geodatabase/second_version/zonalStats/results_summary_v2.csv", row.names = FALSE)

#constraints plotting:

#plot each constraint mask individually 
plot(lcr_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)
plot(alr_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)
plot(pca_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)
plot(roads_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)
plot(cities_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)
plot(canopy_binary_3005,
     col = c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE)

#plot final mask 
plot(final_mask,
     col= c("red", "green"),   # 0 = red, 1 = green
     legend = TRUE,
     main = "Final Constraints Mask")

#suitability class per ecoprovince generation: 

#download ecoprovs sf
ecoprovs <- ecoprovinces()

#ensure alignment
ecoprovs <- project(ecoprovs, final_mask)

#make suitability scaled raster not spat rast
suitability_rast <- raster(suitability_scaled)

#calculate zonal stats
zonal_stats <- exact_extract(suitability_rast, ecoprovs, function(values, coverage_fraction) {
  tbl <- table(factor(values, levels=1:5))       # count each class
  prop <- tbl / sum(tbl)                         # convert to proportion
  return(as.list(prop))
})

#make df
zonal_df <- as.data.frame(zonal_stats)
names(zonal_df)

#replace NaN with NA 
zonal_df[zonal_df == "NaN"] <- NA
zonal_df <- as.data.frame(lapply(zonal_df, as.numeric))

#assign names
eco_names <- ecoprovs$ECOPROVINCE_NAME 
names(zonal_df)[names(zonal_df) != "class"] <- eco_names

#make class column 
zonal_df$class <- 1:5

#pivot longer
zonal_df_long <- zonal_df %>%
  pivot_longer(!class,
               names_to = "ecoprovince",
               values_to = "cell_count")

#convert table counts to area per class per ecoprovince
zonal_df_long$area_km2 <- zonal_df_long$cell_count * pixel_area_km2

#calculate proportions
zonal_df_prop <- zonal_df_long %>%
  group_by(ecoprovince) %>%
  mutate(
    prop = area_km2 / sum(area_km2, na.rm = TRUE)
  ) %>%
  ungroup()

#check to make sure each ecoprov = 1
zonal_df_prop %>%
  group_by(ecoprovince) %>%
  summarise(sum_prop = sum(prop, na.rm = TRUE))

#remove northeast pacific ecoprov 
zonal_df_prop <- zonal_df_prop %>%
  filter(ecoprovince != "NORTHEAST PACIFIC")

#export as csv
write.csv(zonal_df_prop, "C:/MGEM/FCOR599/geodatabase/second_version/zonalStats/ZonStats_byEcoprov_v2.csv")
zonal_df_prop <- read.csv("C:/MGEM/FCOR599/geodatabase/second_version/zonalStats/ZonStats_byEcoprov_v2.csv")

#plot
class_colors <- c(
  "1" = "red",      
  "2" = "orange",     
  "3" = "yellow",     
  "4" = "green",      
  "5" = "dark green")

#fix labels for plotting 
zonal_df_prop$ecoprovince <- tools::toTitleCase(
  tolower(zonal_df_prop$ecoprovince))

#add constantia font before plotting
font_add("Constantia", "C:/Windows/Fonts/constan.ttf")
showtext_auto()

ggplot(zonal_df_prop, aes(fill = factor(class), y = prop, x = ecoprovince)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = class_colors, 
    name = "Suitability Class",
    labels = c("1" = "Class 1 - Least Suitable",
               "2" = "Class 2 - Less Suitability",
               "3" = "Class 3 - Moderately Suitable",
               "4" = "Class 4 - Suitable",
               "5" = "Class 5 - Very Suitable")) +
  labs(x = "Ecoprovince", 
       y = expression("Proportion of Area (km"^2*")"), 
       fill = "Suitability Class") +
  theme_classic(base_family = "Constantia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#transform to match excel format 
zonal_df_wide <- zonal_df_prop %>%
  pivot_wider(names_from = ecoprovince, values_from = prop)

write.csv(zonal_df_wide,  "C:/MGEM/FCOR599/geodatabase/second_version/zonalStats/ZonStats_byEcoprov_wide_v2.csv")

#STEP 6########################################
#PVOUT ----

#clean and align pvout data
pvout <- rast("C:/MGEM/FCOR599/raw_data/raster/PVOUT_data/Canada_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/PVOUT.tif")

#project to suitability data so you can crop
pvout_proj <- project(pvout, template, method = "bilinear")

#crop and mask
pvout_crop <- crop(pvout_proj, template)
pvout_mask <- mask(pvout_crop, template)

writeRaster(pvout_mask, "C:/MGEM/FCOR599/geodatabase/second_version/pvout/pvout_clean.tif", overwrite = TRUE)
pvout_clean <- rast("C:/MGEM/FCOR599/geodatabase/second_version/pvout/pvout_clean.tif")

#workflow to identify patches in classes 4 and 5

#isolate each class; now everything other than the cls is NA
class4 <- suitability_scaled
class4[suitability_scaled != 4] <- NA 
writeRaster(class4, "C:/MGEM/FCOR599/geodatabase/second_version/pvout/suitability_cl4.tif", overwrite=TRUE)
class4 <- rast("C:/MGEM/FCOR599/geodatabase/second_version/pvout/suitability_cl4.tif")

class5 <- suitability_scaled
class5[suitability_scaled != 5] <- NA 
writeRaster(class5, "C:/MGEM/FCOR599/geodatabase/second_version/pvout/suitability_cl5.tif", overwrite=TRUE)
class5 <- rast("C:/MGEM/FCOR599/geodatabase/second_version/pvout/suitability_cl5.tif")

#RASTER TO POLYGON CONVERSION DONE IN ARC

#read in polygons 
class4_poly <- st_read("C:/MGEM/FCOR599/geodatabase/second_version/pvout/class_4_polygons.shp")
class5_poly <- st_read("C:/MGEM/FCOR599/geodatabase/second_version/pvout/class_5_polygons.shp")

#get area and filter to minimum map unit (mmu) of 5 acres
#divide m2 by 4047 to get acres
class4_poly$area_acres <- class4_poly$Shape_Area / 4047.85642 
class4_poly$area_km <- class4_poly$Shape_Area / 1e6
class4_mmu <- class4_poly[class4_poly$area_acres > 5, ]

class5_poly$area_acres <- class5_poly$Shape_Area / 4047.85642
class5_poly$area_km <- class5_poly$Shape_Area / 1e6
class5_mmu <- class5_poly[class5_poly$area_acres > 5, ] 

#make spatvectors for processing
cl4_mmu_vect <- vect(class4_mmu)
cl5_mmu_vect <- vect(class5_mmu)

#Equation 1: Installable Capacity 
# Ci = Ai x Y x GCR 
# Ai = installable area of polygons (in m2)
# Y = representative panel yield - 0.2 used (20% efficiency) 
# GCR = ground cover ratio - 0.5 used (accounts for fact that 100% of ground isnt solar panels)
# calculate per polygon and then take average 

class4_mmu$Ci <- class4_mmu$Shape_Area * (0.2 * 0.5)
class5_mmu$Ci <- class5_mmu$Shape_Area * (0.2 * 0.5) 
mean(class4_mmu$Ci)
mean(class5_mmu$Ci)

#Equation 2: Annual Energy Production 
# Ei = Ci x PVOUTi
# Ci = installable capacity 
# PVOUTi = mean pvout per polygon 
# get mean pvout per polygon using zonal stats, then calculate Ei

cl4_pvout_mean <- terra::extract(pvout_clean, cl4_mmu_vect, fun = mean, na.rm = TRUE)
cl5_pvout_mean <- terra::extract(pvout_clean, cl5_mmu_vect, fun = mean, na.rm = TRUE)

class4_mmu$pvout_mean <- cl4_pvout_mean[,2] #column 2 is mean pvout 
class5_mmu$pvout_mean <- cl5_pvout_mean[,2]

class4_mmu$Ei <- class4_mmu$Ci * class4_mmu$pvout_mean
class5_mmu$Ei <- class5_mmu$Ci * class5_mmu$pvout_mean

print(sum(class4_mmu$Ei), digits = 15)
print(sum(class5_mmu$Ei), digits = 15)

#get average pvout across all polygons 
mean(class4_mmu$pvout_mean)
mean(class5_mmu$pvout_mean)
