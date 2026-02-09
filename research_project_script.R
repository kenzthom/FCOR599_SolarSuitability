#FCOR599 Capstone: Site Suitability Analysis for Solar Farms in BC
#Mackenzie Thomson 

#Packages used:
library(sf)
library(dplyr)
library(terra)
library(bcmaps)
library(ggplot2)
library(AHPtools)
library(gtools) 
library(landscapemetrics)
library(exactextractr)
library(tidyverse)

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

# set output directory
out_dir <- "C:/MGEM/FCOR599/geodatabase/project_working.gdb"

# define ecoprovinces to process
ecoprovs <- c(
  "SAL", "NBM", "TAP", "BOP", "SBI",
  "SIM", "SOI", "COM", "GED", "NEP", "CEI"
)

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
writeRaster(bc_slope_final, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/BC_SLOPE_FULL.tif") #IT WORKEDDD

#reclassify slope to 5 bin scale
m <- c(0, 2, 5, #best
       2, 5, 4, 
       5, 10, 3, 
       10, 15, 2, 
       15, 90, 1) #worst 
slope_matrix <- matrix(m, ncol = 3, byrow = TRUE)
slope_reclass <- terra::classify(slope_project, slope_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(slope_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/slope_reclassified.tif", overwrite = TRUE)

######Aspect ----

#create aspect layer from DEM
ecoprovs <- c("BOP", "SBI", "SIM", "SOI", "COM", "GED", "NEP", "CEI", "TAP", "NBM", "SAL" )

for (prov in ecoprovs) {
  message("Processing ", prov)
  
  #get ecoprov boundary
  ecoprov_bound <- ecoprov %>% filter(ECOPROVINCE_CODE == prov)
  
  #convert to spatvect
  vect_bound <- vect(ecoprov_bound)
  
  #create DEM
  aoi <- ecoprovinces()[ecoprovinces()$ECOPROVINCE_CODE == prov, ]
  dem <- cded_raster(aoi)
  
  #export dem 
  writeRaster(dem, paste0(prov, "_dem.tif"), overwrite = TRUE)
  
  #project DEM and boundary first - so aspect can be calculated 
  dem <- rast(dem)
  dem <- project(dem, "EPSG:3005")
  
  #compute aspect from projected DEM
  aspect <- terrain(dem, v = "aspect", unit = "degrees", neighbors = 8)
  
  #rename layer 
  names(aspect) <- "aspect"
  
  #crop and mask aspect to boundary
  aspect_clipped <- mask(aspect, vect_bound) 
  
  #export slope raster
  writeRaster(aspect_clipped, paste0(prov, "_aspect.tif"), overwrite = TRUE)
  
  #clean memory
  rm(dem, aspect, aspect_clipped, vect_bound)
  gc()
}

#list all saved slope rasters in files
aspect_directory <- ("C:/MGEM/FCOR599/geodatabase/project_working.gdb")
aspect_files <- list.files(path = aspect_directory, pattern = "_aspect\\.tif$")
print(aspect_files)

#load into a spatrast list
aspect_list <- lapply(aspect_files, rast)

#clean names
names(aspect_list) <- gsub("_aspect\\.tif$", "", basename(aspect_files))
names(aspect_list)
print(aspect_list)

#merge all together
#have to remove names for merge function to work 
bc_aspect <- do.call(merge, unname(aspect_list))
bc_aspect <- mask(bc_aspect, bc_albers)

#export unified aspect raster for bc!! :-)
writeRaster(bc_aspect, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/BC_ASPECT.tif")

#project to template raster
aspect_proj <- project(bc_aspect, canopy_binary_3005, method = "bilinear")

#reclassify 
m <- c(315, 360, 1, #N
       0, 45, 1, #N (wrap-around)
       45, 135, 3, #E
       135, 225, 5, #S
       225, 315, 3) #W
aspect_matrix <- matrix(m, ncol = 3, byrow = TRUE)
aspect_reclass <- terra::classify(aspect_proj, aspect_matrix, include.lowest = TRUE, right = TRUE)

writeRaster(aspect_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/aspect_reclassified.tif", overwrite = TRUE)

######GHI ----

#read in raster
GHI <- rast("C:/MGEM/FCOR599/raw_data/raster/PVOUT_data/Canada_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/GHI.tif")

#clip to bc
ghi_crop <- crop(GHI, bc)

#mask to bc
ghi_mask <- mask(ghi_crop, bc) #still unprojected

#project to template raster
ghi_3005 <- project(ghi_mask, canopy_binary_3005, method = "bilinear")

#reclassify to 1-5 scale
m <- c(0, 600, 1,
       600, 800, 2,
       800, 1000, 3,
       1000, 1200, 4,
       1200, 2000, 5) #best
ghi_matrix <- matrix(m, ncol = 3, byrow = TRUE)
ghi_reclass <- terra::classify(ghi_3005, ghi_matrix, include.lowest = TRUE, right = TRUE)

#export
writeRaster(ghi_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/ghi_reclassified.tif", overwrite = TRUE)

######Proximity to Roads ----

#read in roads layer
roads <- st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_7H_MIL_ROADS_LINE.gdb") |>
  st_transform(3005)

#clip
roads_clip <- st_intersection(roads, bc_albers)

#export so you can delete country wide data
sf::st_write(roads_clip, "C:/MGEM/FCOR599/raw_data/vector/roads_clip.shp", overwrite = TRUE)

#convert to terra vector
roads_vect <- vect(roads_clip)

#load template raster
template_raster <- rast("canopy_binary_3005.tif")

#project to template raster
roads_proj <- project(roads_vect, crs(template_raster))

# Rasterize
roads_raster <- rasterize(roads_proj, template_raster, field = 1,  background = NA)

#create continuous distance from road
dist_road <- distance(roads_raster)

#mask to bc boundary 
dist_roads_mask <- mask(dist_road, bc_albers)

#classify into 5 bins#cldist_roads_maskassify into 5 bins
m <- c(0, 100, NA, #within buffer
       100, 300, 5, #best within 100-300m
       300, 1000, 4, 
       1000, 2000, 3,
       2000, 5000, 2,
       5000, Inf, 1)
roads_matrix <- matrix(m, ncol = 3, byrow = TRUE)
roads_reclass <- terra::classify(dist_roads_mask, roads_matrix, include.lowest = TRUE, right = FALSE)

#export
writeRaster(roads_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_reclassified.tif", overwrite = TRUE)

######Proximity to Cities ----

cities <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/DBM_BC_7H_MIL_POPULATION_POINT.gdb")

#clip
cities_clip <- st_intersection(cities, bc_albers)

#convert to terra vector
cities_vect <- vect(cities_clip)

#load template raster
template_raster <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary_3005.tif")

#upsample to reduce storage size
template_coarse <- aggregate(template_raster, fact = 5)

# Rasterize
cities_raster <- rasterize(cities_vect, template_coarse, field = 1,  background = NA)

#create continuous distance from road
dist_cities <- distance(cities_raster)

#mask to bc boundary 
dist_cities_mask <- mask(dist_cities, bc_albers)

#downsample back to 30x30m template
distance_cities <- resample(dist_cities_mask, template_raster, method = "bilinear")

#classify into 5 bins
m <- c(0, 1000, NA, #within buffer
       1000, 2000, 5, #best within 1-2km
       2000, 3000, 4, 
       3000, 6000, 3,
       6000, 10000, 2, 
       10000, Inf, 1)
cities_matrix <- matrix(m, ncol = 3, byrow = TRUE)
cities_reclass <- terra::classify(distance_cities, cities_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(cities_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_reclassified.tif", overwrite = TRUE)

######Proximity to Existing Infrastructure ----

dir.create("C:/MGEM/FCOR599/temp", showWarnings = FALSE)

substations <- sf::st_read("C:/MGEM/FCOR599/raw_data/vector/substations.shp")

#drop z coordinate
substations_2d <- st_zm(substations, drop = TRUE)
subs_vect <- vect(substations_2d)

#convert to terra vector
#subs_vect <- vect(substations)

#load template raster
template_raster <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/canopy_binary_3005.tif")

#upsample to reduce storage size
template_coarse <- aggregate(template_raster, fact = 5)

#rasterize
subs_raster <- rasterize(subs_vect, template_coarse, field = 1, background = NA)

#create continuous distance from road
dist_subs <- distance(subs_raster)

#mask to bc boundary 
dist_subs_mask <- mask(dist_subs, bc_albers)

#resample back to 30x30m template
distance_subs <- resample(dist_subs_mask, template_raster, method = "bilinear")

#classify into 5 bins
m <- c(0, 500, 5, 
       500, 1500, 4, 
       1500, 3000, 3, 
       3000, 6000, 2, 
       6000, Inf, 1)
subs_matrix <- matrix(m, ncol = 3, byrow = TRUE)
subs_reclass <- classify(distance_subs, subs_matrix, include.lowest = TRUE, right = FALSE)

writeRaster(subs_reclass, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/substations_reclassified.tif", overwrite = TRUE)

######ALL RECLASSIFIED SUB CRITERA ----

slope_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/slope_reclassified.tif")
aspect_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/aspect_reclassified.tif")
ghi_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/ghi_reclassified.tif")
roads_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/roads_reclassified.tif")
cities_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cities_reclassified.tif")
substations_reclassified <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/substations_reclassified.tif") 

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

criteria <- c("GHI", "Slope", "Aspect", "Roads", "Urban", "Energy")

#use 9-point scale to create top triangle (bottom = recipricals)
pcm_values <- c(
  7, 9, 5, 6, 4,  #GHI vs others
  2, 1, 2, 0.5,   #slope vs others
  0.6, 1, 1,      #aspect vs others
  3, 2,           #roads vs others
  1               #urban vs Energy
)

#build the matrix
pcm <-createPCM(pcm_values)
pcm

#get weights and consistency ratio
CR(pcm)

weights_raw <- c(0.9239522, 0.1753882, 0.1183795, 0.2381606, 0.1164439, 0.1767993) 

#normalize 
weights_norm <- weights_raw / sum(weights_raw) 
weights_percent <- weights_norm * 100

sum(weights_norm) #must = 1

as.data.frame(weights_percent, criteria)

#STEP 4########################################
#Weighted Sum Overlay ----


#pull normalized weights in decimal form
ahp_weights <- as.data.frame(weights_norm, criteria)

#assign normalized weights
w <- c(GHI = 0.52823720, 
       slope = 0.10027204,       
       aspect = 0.06767932,      
       roads = 0.13615995,       
       cities = 0.06657271,       
       energy = 0.10107879)

suitability_raw <-
  ghi_reclassified * w["GHI"] +
  slope_reclassified * w["slope"] +
  aspect_reclassified * w["aspect"] +
  roads_reclassified * w["roads"] +
  cities_reclassified * w["cities"] +
  substations_reclassified * w["energy"]

writeRaster(suitability_raw, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_raw.tif")
suitability_raw <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_raw.tif")
plot(suitability_raw)

#mask suitability surface using constraints
#this removes unsuitable locations and sets them as NA
suitability_final <- mask(suitability_raw, final_mask, maskvalue = 0)

#calculate final raster that keeps masked values
suitability_masked <- suitability_raw * final_mask

writeRaster(suitability_final, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_final.tif")
suitability_final <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_final.tif")
plot(suitability_final)

#transform into categorical raster (1-5)

#check min/max of raw data
suit_min <- global(suitability_final, "min", na.rm = TRUE)[1,1] #add 1,1 to pull number from output table
suit_max <- global(suitability_final, "max", na.rm = TRUE)[1,1]

#create 5 equal interval breaks between min and max
breaks <- seq(suit_min, suit_max, length.out = 6)
m <- c(breaks[1], breaks[2], 1,
       breaks[2], breaks[3], 2,
       breaks[3], breaks[4], 3,
       breaks[4], breaks[5], 4,
       breaks[5], breaks[6], 5)
suit_matrix <- matrix(m, ncol = 3, byrow = TRUE)

#reclassify
suitability_scaled <- terra::classify(suitability_final, suit_matrix, include.lowest = TRUE, right = FALSE)

#assign correct name
names(suitability_scaled) <- "suitability_scaled"

#export... this shows scaled raster weighed from 1-5 with constraints set as NA (not retained)
writeRaster(suitability_scaled, "C:/MGEM/FCOR599/geodatabase/project_final.gdb/suitability_scaled.tif")

#make another that retains 0 in scale so you can calculate area of it
suitability_scaled[final_mask == 0] <- 0

WriteRaster(suitability_scaled, "C:/MGEM/FCOR599/geodatabase/project_final.gdb/suitability_scaled_with0.tif", overwrite = TRUE)

#ensure alignment with constraints mask
suitability_aligned <- align(suitability_scaled_w0, final_mask, method="near")

#export...this shows scaled raster weighed from 1-5 with constraints set as 0 (retained)
writeRaster(suitability_aligned, "C:/MGEM/FCOR599/geodatabase/project_final.gdb/suitability_aligned.tif", overwrite = TRUE)


#STEP 5########################################
#Exploring Results ----

constraints_summary <- read.csv("C:/MGEM/FCOR599/geodatabase/project_working.gdb/constraints_summary.csv")
suitability_summary <- read.csv("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_summary.csv")

suitability_scaled <- rast("C:/MGEM/FCOR599/geodatabase/project_final.gdb/suitability_scaled.tif")
suitability_scaled_w0 <- rast("C:/MGEM/FCOR599/geodatabase/project_final.gdb/suitability_scaled_with0.tif")

#make a summary df without constraints
suitability_summary <- as.data.frame(freq(suitability_scaled))
#get area of 1 pixel
pixel_area_km2 <- prod(res(suitability_scaled)) / 1e6
#add and populate area column 
suitability_summary$area_km2 <- suitability_summary$count * pixel_area_km2
#save for later
write.csv(suitability_summary, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_summary.csv", row.names = FALSE)

sum(suitability_summary$area_km2) #224591.3 km2 of suitable land (classes 1-5)

#make a summary df with constraints
suitability_summary_w0 <- as.data.frame(freq(suitability_scaled_w0))
suitability_summary_w0$area_km2 <- suitability_summary_w0$count * pixel_area_km2
write.csv(suitability_summary_w0, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_summary_w0.csv", row.names = FALSE)

#add fraction and percent of total land each category occupies
sum(suitability_summary_w0$area_km2) #948539.3 km2
suitability_summary_w0$fraction <- suitability_summary_w0$area_km2 / 948539.3
suitability_summary_w0$percent <- suitability_summary_w0$area_km2 / 948539.3 * 100
suitability_summary_w0

#calculate summary df that shows pixel count & area of both 1 and 0 
constraints_rasters <- list(
  LCR = lcr_binary_3005,
  ALR = alr_binary_3005,
  PCA = pca_binary_3005,
  Roads = roads_binary_3005,
  Cities = cities_binary_3005,
  Forests = canopy_binary_3005
)

constraints_summary <- lapply(names(constraints_rasters), function(nm) {
  r <- constraints_rasters[[nm]]
  
  #get frequency table (as df)
  f <- as.data.frame(freq(r, value=TRUE))
  
  #count pixels == 1
  count_1 <- global(r, "sum", na.rm = TRUE)[1,1]
  
  #subtract remaining pixels to get pixels == 0
  count_0 <- global(!is.na(r), "sum")[1,1] - count_1
  
  pixel_area_km2 <- prod(res(r)) / 1e6
  
  data.frame(
    raster = nm, 
    value = c(0,1),
    count = c(count_0, count_1),
    area_km2 = c(count_0, count_1) * pixel_area_km2
  )
})

#combine all into one data frame
constraints_summary_df <- do.call(rbind, constraints_summary)

#save it for later
write.csv(constraints_summary_df, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/constraints_summary.csv", row.names = FALSE)

#STEP 6########################################
#PVOUT ----

#read in pvout data
pvout <- rast("C:/MGEM/FCOR599/raw_data/raster/PVOUT_data/Canada_GISdata_LTAy_YearlyMonthlyTotals_GlobalSolarAtlas-v2_GEOTIFF/PVOUT.tif")

#project to suitability data so you can crop
pvout_3005 <- project(pvout, suitability_scaled_w0, method = "bilinear")

#crop and mask
pvout_crop <- crop(pvout_3005, suitability_scaled_w0)
pvout_mask <- mask(pvout_crop, suitability_scaled_w0)

writeRaster(pvout_mask, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/pvout_masked.tif")
pvout_mask <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/pvout_masked.tif")

#keep only suitability classes 3-5
suit_35 <- suitability_scaled_w0
suit_35[suit_35 < 3] <- NA
writeRaster(suit_35, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_3to5.tif")
suit_35 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_3to5.tif")

#extract pvout by suitability class using zonal stats
pvout_by_class <- zonal(pvout_mask, suit_35, fun = "mean", na.rm = TRUE) 
#expected average energy yield per class
#3 = 1146.417
#4 = 1309.010
#5 = 1296.912

#workflow to identify patches in classes 4 and 5
#isolate each class; now everything other than the cls is NA
suit_cl4 <- suit_35
suit_cl4[suit_35 != 4] <- NA 
writeRaster(suit_cl4, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_cl4.tif")
suit_cl4 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_cl4.tif")

suit_cl5 <- suit_35
suit_cl5[suit_35 != 5] <- NA 
writeRaster(suit_cl5, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_cl5.tif")
suit_cl5 <- rast("C:/MGEM/FCOR599/geodatabase/project_working.gdb/suitability_cl5.tif")

#convert 5 acres (min footprint) to cells
#1 cell = 900m2
#5 acres = 20,234m2
#threshold = 20234/900 = 23 cells

#load in class 5 polygons
cl5_mmu_poly <- sf::st_read("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cl5_poly_mmu.shp")
#add acres area column 
cl5_mmu_poly$acres <- cl5_mmu_poly$Area / 4047

#load in class 4 polygons
cl4_poly <- sf::st_read("C:/MGEM/FCOR599/geodatabase/project_working.gdb/cl4_polygonized.shp")
#add area
cl4_polys <- cl4_poly %>%
  mutate(Area = sf::st_area(cl4_poly))
#add acres area column
cl4_polys$acres <- cl4_polys$Area / 4047 
#keep only polygons bigger or equal to 5 acreas
cl4_mmu_poly <- cl4_polys %>%
  filter(DN == 4) %>%
  mutate(acres = as.numeric(st_area(cl4_polys)) / 4047) %>% #as.numeric to strip units
  filter(acres >= 5)
plot(cl4_mmu_poly)

sf::st_write(cl4_mmu_poly, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cl4_poly_mmu.shp")

#read in cleaned data 
sum(cl4_mmu_poly$Area) / 1e+6 # = 28041.37km2 of contiguous suitable land
sum(cl5_mmu_poly$Area) / 1e+6 # = 129.115km2 of contiguous very suitable land

#calculate average PVOUT per polygon by masking PVOUT to polygon layer
#make spatvectors 
cl4_mmu_vect <- vect(cl4_mmu_poly)
cl5_mmu_vect <- vect(cl5_mmu_poly)

#mask pvout using polygons
cl4_pvout <- mask(pvout_mask, cl4_mmu_vect) 
writeRaster(cl4_pvout, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cl4_pvout.tif")

#repeat for class 5
cl5_pvout <- mask(pvout_mask, cl5_mmu_vect)
writeRaster(cl5_pvout, "C:/MGEM/FCOR599/geodatabase/project_working.gdb/cl5_pvout.tif")

#count polygons above 10 acres (2MW)
sum(cl4_mmu_poly$acres > 10, na.rm = TRUE) #increasing mmu to 10 acres = 39206 polygons (from 65,102)
sum(cl5_mmu_poly$acres > 10, na.rm = TRUE) #increasing mmu to 10 acres = 760 polygons (from 1,656)

#STEP 7########################################
#Plotting Results ----

#pie chart to show area in each suitability class
land_div <- c(0.0002281924,0.0320099254, 0.1319460980, 0.0710038039, 0.0015879564, 0.7632240282)
sum(land_div)

# Class labels (simple names for the slices)
class_labels <- c("Very Suitable",
                  "Suitable",
                  "Moderately Suitable",
                  "Less Suitable",
                  "Least Suitable",
                  "Restricted")

# Area labels for the legend
#area_labels <- c("216.45 km²",
#                 "30362.67 km²",
 #                "125156.06 km²",
  #               "67349.89 km²",
   #              "1506.24 km²",
    #             "723947.99 km²")

# Assign class labels as the names for pie wedge labels
#names(land_div) <- area_labels

# Create pie chart
pie(land_div,
    col = c("darkgreen", "green", "yellow", "orange", "red", "grey"),
    main = "BC's Land Base by Suitability Class",
    cex = 0.4)

# Add legend with area values only
legend(
  "topleft",
  legend = class_labels,
  fill = c("darkgreen", "green", "yellow", "orange", "red", "grey"),
  cex = 0.6,        # text + box scaling
  pt.cex = 0.6,     # box size
  y.intersp = 1,  # vertical spacing
  x.intersp = 0.7,  # spacing between box and text
  inset = 0.02)


#multi bar chart; suitability by ecoprovince
# Ecoprovinces
ecoprovinces <- c(rep("Southern Interior" , 5),
                  rep("Southern Interior Mountains" , 5),
                  rep("Central Interior" , 5),
                  rep("Coast and Mountains" , 5),
                  rep("Georgia Depression", 5),
                  rep("Sub-Boreal Interior", 5),
                  rep("Boreal Plains", 5),
                  rep("Northern Boreal Mountains", 5),
                  rep("Taiga Plains", 5),
                  rep("Southern Alaska Mountains", 5))

# Suitability classes
class <- rep(c("Very Suitable", "Suitable", "Moderately Suitable",
               "Less Suitable", "Least Suitable"), 10)

df <- data.frame(Ecoprovince = ecoprovinces,
                 Suitability_Class = class,
                 Percent = NA)

# Plot
ggplot(df, aes(x = Ecoprovince,
               y = Percent,
               fill = Suitability_Class)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Very Suitable" = "darkgreen",
                               "Suitable" = "green",
                               "Moderately Suitable" = "yellow",
                               "Less Suitable" = "orange",
                               "Least Suitable" = "red")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Ecoprovince",
       y = "Percent",
       fill = "Suitability Class",
       title = "Suitability Class Proportions by Ecoprovince") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))