# code for Wrangell Island lidar sampling project
#
# data are in Box. Paths to local copies:
#
# FUSION metrics:
# C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy/Fusion_Gridmetrics/Metrics_30METERS
#
# Access layers:
# C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy.gdb
#
library(terra)

compareMetric <- function(
    metric,
    metricType = 0, # 0 = elevation (all returns), 1 = elevation (1st returns), 2 = intensity (all returns), 3 = intensity(1st returns), 4 = explicit metric file
    verbose = TRUE
  )
{
  baseFolder <- "C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy/Fusion_Gridmetrics/Metrics_30METERS"
  # build metric file name
  if (metricType == 0)
    metricFile <- paste0(baseFolder, "/elev_", metric, "_2plus_30METERS.asc")
  else if (metricType == 1)
    metricFile <- paste0(baseFolder, "/FIRST_RETURNS_elev_", metric, "_2plus_30METERS.asc")
  else if (metricType == 2)
    metricFile <- paste0(baseFolder, "/int_", metric, "_2plus_30METERS.asc")
  else if (metricType == 3)
    metricFile <- paste0(baseFolder, "/FIRST_RETURNS_int_", metric, "_2plus_30METERS.asc")
  else if (metricType == 4)
    metricFile <- metric
  else
    stop("Invalid metricType")


}

epsg <- 26908

metrics <- c("P30", "P50", "P95", "P99", "profile_area")

P95file <- "C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy/Fusion_Gridmetrics/Metrics_30METERS/elev_P95_2plus_30METERS.asc"

# open geodatabase and read access layer without beach access
# ***** this isn't working...could be out-of-date terra package
# data are in EPSG:4326 WGS-84 lat-lon
#gdb <- "C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy/Wrangell Access Lidar Plots.gdb"
#access <- vect(gdb, layer = "WGL_SurveyArea_Road_Corridors")

# read from local shapefile in EPSG:4326 WGS-84 lat-lon
access <- vect("extras/WGL_SurveyArea_Road_Corridors.shp")

# reproject to UTM 8N
access <- project(access, "epsg:26908")

# read raster data...projection file isn't complete so force the UTM 8N
P95 <- rast(P95file)
crs(P95) <- crs("epsg:26908")

plot(P95)
plot(access, add = TRUE)

# buffer 30m metric layer to create island perimeter
islandPoly <- buffer(P95, 1)
plot(islandPoly)

# intersect access area with raster
P95access <- crop(P95, access, mask = TRUE)

plot(P95access)

hist(P95, main = "All Area", maxcell = nrow(P95) * ncol(P95), breaks = 50)
hist(P95access, main = "Road Access", maxcell = nrow(P95) * ncol(P95), breaks = 50)

# summary stats
global(P95, fun = "mean", na.rm = TRUE)
global(P95access, fun = "mean", na.rm = TRUE)

crs(P95)
crs(access)
