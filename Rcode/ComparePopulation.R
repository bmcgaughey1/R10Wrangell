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
library(ggplot2)

compareMetric <- function(
    metric,
    metricType = 0, # 0 = elevation (all returns), 1 = elevation (1st returns), 2 = intensity (all returns), 3 = intensity (1st returns), 4 = explicit metric file
    baseFolder = "C:/Users/bmcgaughey/Box/VMARS-TL/VMARS/Projects/2024_R10_LidarStrategy/Fusion_Gridmetrics/Metrics_30METERS",
    accessFile = "extras/WGL_SurveyArea_Road_Corridors.shp",
    verbose = TRUE
  )
{
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
    metricFile <- paste0(baseFolder, "/", metric)
  else
    stop("Invalid metricType")

  # read access layer and project
  access <- vect(accessFile)
  access <- project(access, "epsg:26908")

  # read metric layer and assign projection
  if (!file.exists(metricFile))
    stop("invalid metric file: ", metricFile)

  metric <- rast(metricFile)
  crs(metric) <- crs("epsg:26908")

  # intersect access area with raster
  metricCrop <- crop(metric, access, mask = TRUE)

  # compute metrics over entire area
  metricStats <- data.frame(
    mean = round((global(metric, fun = "mean", na.rm = TRUE))$mean, 2),
    std = round((global(metric, fun = "std", na.rm = TRUE))$std, 2),
    min = round((global(metric, fun = "min", na.rm = TRUE))$min, 2),
    max = round((global(metric, fun = "max", na.rm = TRUE))$max, 2),
    row.names = "All Area"
  )

  # compute metrics within access area
  metricCropStats <- data.frame(
    mean = round((global(metricCrop, fun = "mean", na.rm = TRUE))$mean, 2),
    std = round((global(metricCrop, fun = "std", na.rm = TRUE))$std, 2),
    min = round((global(metricCrop, fun = "min", na.rm = TRUE))$min, 2),
    max = round((global(metricCrop, fun = "max", na.rm = TRUE))$max, 2),
    row.names = "Access Area"
  )

  metricHist <- hist(metric, main = "All Area", maxcell = nrow(metric) * ncol(metric), breaks = 20)
  metricCropHist <- hist(metricCrop, main = "Access Area", maxcell = nrow(metricCrop) * ncol(metricCrop), breaks = 20)

  return(list(metricStats, metricCropStats, metricHist, metricCropHist))
}

# metric types...second item in list
# 0 = elevation (all returns), 1 = elevation (1st returns), 2 = intensity (all returns), 3 = intensity (1st returns), 4 = explicit metric file
metrics <- list(
  list("P30", 0),
  list("P50", 0),
  list("P95", 0),
  list("P99", 0),
  list("P95", 3),
  list("P95", 2),
  list("profile_area_30METERS.asc", 4)
)

mTypes = c(
  "elevation_all_returns",
  "elevation_1st_returns",
  "intensity_all_returns",
  "intensity_1st_returns",
  ""
)

for (i in 1:length(metrics)) {
  cat("working on ", metrics[[i]][[1]], "...\n")
  t <- compareMetric(
      metrics[[i]][[1]], metrics[[i]][[2]]
  )

  pdf(file = paste0("extras/", metrics[[i]][[1]], "_", mTypes[metrics[[i]][[2]] + 1], "_.pdf"), width = 10, height = 7.5)
  c1 <- capture.output(print(t[[1]]))
  c2 <- capture.output(print(t[[2]]))

  par(mfrow = c(1, 2))
  plot(t[[3]], main = "All area")
  text(t[[3]]$breaks[8], max(t[[3]]$counts) * 0.9, c1[1])
  text(t[[3]]$breaks[8], max(t[[3]]$counts) * 0.85, c1[2])

  plot(t[[4]], main = "Access area")
  text(t[[4]]$breaks[8], max(t[[4]]$counts) * 0.9, c2[1])
  text(t[[4]]$breaks[8], max(t[[4]]$counts) * 0.85, c2[2])
  par(mfrow = c(1, 1))
  dev.off()
}





# testing code...don't need -----------------------------------------------
epsg <- 26908

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

p <- ggplot(data = P95, aes)
# summary stats
tm <- global(P95, fun = "mean", na.rm = TRUE)
global(P95access, fun = "mean", na.rm = TRUE)

crs(P95)
crs(access)
