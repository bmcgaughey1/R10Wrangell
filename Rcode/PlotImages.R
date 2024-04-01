# code to work with Wrangell Island orthos
#
# images are in AK state plane
# Jacob's sample plot locations are NAD83 UTM zone 8
#
library(terra)
library(mapview)
library(leafem)
library(leaflet)
library(tidyverse)

options(scipen = 10E6)

folder <- "C:/Users/bmcgaughey/Desktop/Wrangell/"

# Base map--qquads with plots, roads, and access area ---------------------
pts <- vect(paste0(folder, "wrangell_grid_sample_panel_20240315.shp"))
crs(pts)

# load quads needed for area...only qquads that have sample points
qquads <- vect(paste0(folder, "QQuads.shp"))

pts <- project(pts, crs("epsg:26908"))
qquads <- project(qquads, crs("epsg:26908"))

qq <- qquads[c(4, 1, 6, 5, 2, 10, 11, 7, 3, 13, 12, 9, 8, 16, 17, 14, 15), ]
qq$seq <- c(1:17)

# load roads and accessible area
roads <- vect(paste0(folder, "Roads.shp"))
access <- vect(paste0(folder, "AccessibleArea.shp"))
roads <- project(roads, crs("epsg:26908"))
access <- project(access, crs("epsg:26908"))

# map showing qquads with numeric labels
m <- mapview(qq, col.regions = "gray", map.types = c("OpenTopoMap"), alpha.regions = 0.4)
r <- mapview(roads, color = "black")
a <- mapview(access, col.regions = "cyan", alpha.regions = 0.2)
p <- mapview(pts, color = "red", col.regions = "red", cex = 2)
t <- m + r + a + p

# compute center of area in lat-lon
e <- ext(qq)
e <- project(e, crs(qq), crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
cntr_x <- (e$xmin[[1]] + e$xmax[[1]]) / 2
cntr_y <- (e$ymin[[1]] + e$ymax[[1]]) / 2

m <- addStaticLabels(t, data = sf::st_as_sf(qq),
                label = qq$seq,
                noHide = TRUE,
                group = "quadNumbers",
                direction = 'top',
                offset = c(0, 25),
                textOnly = TRUE,
                textsize = "20px",
                style = list("background" = "black", "color" = "white", "font-weight" = "bold"))

# set zoom level and display
m %>% setView(cntr_x, cntr_y, zoom = 13)

# grab image
mapshot(m, url = "testmap.html")

# Individual quads with plots ---------------------------------------------
i <- 7
for (i in 1:nrow(qq)) {
  # img <- rast(paste0(folder, "Ortho/", qq$NAME[i]))

  e <- ext(qq[i])
  e <- project(e, crs(qq), crs("epsg:26908"))

  # intersect layers
  ipts <- terra::intersect(pts, e)
  iroads <- terra::intersect(roads, e)
  iaccess <- terra::intersect(access, e)

  t <- mapview(qq[i], col.regions = "gray", alpha.regions = 0.1)
  if (nrow(iroads)) t <- t + mapview(iroads, color = "black")
  if (nrow(iaccess)) t <- t + mapview(iaccess, col.regions = "cyan", alpha.regions = 0.1)
  if (nrow(ipts)) t <- t + mapview(ipts, color = "red", col.regions = "red", cex = 2)

  addStaticLabels(t, data = sf::st_as_sf(qq[i]),
                  label = qq$seq[i],
                  noHide = TRUE,
                  group = "quadNumber",
                  direction = 'top',
                  offset = c(0, 25),
                  textOnly = TRUE,
                  textsize = "20px",
                  style = list("color" = "white", "font-weight" = "bold"))

  addStaticLabels(t, data = sf::st_as_sf(ipts),
                  label = ipts$id,
                  noHide = TRUE,
                  group = "plotNumber",
                  direction = 'top',
                  offset = c(3, 10),
                  textOnly = TRUE,
                  textsize = "10px",
                  style = list("color" = "black", "font-weight" = "bold"))
}

mapShot

# load a tile
img <- rast(paste0(folder, "Ortho/m_5613217_SE_3_06_30_20200730.tif"))
crs(img)

# project image to NAD83-UTM Z8
imgUTM <- project(img, crs("epsg:26908"))



# project points to match images
if (F) {
  ipts <- project(pts, img)
  img <- project(img, pts)
  ipts <- pts
  crs(ipts)

  plotRGB(img)
  points(ipts, col = "red")
}

# remove duplicated plots and sort
plotFlags <- duplicated(pts$id)
plots <- pts[!plotFlags]
plots <- plots[order(plots$id)]

# build buffers around plots for image clip
bufferSize <- 1000
plotsBuf <- buffer(plots, bufferSize / 2, capstyle = "square")

# build buffer around plot to show plot footprint
plotRadius <- 30.4035 / 2
plotsFootprint <- buffer(plots, plotRadius)

# work through the points
# build a bounding box for the image area around the plot
# plot the image with the bounding extent
# add the plot center and plot footprint
# add roads
# add legend info...plot number, panel, location, north arrow, scale bar
i <- 1
for (i in 1:nrow(plots)) {
  cat("Plot: ", plots$id[i], "\n")

  plotRGB(imgUTM, ext = ext(plotsBuf[i]), axes = T, grid = T, mar = c(7, 2, 3, 2), pax = list(side=c(1:4), col = "gray"))
  polys(plotsFootprint, col = "gray", alpha = 0.4)
  points(plots[i], col = "red")
  text(plots[i]$X, plots[i]$Y, plots[i]$id, col = "yellow", pos = 3)

  bx <- (ext(plotsBuf[i])$xmin[[1]] + ext(plotsBuf[i])$xmax[[1]]) / 2
  by <- ext(plotsBuf[i])$ymax[[1]] + 100
  text(bx, by, "Wrangell Island Lidar Modeling Project", pos = 1, xpd = NA)

  by <- ext(plotsBuf[i])$ymin[[1]] - 75
  text(bx, by, paste0("Plot: ", plots[i]$id), pos = 1, xpd = NA)
  by <- by - 35
  text(bx, by, paste0("   NAD83 UTM zone 8 (EPSG: 26908): (", round(plots[i]$X, 0), ", ", round(plots[i]$Y, 0), ")"), pos = 1, xpd = NA)
}


as.list(ext(plotsBuf[i]))
plotsBuf[i]$NAME
ext(plotsBuf[i])$ymin[[1]]




if (F) {
# test topo maps
# downloaded on-demand topos from USGS. They have no crs and extent is wonky
topo <- rast(paste0(folder, "Topo/", "AK_PetersburgB2SE_20240323_233040755000_TM_geo.tif"))
summary(topo)
plotRGB(topo)
crs(topo)
ext(topo)
}
