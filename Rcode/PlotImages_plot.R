# code to work with Wrangell Island plot locations to produce map books
#
# I tried using mapview to do this and found it impossible. You have little control
# over labels in mapview/leaflet. I could get one set of labels (labels on one layer)
# but not multiple labels. Zoom control is also problematic. I think you could produce
# html files for the mapbooks that would work but I didn't try this (other than some
# experimentation using mapshot to save to html). I also had trouble saving images
# with mapshot (layers were not rendered). I have had this problem before and not
# found a solution.
#
# I started with mapview because I wanted to show contours and topography. Mapview
# seemed like the easiest way to get this result.
#
# I initially downloaded topo maps from USGS and tried using these as the background
# for plots...didn't work (georeferencing in downloaded maps was missing and maps
# had other problems).
#
# The initial code is in PlotImages.R but this file isn't good for much except showing
# the wrong way to produce the plot images.
#
# In the end, I found the basemaps package that lets you download good topo
# layers to use as a basemap. Code uses terra (mostly) but there are a few
# functions that still require simple features objects. For these, I converted
# things on the fly.
#
library(terra)
#library(OpenStreetMap)
#library(tidyterra)
#library(ggplot2)
library(basemaps)

options(scipen = 10E6)

localcrs <- "EPSG:26908" # UTM 8N NAD83

# base folder for data
folder <- "C:/Users/bmcgaughey/Desktop/Wrangell/"

# read 50m grid used for plots. plotRGB() with grid = TRUE didn't always work.
grid <- vect(paste0(folder, "grid50.shp"))

# create collection of ortho images...these came from R10. They are 4-band images
# so you can also produce false-color IR renderings plotRGB(x, r = 4, g = 3, b = 2)
images <- list.files(paste0(folder, "Ortho/"), pattern = "tif")
images <- paste0(folder, "Ortho/", images)
imgCollection <- sprc(images)

# Base map--index tiles with plots, roads, and access area ---------------------
pts <- vect(paste0(folder, "wrangell_grid_sample_panel_20240315.shp"))

# assign priority to plots...first 100 are priority 1, next 14 are priority 2
# last 14 are priority 3
# We shoujld have included the priority in the data...as it is, this has to be
# assigned "manually"
pts$priority <- 1
pts$priority[101:114] <- 2
pts$priority[115:128] <- 3

# assign point colors based on priority
ptcol <- c("red", "green", "cyan")
pts$color <- ptcol[pts$priority]

# sort by id
pts <- pts[order(pts$id), ]

# load index tiles for area
indexTiles <- vect(paste0(folder, "IndexTiles.shp"))

# shouldn't be needed...both items are already in UTN8N
pts <- project(pts, crs("epsg:26908"))
indexTiles <- project(indexTiles, crs("epsg:26908"))

# only need tiles containing points
t <- relate(indexTiles, pts, "contains", na.rm = F)
t <- rowSums(t)
indexTiles <- indexTiles[t > 0, ]

# assign sequential number...starts at bottom left index tile
indexTiles$seq <- seq(1:length(indexTiles))

# load roads and accessible area and project to UTM 8N
roads <- vect(paste0(folder, "Roads.shp"))
access <- vect(paste0(folder, "AccessibleArea.shp"))
roads <- project(roads, localcrs)
access <- project(access, localcrs)

# get base map layer and project to UTM 8N...want this to extend beyond the index
# tiles
basemaps::set_defaults(map_service = "osm", map_type = "topographic")
indexTileswm <- project(indexTiles, crs("EPSG:3857"))
eindexTileswm <- ext(indexTileswm) + 800
Base_basemaps <-  basemaps::basemap_terra(ext=sf::st_bbox(as.vector(eindexTileswm), crs = "EPSG:3857"), verbose=FALSE)
#Base_basemaps <-  basemaps::basemap_terra(ext=sf::st_as_sf(project(indexTiles, crs("EPSG:3857"))), verbose=FALSE)
Base_basemaps <- project(Base_basemaps, localcrs)

pdf(file = paste0(folder, "MapBook.pdf"))

# overview plot...tried to add road labels but there are no control options in terra for labeling along
# linear features so the result is very messy and useless.
plotRGB(Base_basemaps, ext = ext(Base_basemaps), axes = T, grid = T, mar = c(2, 2, 2, 2), pax = list(side=c(1:4), col = "gray"))
polys(indexTiles, col = "gray", alpha = 0.4)
polys(access, col = "cyan", border = "darkgray", alpha = 0.2)
lines(roads, col = "black", lwd = 2)
#polys(plotsFootprint, col = "gray", alpha = 0.4)
points(pts, col = pts$color)
text(indexTiles, indexTiles$seq, col = "yellow", pos = 3, cex = 2.0, offset = c(-0.5, -0), halo = T, hc = "black")

# title block and legend
rect(xleft = 670000, xright = 687800, ybottom = 6255000, ytop = 6263000, col = "white", border = "black")
rect(xleft = 682000, xright = 687800, ybottom = 6255000, ytop = 6263000, col = "white", border = "black")
legend("topright", c("1 (100 plots)", "2 (14 plots)", "3 (14 plots)"), inset = c(0.12, 0.05), bg = NULL, box.lty = 0, horiz = F, title = "Plot Priority", col = ptcol, pch = 16, cex = 0.8)
sbar(10000, xy = c(670700, 6255800), divs = 4, ticks = T, type = "bar", below = "meters", cex = 0.8)
#north(xy = c(680500, 6260500), type = 1, d = 2000)
text(676000, 6260000, "Wrangell Island", pos = 3, xpd = T, cex = 1.2, font = 2)
text(676000, 6258500, "Lidar Plots", pos = 3, xpd = T, cex = 1.2, font = 2)
text(675900, 6257000, "Coordinates in NAD83 UTM 8N", pos = 3, xpd = T, cex = 0.8)

dev.off()

# Individual quads with plots ---------------------------------------------
# intersect plots with indexTiles
plotList <- terra::intersect(pts, indexTiles)

# build buffer around plot to show plot footprint
plotRadius <- 9.2671
plotsFootprint <- buffer(plotList, plotRadius)

# size for image chips and area covered on individual plot pages
bufferSize <- 200

doPlots <- T

i <- 1
for (i in 1:nrow(indexTiles)) {
  pdf(file = paste0(folder, "MapBook_", indexTiles$seq[i], ".pdf"))

  # get base map layer and project to UTM 8N
  basemaps::set_defaults(map_service = "osm", map_type = "topographic")
  indexTileswm <- project(indexTiles[i], crs("EPSG:3857"))
  eindexTileswm <- ext(indexTileswm) + 800
  Base_basemaps <-  basemaps::basemap_terra(ext=sf::st_bbox(as.vector(eindexTileswm), crs = "EPSG:3857"), verbose=FALSE)
  #Base_basemaps <-  basemaps::basemap_terra(ext=sf::st_as_sf(project(indexTiles[i], crs("EPSG:3857"))), verbose=FALSE)
  Base_basemaps <- project(Base_basemaps, localcrs)

  e <- ext(indexTiles[i])
  e <- project(e, crs(indexTiles), crs("epsg:26908"))

  me <- project(eindexTileswm, crs("EPSG:3857"), crs("epsg:26908"))

  # intersect layers
  ipts <- terra::intersect(pts, indexTiles[i])
  iplotList <- terra::intersect(plotList, indexTiles[i])
  iroads <- terra::intersect(roads, indexTiles[i])
  iaccess <- terra::intersect(access, indexTiles[i])
  iplotsFootprint <- terra::intersect(plotsFootprint, indexTiles[i])

  # only need plots on this quad
  #iplotList <- iplotList[iplotList$NAME == indexTiles$NAME[i], ]
  #iplotsFootprint <- iplotsFootprint[iplotsFootprint$NAME == indexTiles$NAME[i], ]

  if (nrow(ipts)) {
    plotRGB(Base_basemaps, axes = T, grid = F, mar = c(5, 2, 2, 2), pax = list(side=c(1:4), col = "gray"))
    lines(indexTiles[i], lwd = 2)
    #polys(indexTiles[i], col = "gray", alpha = 0.4)
    if (nrow(iaccess)) polys(iaccess, col = "cyan", border = "black", alpha = 0.05)
    if (nrow(iroads)) lines(iroads, col = "black", lwd = 2)
    # polys(plotsFootprint, col = "red", border = "darkgray", alpha = 0.2)
    points(ipts, col = ipts$color)
    text(indexTiles[i], labels = "seq", col = "yellow", pos = 3, cex = 2.0, offset = c(-0.5, -0), halo = T, hc = "black")
    text(ipts, "id", col = "black", pos = 4, offset = c(0.2, 0), halo = T, hc = "white")

    # title block and legend
    rect(xleft = me$xmin[[1]], xright = me$xmax[[1]], ybottom = me$ymin[[1]] - 1500, ytop = me$ymin[[1]] - 500, col = "white", border = "black", xpd = T)

    legend("bottomright", c("1", "2", "3"), inset = c(0.13, -0.17), bg = NULL, box.lty = 0, title = "Plot Priority", col = ptcol, pch = 16, xpd = T, horiz = T)
    sbar(2000, xy = c(me$xmin[[1]] + 2800, me$ymin[[1]] - 1200), divs = 4, ticks = T, type = "bar", below = "meters", cex = 0.8)
    #north(xy = c(680500, 6260500), type = 1, d = 2000)
    text(me$xmin[[1]] + 50, me$ymin[[1]] - 800, "Wrangell Island", pos = 4, xpd = T, cex = 1., font = 2)
    text(me$xmin[[1]] + 50, me$ymin[[1]] - 1100, "Lidar Plots", pos = 4, xpd = T, cex = 1., font = 2)

    # sort list by priority and id
    iplotList <- iplotList[order(iplotList$priority, iplotList$id), ]
    iplotsFootprint <- iplotsFootprint[order(iplotsFootprint$priority, iplotsFootprint$id), ]

    plotsBuf <- terra::buffer(iplotList, bufferSize / 2, capstyle = "square")

    rm(Base_basemaps)

    # run through plots
    j <- 1
    if (doPlots) {
      for (j in 1:nrow(iplotList)) {
        # load image tile
        #cat("loading: ", iplotList$NAME[j], "...\n")

        #img <- rast(paste0(folder, "Ortho/", iplotList$NAME[j]))

        # crop
        #imgCrop <- terra::crop(img, project(ext(plotsBuf[j]), crs(plotsBuf), crs(img)))
        imgCrop <- terra::crop(imgCollection, project(ext(plotsBuf[j]), crs(plotsBuf), crs("EPSG:6394")))
        imgMosaic <- merge(imgCrop)
        # mosaic was producing bad clips for some images. Not sure why but merge() is supposed to be faster anyway
        # and all the clips were good.
        #imgMosaic <- mosaic(imgCrop, fun = "max")

        # project cropped image to UTM 8N
        imgMosaic <- project(imgMosaic, crs("EPSG:26908"))

        # intersect plot center with cropped image
        iptCenter <- terra::intersect(iplotList[j], ext(imgMosaic))

        if (length(iptCenter)) {
          plotRGB(imgMosaic, axes = T, grid = F, mar = c(5, 2, 2, 2), pax = list(side=c(1:4), col = "gray"))
          lines(grid, col = "gray")
          polys(iplotsFootprint[j], col = NULL, border = "white", alpha = 0.2)
          points(iplotList[j], col = iplotList$color[j], cex = 1.2)
          if (nrow(iroads)) lines(iroads, col = "black", lwd = 2)
          #text(iplotList[j], labels = "id", col = "black", pos = 3, adj = c(0.1, 0.5), halo = T, hc = "yellow")
          text(iplotList$X[j], iplotList$Y[j] + 8, iplotList$id[j], col = "yellow", pos = 3, cex = 1.4)

          sbar(100, "bottom", divs = 4, ticks = T, type = "bar", below = "meters", cex = 0.9, col = "white", halo = T)

          bx <- (ext(plotsBuf[j])$xmin[[1]] + ext(plotsBuf[j])$xmax[[1]]) / 2
          #by <- ext(plotsBuf[j])$ymax[[1]] + 150
          #text(bx, by, "Wrangell Island Lidar Modeling Project", pos = 1, xpd = NA)

          by <- ext(plotsBuf[j])$ymin[[1]] - 15
          text(bx, by, paste0("Plot: ", iplotList$id[j], "   Priority: ", iplotList$priority[j]), pos = 1, xpd = T, font = 2)
          by <- by - 10
          text(bx, by, paste0("   NAD83 UTM 8N (EPSG: 26908): (", round(iplotList$X[j], 0), ", ", round(iplotList$Y[j], 0), ")"), pos = 1, xpd = T, font = 2)
        }
      }
    }
  }
  dev.off()
}




# make a set of index tiles
if (F) {
  library(sf)

  t <- st_make_grid(sf::st_as_sf(pts), c(6000, 8000), offset = c(663000, 6230000), crs = crs("EPSG:26908"))
  plot(t)
  st_write(t, paste0(folder, "IndexTiles.shp"), append = F)
}

# make a 200m grid to plot over image clips. grid = TRUE on plotRGB() failed often
if (F) {
  library(sf)

  t <- st_make_grid(sf::st_as_sf(indexTiles), 200, offset = c(658000, 6230000), crs = crs("EPSG:26908"))
  plot(t)
  st_write(t, paste0(folder, "grid200.shp"), append = F)
}

# make a 100m grid to plot over image clips. grid = TRUE on plotRGB() failed often
if (F) {
  library(sf)

  t <- st_make_grid(sf::st_as_sf(indexTiles), 100, offset = c(658000, 6230000), crs = crs("EPSG:26908"))
  plot(t)
  st_write(t, paste0(folder, "grid100.shp"), append = F)
}

# make a 50m grid to plot over image clips. grid = TRUE on plotRGB() failed often
if (F) {
  library(sf)

  t <- st_make_grid(sf::st_as_sf(indexTiles), 50, offset = c(658000, 6230000), crs = crs("EPSG:26908"))
  plot(t)
  st_write(t, paste0(folder, "grid50.shp"), append = F)
}

