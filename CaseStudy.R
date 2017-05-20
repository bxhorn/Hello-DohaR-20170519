# Header ----
#
# Name:         CaseStudy.R
# Title:        Case study for DohaR meeting
# Version:      1.0
# Date:         2017-May-19
# Author:       Brad Horn
#               bxhorn@gmail.com
#
# Description: A script to create a map of the State of Qatar that includes:
#                   - administrative boundaries and names
#                   - 6 potential development sites for solar PV installation
#                   - A pixel grid for Meteosat-10 satellite data collection
#                   - 5km and 10km buffer zones around each project to identify the
#                     satellite pixel points closest to each project
#
#              A simple extraction routine then pulls the pixel points inside the
#              buffer zones.
#
#              Meteosat pixel locations represents 250 million points on the Earth
#              where the Meteosat-10 satellite collects data daily.  The large data
#              files with pixel locations can be manually downloaded from SAF servers
#              as HDF5 files, one for latitude and one for longitude data.  The binary
#              files are read into R using the GDAL tool set.  The seperate lon/lat
#              data is then combined into a large SpatialPoint data object.  These
#              pixel points are overlayed on vector shapefile map layers for the
#              State of Qatar.  One shape file contains SpatialLines for the the coastal
#              boundary of Qatar.  A second SpatialLines shapefile includes the national
#              boundary, and a third shapefile has SpatialPolygons for the administrative
#              boundaries in Qatar.  A SpatialPoints vector object is created within
#              code to define the 6 potential project sites.  The buffer zones around
#              the projects are created easily using a standard function from the rgeos
#              package in R.
#
#              A custom function is also developed to assess the spatial resolution
#              of the projected satellite grid, recognizing that the resolution on the
#              Earth's surface changes by latitude given the curvature of the Earth.
#
# Details:     Pixel data: https://landsaf.meteo.pt/auxiliarDataFiles.jsp
#              Data file(s): 1. HDF5_LSASAF_MSG_LAT_NAfr_4bytesPrecision
#                            2. HDF5_LSASAF_MSG_LON_NAfr_4bytesPrecision
#
#              GIS shore data: http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html
#              Data file(s): 1. gshhs_f.b
#
#              GIS country polygons: CIA World Data Bank II
#              Data file(s): 1. WDBII_border_f_L1
#
#              GIS admin boundaries Qatar: http://www.gadm.org
#              Data file(s): 1. QAT_adm1
#
# Dev Notes:   Users should maximize the plot window in RStudio to cover the entire
#              right half of the RStudio workspace.  This will avoid the chance of
#              encountering a plot error that reads "graph device too snmall for
#              graph."
#
# Depends:     A second script (Configure.R) is required to configure the users R
#              workspace environment to match the environment used by the author,
#              Source the configure script before sourcing the case study script.
#
#              The configure file lists all R packages that the user should install
#              for geospatial analysis.
#
#              Several input data files are also required and can be downloaded from
#              https://github.com/bxhorn/Hello-DohaR-20170519
#
# References:  LSA-SAF Product User Manual (DSSF) on Meteosat-10 data files
#              EUMETSAT MFG/MSG User Handbooks on Meteosat-10 data
##----------------------------------------------------------------------------------------##

# 0.Initialize Code
# Load librariers and DLLs
library(sp, quietly = TRUE)
library(rgdal, quietly = TRUE)
library(maptools, quietly = TRUE)

# paths - update to match the location where dependent data files are stored
data.path <- "/home/bxhorn/Documents/DohaR/data/"

# map projection and graph devices
map.proj <- CRS("+proj=longlat +ellps=WGS84")
op <<- par()
pdf.options(family = "Helvetica")
##----------------------------------------------------------------------------------------##

# 1.Define spatial points for 6 potential development sites
# site metadata
x <- c(51.25, 51.40, 51.25, 50.80, 50.83, 50.80)
y <- c(26.00, 25.95, 25.75, 25.30, 25.31, 25.20)
coords <- cbind(x, y)
site.names <- data.frame(site = LETTERS[1:6])
# go spatial
spdf.sites <-SpatialPointsDataFrame(coords, site.names)
##----------------------------------------------------------------------------------------##

# 2.Load Qatar map data
qatar.bbox <- matrix(c(50.7, 51.65, 24.4, 26.4), ncol = 2, byrow = TRUE,
                     dimnames = list(c("x", "y"), c("min", "max")))
# GSHHS Shores Region Map (SpatialPolygons)
qatar.shores <- Rgshhs(paste0(data.path, "gshhs_f.b"), xlim = c(50.65, 51.7), ylim = c(24.4, 26.4))
q.shoreGSHHS <- qatar.shores$SP
bbox(q.shoreGSHHS)
# NOAA Borders (SpatialLinesDataFrame)
WDBIIinfo <- ogrInfo(data.path, "WDBII_border_f_L1")
WDBII_b_L1 <- readOGR(data.path, "WDBII_border_f_L1")
q.borderNOAA <- pruneMap(WDBII_b_L1, xlim = c(50.65, 51.7), ylim = c(24.4, 26.4))

# GADM Amin Boundries
GADMinfo <- ogrInfo(data.path, "QAT_adm1")
q.adminGADM <- readOGR(data.path, "QAT_adm1")
# Convert to SpatialPolygonDataFrame to SpatialLineDataFrame
q.adminGADM <- as(q.adminGADM, "SpatialLinesDataFrame")
##----------------------------------------------------------------------------------------##

# 3.Load LSA-SAF pixels
# load pixel coordinates and geographical coordinates
lat.mena <- readGDAL(paste0(data.path, "HDF5_LSASAF_MSG_LAT_NAfr_4bytesPrecision"))
lon.mena <- readGDAL(paste0(data.path, "HDF5_LSASAF_MSG_LON_NAfr_4bytesPrecision"))
# clean-up missing data
is.na(lat.mena@data$band1) <- lat.mena@data$band1 == 900000
is.na(lon.mena@data$band1) <- lon.mena@data$band1 == 900000
# define spatial points for satellite pixels over Qatar land mass
lon.sp <- lon.mena@data$band1/10000
lat.sp <- lat.mena@data$band1/10000
mat.sp <- cbind(lon.sp, lat.sp)
row.names(mat.sp) <- 1:nrow(mat.sp)
sp.pixels <- SpatialPoints(na.omit(mat.sp), bbox = qatar.bbox, proj4string = map.proj)
bbox(sp.pixels)
##----------------------------------------------------------------------------------------##

# 4.Create State of Qatar Pixel Map
M0title <- "State of Qatar"
M1title <- "Project Sites vs. Meteosat-10 Pixel Locations"
S0title <- "Pixel Map: MSG Data Servers, Satellite Application Facility, EUMETSAT"
S1title <- "Land Map: GSHHS and WDBII Databases, National Geophysical Data Center, NOAA"
par(op)
par(mar = c(2,2,2,2) + 0.01, pin = c(3.73, 7.5))
plot(q.shoreGSHHS, xaxs = "i", yaxs = "i", axes = FALSE, bg = "lightcyan", col = "papayawhip", border = "lightskyblue3")
plot(q.borderNOAA, lwd = 1.3, col = "grey45", add = TRUE)
plot(q.adminGADM, lwd = 0.5, col = "grey60", add = TRUE)
plot(sp.pixels, col = "plum", cex = 0.45, add = TRUE)
plot(spdf.sites, col = "red", pch = 16, cex = 0.95, add = TRUE)
plot(buff10k, lty = 2, border = "red", add = TRUE)
plot(buff5k, lty = 2, border = "red", add = TRUE)
axis(1, at = c(50.8 + 0:5 * 0.2), pos = 24.393, las = 1, cex.axis = 0.5, col = "grey20",
     col.axis = "grey20", labels = parse(text = sp:::degreeLabelsEW(c(50.8 + 0:5 * 0.2))))
axis(2, at = c(24.5 + 0:10 * 0.2), pos = 50.65, las = 2, cex.axis = 0.5, col = "grey20",
     col.axis = "grey20", labels = parse(text = sp:::degreeLabelsNS(c(24.5 + 0:10 * 0.2))))
axis(3, at = c(50.8 + 0:5 * 0.2), pos = 26.2927, las = 1, cex.axis = 0.5, col = "grey20",
     col.axis = "grey20", labels = parse(text = sp:::degreeLabelsEW(c(50.8 + 0:5 * 0.2))))
axis(4, at = c(24.5 + 0:10 * 0.2), pos = 51.695, las = 2, cex.axis = 0.5, col = "grey20",
     col.axis = "grey20", labels = parse(text = sp:::degreeLabelsNS(c(24.5 + 0:10 * 0.2))))
title(main = M0title, font.main = 2, line = 3.25)
title(main = M1title, cex.main = 0.85, font.main = 2, line = 2.25)
title(sub = S0title, cex.sub = 0.80, font.sub = 3, line = 2.0)
title(sub = S1title, cex.sub = 0.80, font.sub = 3, line = 3.00)
box(col = "grey20", lwd = 1.3)
text(x = 51.1, y = 24.9, label = "Jarayan Al Batnah", cex = .6)
text(x = 51.46, y = 25.0, label = "Al Wakrah", cex = .6)
text(x = 50.95, y = 25.4, label = "Al Jumayliyah", cex = .6)
text(x = 51.15, y = 25.785, label = "Al Ghuwayriyha", cex = .6)
text(x = 51.431, y = 25.235, label = "Ar Rayyan", cex = .6)
text(x = 51.58, y = 25.32, label = "Ad Dawah", cex = .6)
text(x = 51.405, y = 25.46, label = "Umm Salal", cex = .6)
text(x = 51.39, y = 25.7, label = "Al Khawr", cex = .6)
text(x = 51.21, y = 25.95, label = "Ash Shamal", cex = .6)
##----------------------------------------------------------------------------------------##

# 5. Extract Satellite Pixels Inside the 10km and 5km Buffer Zones
# first ensure the required data objects have the same CRS
proj4string(sp.pixels) <- WGS84
proj4string(buff10k) <- WGS84
proj4string(buff5k) <- WGS84

# Next two lines confirm the beauty of sp methods
# A few dozen satellite pixel points are easily extracted from hundreds of millions!!!
buff5k <- gBuffer(spdf.sites, width = 0.05)
buff10k <- gBuffer(spdf.sites, width = 0.10)
##----------------------------------------------------------------------------------------##

# 6. Confirm pixel ID# and coordinates for FTP download
ftplist.5k@coords
##----------------------------------------------------------------------------------------##

# 7.Functions ----
# a simple function to interogate satellite grid resolution (km) for any latitude
# recognizing that the distance between satellite pixel points will change given
# the curvature of the Earth

Meteo.Res <- function(d.lat, d.lon, lat){
  # d.lat = north/south distance in degrees for satellite resolution design spec
  # d.lon = east/west distance in degrees for satellite resoultion design spec
  # lat = the latitude of interest used to determine the distance between satellite
  # pixel points on the Earth's surface

  # convert inputs from degrees to radians
  d.lat <- d.lat * pi/180
  d.lon <- d.lon * pi/180
  lat <- lat * pi/180
  # assumed radius of the Earth (km)
  R <- 6378
  # spatial resolution (km) in north-south direction
  res.NS.km <- R * d.lat
  # Spatial resolution (km) in west-east direction
  res.WE.km <- R * d.lon * cos(lat)
  # Combine results
  res.km <- matrix(c(res.NS.km, res.WE.km), ncol=1,
                   dimnames = list(c("res.NS", "res.WE"), "km"))
  return(res.km)
}

# Test function
Meteo.Res(.05, .05, 0)