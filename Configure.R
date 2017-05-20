# Header ----
#
# Name:        0_Config.R
#
# Title:       Configure project workspace
#
# Version:     1.0
# Date:        2017-Jan-28
# Author:      Brad Horn
# License:     GPL (>=2)
#
# Description: Script to centralize all project configurations
#
# Details:     The script will load required libraries, set project file paths,
#              source custom functions, set workspace options, configure devices, and
#              define geographic coordinate systems and map projections.  The script
#              is essential for managing large projects, while supporting repeatable
#              results and code sharing.
#
# Dev.Notes:   NA
#
# Depends      The R programming language (3.3) and all listed libraries. Custom
#              functions include:
#                   - Convert.R for custom map conversion and pruning functions
#                   - themeMap.R for custom map plot formatting
#
# References:  see http://spatialreference.org/ and http://www.radicalcartography.net/?projectionref
#              for details on map projections, coordinate reference systems and
#              PROJ4 strings
##-------------------------------------------------------------------------------------------##

# 0.Memory Management ----
# clear all environment data, but not function objects, and reset memory
# rm(list = ls()[which(sapply(ls(), function(x) !is.function(get(x))))])
# gc(reset = TRUE)

# 1.Libraries ----
# Load RStudio enhancements
library(rstudioapi)                          # access RStudio API
library(colourpicker)                        # Shiny tool for colour selection

# Load graphical devices
library(gridExtra)                           # Misc graphics functions
library(ggplot2)                             # Grammar of Graphics
library(ggthemes)                            # plot themese for ggplot
library(ggmap)                               # spatial visualization with ggplot2
library(GGally)                              # extension of ggplot
library(iplots)                              # interactive graphics
library(scales)                              # scale functions
library(RColorBrewer)                        # colour ramps
library(viridis)                             # gradient colours
library(plotrix)                             # various plotting functions
library(choroplethr)                         # simplify creation of choropleth maps

# Load data analytic libraries
library(rgdal)                               # bindings for the Geospatial Data Abstraction Library
library(sp)                                  # classes and methods for spatial data
library(rgeos)                               # interface to GEOS: geometry engine open source
library(raster)                              # geographic data anlysis and modeling
library(rasterVis)                           # visualization methods for raster data
library(spbabel)                             # Convert sp data using to tidy tables
library(spdplyr)                             # data manipulation for spatial classes
library(spdep)                               # spatial weighting schemes, statistics and models

library(OpenStreetMap)                       # access OSM raster images
library(osmar)                               # OSM and R
library(spatial)                             # kriging and point pattern analysis
library(fields)                              # tools for spatial data
library(maptools)                            # read and manage spatial objects
library(classInt)                            # univariate class intervals
library(geoR)                                # analysis of geostatistical data
library(geosphere)                           # spherical geometry
library(gstat)                               # spatial and temporal models, prediction, simulation
library(graticule)                           # meridian and parallel lines for maps

# Load data mgmt libraries
library(openxlsx)                            # interface for MS Excel
library(UScensus2010)                        # US Census and ACS data
library(UScensus2010blk)
library(UScensus2010blkgrp)
library(UScensus2010cdp)
library(UScensus2010county)
library(UScensus2010tract)
library(tigris)                              # Load Census shapefilefiles
library(acs)                                 # US Census American Community Survey
library(tibble)                              # simple data.frames
library(data.table)                          # extension of data.frame
library(dtplyr)		                    # data.table + dplyer
library(tidyr)                               # additional reshaping
library(lubridate)                           # data/time objects
library(forcats)                             # data factor mgmt

# resolve library conflicts and set defaults as desired
# to identify conflicts use conflicts(detail = TRUE)
melt <- reshape2::melt
filter <- dplyr::filter
mutate <- dplyr::mutate
select <- dplyr::select
expand <- tidyr::expand
extract <- tidyr::extract
complete <- tidyr::complete
layer <- ggplot2::layer
area <- raster::area
chol <- base::chol
##-------------------------------------------------------------------------------------------##

# 2.Project Paths ----
cache.path <- file.path(getwd(), "cache/")
data.path <- file.path(getwd(), "data/")
plot.path <- file.path(getwd(), "Rplots/")
report.path <- file.path(getwd(), "reports/")
source.path <- file.path(getwd(), "src/")
lib.path <- file.path(getwd(), "packrat", "lib", R.version$platform, getRversion())
archive <- "/mnt/timecapsule/BXH/"

##-------------------------------------------------------------------------------------------##

# 3.Source custom GIS Functions ----
# see referenced file headers for details
source("~/0-RProjects/UScensus/src/F1_Convert.R")
source("~/0-RProjects/UScensus/src/F2_themeMap.R")
source("~/0-RProjects/UScensus/src/F3_sh.Clip.R")
##-------------------------------------------------------------------------------------------##

# 4.Configure Project Workspace ----
# options
options(prompt = "R> ")
options(width = 95)
options(digits = 4, scipen = 3)
options(max.print = 99999)
options(stringsAsFactors = FALSE)
options(papersize = "a4")
options(repos = "https://cran.rstudio.com")
Sys.setenv(TZ = "Asia/Qatar")
Sys.setenv(R_HISTSIZE = '100000')
##-------------------------------------------------------------------------------------------##

# 5.Configure Graphic Devices ----
# store basic plot parameters (op = old.parameters)
op <- par(no.readonly = TRUE)
# customize plot parameters for mapping
par(mai = c(1.02, 0.82, 0.82, 1.02), mar = c(5.1, 4.1, 4.1, 5.1))

# create custom plot themes for map data analysis
theme.Dat <- theme_gdocs() +
     theme(plot.title = element_text(size = 15),
           plot.subtitle = element_text(size = 11),
           plot.caption = element_text(size = 9, hjust = 0, vjust = -15),
           axis.title.y = element_text(face = "bold", color = "gray30"),
           axis.title.x = element_text(face = "bold", color = "gray30", vjust = -0.25))

# define trellis plot parameters and formatting
opt <- trellis.par.get()
trellis.colors <- c("#DFDFDF", "#BFBFBF", "#9F9F9F", "#808080", "#606060",
                    "#404040", "#202020")
trellis.par.set(strip.background = list(col = trellis.colors))
##-------------------------------------------------------------------------------------------##

# 6.Coordinate Reference Systems ----
# configure map projections
# NOTE: keep on one line; avoid white-space around CRS equal sign
NAD83 <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
NATATLAS <- CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
ALBERS <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
LAMBERT <- CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
ECONIC <- CRS("+proj=eqdc +lat_0=39 +lon_0=-96 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
WGS84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
ROBINSON <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
UTM <- CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
gpclibPermit()
##-------------------------------------------------------------------------------------------##

