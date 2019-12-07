library(sf)
library(sp)
library(tmap)
library(rgeos)
library(rgdal)
library(tidyverse)

setwd("~/Desktop/Fall 2019/GIS I/Final Project")

CRSnew <- CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

IL_boundary <- read_sf("IL_BNDY_County/IL_BNDY_County_Py.shp")
IL_boundary <- st_transform(IL_boundary, 3435)

NE_boundary <- IL_boundary %>%
  filter(COUNTY_NAM %in% c("COOK", "DUPAGE", "KANE", "KENDALL", "LAKE", "MCHENRY", "WILL"))

plot(NE_boundary["geometry"])

agland2013 <- read_sf("AgLand 2013.shp")

agland2005 <- read_sf("AgLand 2005.shp")

st_crs(NE_boundary)
st_crs(agland2013)
st_crs(agland2005)

tm_shape(NE_boundary) +
  tm_borders() +
tm_shape(agland2013) +
  tm_polygons() +
tm_layout(frame = FALSE)

# union <- st_union(NE_boundary, agland2013)
# union_test <- st_union(agland2013, NE_boundary, by_feature = TRUE)
# union2 <- st_union(agland2013, NE_boundary)

### Join 2013 Ag Land with County Boundaries

union3 <- st_join(agland2013, NE_boundary)

# check projection
st_crs(union3)

tm_shape(union3) +
  tm_borders() +
  tm_layout(frame = FALSE)

# save shapefile
st_write(union3, "FINAL Union Ag Land_County Shapefile.shp")

### Join 2005 Ag Land with County Boundaries

union4 <- st_join(agland2005, NE_boundary)

# check projection
st_crs(union4)

tm_shape(union4) +
  tm_borders() +
  tm_layout(frame = FALSE)

# save shapefile
st_write(union4, "FINAL 2005 Union Ag Land_County Shapefile.shp")


