library(sf)
library(sp)
library(tmap)
library(rgeos)
library(rgdal)
library(tidyverse)

setwd("~/Desktop/Fall 2019/GIS I/Final Project")

agland_area <- read_sf("FINAL Ag Land Project File.shp")
agland_area <- st_transform(agland_area, 3435)
st_crs(agland_area)

agland_df <- st_set_geometry(agland_area, NULL)

TotalArea_byCounty <- 
  agland_df %>%
  group_by(COUNTY_NAM) %>%
  summarize(TotalArea = sum(Shape_Area))


