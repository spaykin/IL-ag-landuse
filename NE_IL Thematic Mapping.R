library(sf)
library(sp)
library(tmap)
library(rgeos)
library(rgdal)
library(tidyverse)

setwd("~/Desktop/Fall 2019/GIS I/Final Project")

##### Data Wrangling - Acreage in Operation #####

operators <- read.csv("IL_County_Acreage_Gender.csv")

NE_IL_Counties <- c("COOK", "DUPAGE", "KANE", "KENDALL", "LAKE", "MCHENRY", "WILL")
operators_NE <- operators$County %in% NE_IL_Counties

NE_operators <- operators[operators_NE, ]

NE_operators$Value <- as.numeric(as.character(NE_operators$Value))

total_acreage_1997 <- 
  NE_operators %>%
  filter(Year == "1997") %>%
  group_by(County) %>%
  summarize(TotalAcreage97 = sum(Value))

total_acreage_2002 <- 
  NE_operators %>%
  filter(Year == "2002") %>%
  group_by(County) %>%
  summarize(TotalAcreage02 = sum(Value))

total_acreage_2007 <- 
  NE_operators %>%
  filter(Year == "2007") %>%
  group_by(County) %>%
  summarize(TotalAcreage07 = sum(Value))

total_acreage_2012 <- 
  NE_operators %>%
  filter(Year == "2012") %>%
  group_by(County) %>%
  summarize(TotalAcreage12 = sum(Value))

# Merge datasets for total acres in operation by county by year

Total_Acreage <- 
  data_frame(COUNTY = total_acreage_1997$County, 
             ACRES_97 = total_acreage_1997$TotalAcreage97, 
             ACRES_02 = total_acreage_2002$TotalAcreage02, 
             ACRES_07 = total_acreage_2007$TotalAcreage07, 
             ACRES_12 = total_acreage_2012$TotalAcreage12)
# save file
# write.csv(Total_Acreage, file = "IL_County_Acres_Operated_AllYears.csv")


##### Data Wrangling - Number of Operators #####

operators_count <- read.csv("IL_Farm_Operators_Count.csv")

operators_count_NE <- operators_count$County %in% NE_IL_Counties

NE_operators_count <- operators_count[operators_count_NE, ]

NE_operators_count$Value <- as.numeric(as.character(NE_operators_count$Value))

NE_count_2002 <- 
  NE_operators_count %>%
  filter(Year == "2002") %>%
  group_by(County) %>%
  summarize(Total02 = sum(Value))

NE_count_2007 <- 
  NE_operators_count %>%
  filter(Year == "2007") %>%
  group_by(County) %>%
  summarize(Total07 = sum(Value))

NE_count_2012 <- 
  NE_operators_count %>%
  filter(Year == "2012") %>%
  group_by(County) %>%
  summarize(Total12 = sum(Value))

# Merge datasets for total number of operators by county by year

Total_Count <- 
  data_frame(COUNTY = count_2002$County, 
             COUNT_02 = count_2002$Total02, 
             COUNT_07 = count_2007$Total07, 
             COUNT_12 = count_2012$Total12)

# Save file
# write.csv(Total_Acreage, file = "IL_County_Count_Operators_AllYears.csv")

# FINAL Dataframe - Merge total Acreage and Count data

Total_Ownership <- merge(Total_Acreage, Total_Count, by="COUNTY")

# Save file
# write.csv(Total_Ownership, file = "FINAL IL_County_FarmOwnership.csv")


##### Exploratory Mapping #####

# Load IL County shapefile
county <- read_sf("FINAL County Farm Ownership shapefile/FINAL_IL_County_Ownership_Shapefile.shp")

NE_county <- 
  county %>%
  filter(COUNTY_NAM %in% c("COOK", "DUPAGE", "KANE", "KENDALL", "LAKE", "MCHENRY", "WILL"))

# Check projection
st_crs(NE_county) #no projection...

# Change projection
NE_county <-
  st_transform(NE_county, CRS("+proj=tmerc +lat_0=36.66666666666666 +lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))

# Check variable names
str(NE_county)

plot(NE_county["geometry"])

### ACREAGE MAPS

# Acreage in operation in 2002
NE_acres_2002 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "ACRES_02", title = "Acres (2002)") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE)

NE_acres_2002

# Acreage in operation in 2007
NE_acres_2007 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "ACRES_07", title = "Acres (2007)") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE)

NE_acres_2007

# Acreage in operation in 2012
NE_acres_2012 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "ACRES_12", title = "Acres (2012)") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE) 

NE_acres_2012

# Compare side-by-side
acreage_comparison <- 
  tmap_arrange(NE_acres_2002, NE_acres_2007, NE_acres_2012)

acreage_comparison

### OPERATORS MAPS

# Number of farm operators in 2002
NE_operators_2002 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "COUNT_02", title = "# Operators (2002)", palette = "Blues") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE)

NE_operators_2002

# Number of farm operators in 2007
NE_operators_2007 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "COUNT_07", title = "# Operators (2007)", palette = "Blues") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE)

NE_operators_2007

# Number of farm operators in 2012
NE_operators_2012 <- 
  tm_shape(NE_county) +
  tm_borders(col = "grey", alpha = .7) +
  tm_fill(col = "COUNT_12", title = "# Operators (2012)", palette = "Blues") +
  tm_text(text = "COUNTY_NAM", size = .8) +
  tm_layout(frame = FALSE)

NE_operators_2012

# Compare side-by-side
operators_comparison <- 
  tmap_arrange(NE_operators_2002, NE_operators_2007, NE_operators_2012, ncol = 3)

operators_comparison


###########################

# read in sales data
sales <- read.csv("SalesData.csv")

NE_count_2002 <- 
  NE_operators_count %>%
  filter(Year == "2002") %>%
  group_by(County) %>%
  summarize(Total02 = sum(Value))

sales2002 <- 
  sales %>%
  filter(Year == "2002") %>%
  filter(COUNTY %in%
           c("COOK", "DUPAGE", "KANE", "KENDALL", "LAKE", "MCHENRY", "WILL"))

sales2012 <- 
  sales %>%
  filter(Year == "2012")