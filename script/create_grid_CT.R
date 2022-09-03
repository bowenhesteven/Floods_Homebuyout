# In this script, we divided the Davidson county into grids and overlay all the indicator's layers on those grids
library(raster)
library(tidycensus)
library(sf)
library(dplyr)
library(tidyverse)
library(gt)


census_tract <- population_2010 %>% dplyr::select(-variable, -estimate, -moe) %>% rename('CT_NAME' = 'NAME') %>%
st_union()
plot(census_tract)

census_tract_grid <- census_tract %>% 
  st_make_grid(n = 324, what = "polygons") %>% # grid of points
  st_intersection(census_tract)                               

 ggplot() + 
 geom_sf(data = census_tract$geometry) + 
 geom_sf(data = census_tract_grid) 
 #geom_sf(data = census_tract$geometry) 
 
 st_write(census_tract_grid, dsn = "/Users/hebowen/Desktop/MarTREC_project/Data/2018_Data", layer = "census_tract_grid",
          driver = "ESRI Shapefile", append = FALSE)