# Dasymetric mapping of ACS indicators using tax parcel data as ancillary data.

library(foreach)
library(doParallel)
library(dplyr)
library(sf)
library(raster)
library(rgeos)
library(sp)
library(spdplyr)
library(stringr)
library(rgdal)
library(tibble)
library(tidyverse)


projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" 
# ---------------------------------------2010 year-------------------------------------------------------------
##---------------------------------------population-----------------------------------------------------------
setwd('~/Desktop/MarTREC_project/Data')
wd <- getwd()
# Read in ACS population data
ACS_CT_2010 <- readRDS(file = 'ACS_census_tract_2010.Rds')
population_CT_2010 <- ACS_CT_2010$population_2010 %>%
  rename('pop' = 'estimate') %>% st_as_sf %>% st_transform(crs = projection)

# Read in parcel data
p_2010 <- readRDS(paste0(wd,"/Nashville_Parcel_Data/Parcels2010/","parcel2010.RDS")) %>% st_as_sf()

# Remove tax pacel data based on duplicated STANPAR
p_2010 <- p_2010[!duplicated(p_2010$STANPAR),]

population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, population_CT_2010) #conduct spatial join on sf objects
pop_parcel_spatialjoin_2010 <- population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
pop_parcel_spatialjoin_2010 <- pop_parcel_spatialjoin_2010[!duplicated(pop_parcel_spatialjoin_2010$STANPAR),]

pop_sums_2010 <- pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_pop_parcel_2010 <- pop_parcel_spatialjoin_2010 %>% left_join(pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = pop*frac_LA, dasy_pop_DU = pop*frac_DU)

st_write(dasy_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/pop", layer = "2010_parcel_dasy_population",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_pop_parcel_2010 = dasy_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/pop", "dasy_pop_parcel_2010.Rds"))

##---------------------------------------male-population-----------------------------------------------------------
male_population_CT_2010 <- ACS_CT_2010$male_population_2010 %>%
  rename(male_pop = estimate ) %>% st_as_sf %>% st_transform(crs = projection)

male_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, male_population_CT_2010) #conduct spatial join on sf objects
male_pop_parcel_spatialjoin_2010 <- male_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
male_pop_parcel_spatialjoin_2010 <- male_pop_parcel_spatialjoin_2010[!duplicated(male_pop_parcel_spatialjoin_2010$STANPAR),]

male_pop_sums_2010 <- male_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_male_pop_parcel_2010 <- male_pop_parcel_spatialjoin_2010 %>% left_join(male_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = male_pop*frac_LA, dasy_pop_DU = male_pop*frac_DU)

st_write(dasy_male_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/male-pop", layer = "2010_parcel_dasy_male_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_male_pop_parcel_2010 = dasy_male_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/male-pop", "dasy_male_pop_parcel_2010.Rds"))
##---------------------------------------female-population-----------------------------------------------------------
female_population_CT_2010 <- ACS_CT_2010$female_population_2010 %>%
  rename(female_pop = estimate ) %>% st_as_sf %>% st_transform(crs = projection)

female_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, female_population_CT_2010) #conduct spatial join on sf objects
female_pop_parcel_spatialjoin_2010 <- female_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
female_pop_parcel_spatialjoin_2010 <- female_pop_parcel_spatialjoin_2010[!duplicated(female_pop_parcel_spatialjoin_2010$STANPAR),]


female_pop_sums_2010 <- female_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_female_pop_parcel_2010 <- female_pop_parcel_spatialjoin_2010 %>% left_join(female_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = female_pop*frac_LA, dasy_pop_DU = female_pop*frac_DU)

st_write(dasy_female_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/female-pop", layer = "2010_parcel_dasy_female_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_female_pop_parcel_2010 = dasy_female_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/female-pop", "dasy_female_pop_parcel_2010.Rds"))

##----------------------------------------pop-disable-------------------------------------------------
disable_population_CT_2010 <- ACS_CT_2010$pop_disability_2012 %>%
  rename(disable_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)

disable_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, disable_population_CT_2010) #conduct spatial join on sf objects
disable_pop_parcel_spatialjoin_2010 <- disable_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
disable_pop_parcel_spatialjoin_2010 <- disable_pop_parcel_spatialjoin_2010[!duplicated(disable_pop_parcel_spatialjoin_2010$STANPAR),]

disable_pop_sums_2010 <- disable_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()
  
dasy_disable_pop_parcel_2010 <- disable_pop_parcel_spatialjoin_2010 %>% left_join(disable_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = ifelse(totDU == 0 ,0 ,DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = disable_pop*frac_LA, dasy_pop_DU = disable_pop*frac_DU)

st_write(dasy_disable_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/disable-pop", layer = "2010_parcel_dasy_disable_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_disable_pop_parcel_2010 = dasy_disable_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/disable-pop", "dasy_disable_pop_parcel_2010.Rds"))

##-----------------------------------poverty----------------------------------------------------
poverty_population_CT_2010 <- ACS_CT_2010$poverty_2010 %>%
  rename(poverty_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)
  
poverty_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, poverty_population_CT_2010) #conduct spatial join on sf objects
poverty_pop_parcel_spatialjoin_2010 <- poverty_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
poverty_pop_parcel_spatialjoin_2010 <- poverty_pop_parcel_spatialjoin_2010[!duplicated(poverty_pop_parcel_spatialjoin_2010$STANPAR),]

poverty_pop_sums_2010 <- poverty_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_poverty_pop_parcel_2010 <- poverty_pop_parcel_spatialjoin_2010 %>% left_join(poverty_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = poverty_pop*frac_LA, dasy_pop_DU = poverty_pop*frac_DU)

st_write(dasy_poverty_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/poverty", layer = "2010_parcel_dasy_poverty_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_poverty_pop_parcel_2010 = dasy_poverty_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/poverty", "dasy_poverty_parcel_2010.Rds"))

##---------------------------------limit-education-attain---------------------------------------
limit_education_attain_population_CT_2010 <- ACS_CT_2010$limit_education_attain_2012 %>%
  rename(limit_edu_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)
  
limit_education_attain_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, limit_education_attain_population_CT_2010) #conduct spatial join on sf objects
limit_education_attain_pop_parcel_spatialjoin_2010 <- limit_education_attain_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
limit_education_attain_pop_parcel_spatialjoin_2010 <- limit_education_attain_pop_parcel_spatialjoin_2010[!duplicated(limit_education_attain_pop_parcel_spatialjoin_2010$STANPAR),]
  
limit_education_attain_pop_sums_2010 <- limit_education_attain_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_limit_education_attain_pop_parcel_2010 <- limit_education_attain_pop_parcel_spatialjoin_2010 %>% left_join(limit_education_attain_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = ifelse(totDU == 0, 0 , DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = limit_edu_pop*frac_LA, dasy_pop_DU = limit_edu_pop*frac_DU)

st_write(dasy_limit_education_attain_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/limit-education-attain", layer = "2010_parcel_dasy_limit_education_attain_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_limit_education_attain_pop_parcel_2010 = dasy_limit_education_attain_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/limit-education-attain", "dasy_limit_education_attain_parcel_2010.Rds"))

##--------------------------------africa-america-------------------------------------------------
black_CT_2010 <- ACS_CT_2010$african_american_2010 %>%
  rename(black = estimate) %>% st_as_sf %>% st_transform(crs = projection)

black_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, black_CT_2010) #conduct spatial join on sf objects
black_pop_parcel_spatialjoin_2010 <- black_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
black_pop_parcel_spatialjoin_2010 <- black_pop_parcel_spatialjoin_2010[!duplicated(black_pop_parcel_spatialjoin_2010$STANPAR),]

black_pop_sums_2010 <- black_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_black_pop_parcel_2010 <- black_pop_parcel_spatialjoin_2010 %>% left_join(black_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = black*frac_LA, dasy_pop_DU = black*frac_DU)

st_write(dasy_black_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/africa-america", layer = "2010_parcel_dasy_black_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_black_pop_parcel_2010 = dasy_black_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/africa-america", "dasy_black_pop_parcel_2010.Rds"))

##--------------------------------hispanic------------------------------------------------------
hispanic_CT_2010 <- ACS_CT_2010$hispanic_2010 %>%
  rename(hispanic = estimate) %>% st_as_sf %>% st_transform(crs = projection)

hispanic_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, hispanic_CT_2010) #conduct spatial join on sf objects
hispanic_pop_parcel_spatialjoin_2010 <- hispanic_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
hispanic_pop_parcel_spatialjoin_2010 <- hispanic_pop_parcel_spatialjoin_2010[!duplicated(hispanic_pop_parcel_spatialjoin_2010$STANPAR),]

hispanic_pop_sums_2010 <- hispanic_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_hispanic_pop_parcel_2010 <- hispanic_pop_parcel_spatialjoin_2010 %>% left_join(hispanic_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = hispanic*frac_LA, dasy_pop_DU = hispanic*frac_DU)

st_write(dasy_hispanic_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/hispanic", layer = "2010_parcel_dasy_hispanic_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_hispanic_pop_parcel_2010 = dasy_hispanic_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/hispanic", "dasy_hispanic_pop_parcel_2010.Rds"))

##--------------------------------white----------------------------------------------------------
white_CT_2010 <- ACS_CT_2010$white_only_2010 %>%
  rename(white = estimate) %>% st_as_sf %>% st_transform(crs = projection)

white_population_parcel_spatialjoin_2010 <- sf::st_join(p_2010, white_CT_2010) #conduct spatial join on sf objects
white_pop_parcel_spatialjoin_2010 <- white_population_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
white_pop_parcel_spatialjoin_2010 <- white_pop_parcel_spatialjoin_2010[!duplicated(white_pop_parcel_spatialjoin_2010$STANPAR),]

white_pop_sums_2010 <- white_pop_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_white_pop_parcel_2010 <- white_pop_parcel_spatialjoin_2010 %>% left_join(white_pop_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = LA_SQ_FT/totLA, frac_DU = DU_COUNT / totDU) %>%
  mutate(dasy_pop_LA = white*frac_LA, dasy_pop_DU = white*frac_DU)

st_write(dasy_white_pop_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/white", layer = "2010_parcel_dasy_white_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_white_pop_parcel_2010 = dasy_white_pop_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/white", "dasy_white_pop_parcel_2010.Rds"))

##---------------------------household-senior-----------------------------------------------------
household_senior_CT_2010 <- ACS_CT_2010$household_senior_2010 %>%
  rename(household_senior = estimate) %>% st_as_sf %>% st_transform(crs = projection)

household <- c("SINGLE FAMILY DWELLING","DUPLEX","SFD(S) - RURAL","RESIDENTIAL CONDOMINIUM", "COMMERCIAL CONDOMINIUM", "MOBILE HOMES(S) - RURAL", "APARTMENT LOW-RISE","DORMITORY/BOARDING HOUSE","RESIDENTIAL COMBO. OR MISC.","TRIPLEX",  "MOBILE HOME(S)","APARTMENT HIGH-RISE","PARSONAGE","QUADRAPLEX","APARTMENT HIGH-RISE","MOBILE HOME PARK", "DUPLEX(S) - RURAL","TRIPLEX(S) - RURAL")
p_2010_household <- p_2010
p_2010_household$LA_SQ_FT <- ifelse(p_2010$LAND_USE_D %in% household,p_2010$LA_SQ_FT,0)
p_2010_household$DU_COUNT <- ifelse(p_2010$LAND_USE_D %in% household,p_2010$DU_COUNT,0)

household_senior_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, household_senior_CT_2010) #conduct spatial join on sf objects
household_senior_parcel_spatialjoin_2010 <- household_senior_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
household_senior_parcel_spatialjoin_2010 <- household_senior_parcel_spatialjoin_2010[!duplicated(household_senior_parcel_spatialjoin_2010$STANPAR),]

household_senior_sums_2010 <- household_senior_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_household_senior_parcel_2010 <- household_senior_parcel_spatialjoin_2010 %>% left_join(household_senior_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = household_senior*frac_LA, dasy_pop_DU = household_senior*frac_DU)

st_write(dasy_household_senior_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/household-senior", layer = "2010_parcel_dasy_household_senior",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_household_senior_parcel_2010 = dasy_household_senior_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/household-senior", "dasy_household_senior_parcel_2010.Rds"))

##--------------------------family-own-children----------------------------------------------------
family_own_children_CT_2010 <- ACS_CT_2010$family_own_children_2010 %>%
  rename(family_own_children = estimate) %>% st_as_sf %>% st_transform(crs = projection)

family_own_children_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, family_own_children_CT_2010) #conduct spatial join on sf objects
family_own_children_parcel_spatialjoin_2010 <- family_own_children_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
family_own_children_parcel_spatialjoin_2010 <- family_own_children_parcel_spatialjoin_2010[!duplicated(family_own_children_parcel_spatialjoin_2010$STANPAR),]
 
family_own_children_sums_2010 <- family_own_children_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_family_own_children_parcel_2010 <- family_own_children_parcel_spatialjoin_2010 %>% left_join(family_own_children_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = family_own_children*frac_LA, dasy_pop_DU = family_own_children*frac_DU)

st_write(dasy_family_own_children_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/family-own-children", layer = "2010_parcel_dasy_family_own_children",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_family_own_children_parcel_2010 = dasy_family_own_children_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/family-own-children", "dasy_family_own_children_parcel_2010.Rds"))

##-------------------------total-household-----------------------------------------------------------
total_household_CT_2010 <- ACS_CT_2010$total_household_2010 %>%
  rename(total_household = estimate) %>% st_as_sf %>% st_transform(crs = projection)

total_household_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, total_household_CT_2010) #conduct spatial join on sf objects
total_household_parcel_spatialjoin_2010 <- total_household_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
total_household_parcel_spatialjoin_2010 <- total_household_parcel_spatialjoin_2010[!duplicated(total_household_parcel_spatialjoin_2010$STANPAR),]

total_household_sums_2010 <-total_household_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_total_household_parcel_2010 <- total_household_parcel_spatialjoin_2010 %>% left_join(total_household_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = total_household*frac_LA, dasy_pop_DU = total_household*frac_DU)

st_write(dasy_total_household_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/total-household", layer = "2010_parcel_dasy_total_household",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_total_household_parcel_2010 = dasy_total_household_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/total-household", "dasy_total_household_parcel_2010.Rds"))

##-----------------------single-parent-family-------------------------------------------------------
single_parent_family_CT_2010 <- ACS_CT_2010$single_parent_family_2010 %>%
  rename(single_parent_family = estimate) %>% st_as_sf %>% st_transform(crs = projection)

single_parent_family_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, single_parent_family_CT_2010) #conduct spatial join on sf objects
single_parent_family_parcel_spatialjoin_2010 <- single_parent_family_parcel_spatialjoin_2010[, c(1,12,13,18,19,20)] 
single_parent_family_parcel_spatialjoin_2010 <- single_parent_family_parcel_spatialjoin_2010[!duplicated(single_parent_family_parcel_spatialjoin_2010$STANPAR),]

single_parent_family_sums_2010 <- single_parent_family_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_single_parent_family_parcel_2010 <- single_parent_family_parcel_spatialjoin_2010 %>% left_join(single_parent_family_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = single_parent_family*frac_LA, dasy_pop_DU =single_parent_family*frac_DU)

st_write(dasy_single_parent_family_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/single-parent-family", layer = "2010_parcel_dasy_single_parent_family",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_single_parent_family_parcel_2010 = dasy_single_parent_family_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/single-parent-family", "dasy_single_parent_family_parcel_2010.Rds"))

##---------------------single-person-household------------------------------------------------------
single_person_household_CT_2010 <- ACS_CT_2010$single_person_household_2010 %>%
  rename(single_person_household = estimate) %>% st_as_sf %>% st_transform(crs = projection)

single_person_household_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, single_person_household_CT_2010) #conduct spatial join on sf objects
single_person_household_parcel_spatialjoin_2010 <- single_person_household_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
single_person_household_parcel_spatialjoin_2010 <- single_person_household_parcel_spatialjoin_2010[!duplicated(single_person_household_parcel_spatialjoin_2010$STANPAR),]

single_person_household_sums_2010 <- single_person_household_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_single_person_household_parcel_2010 <- single_person_household_parcel_spatialjoin_2010 %>% left_join(single_person_household_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = single_person_household*frac_LA, dasy_pop_DU =single_person_household*frac_DU)

st_write(dasy_single_person_household_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/single-person-household", layer = "2010_parcel_dasy_single_person_household",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_single_person_household_parcel_2010 = dasy_single_person_household_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/single-person-household", "dasy_single_person_household_parcel_2010.Rds"))

##------------------------total-housing-units-------------------------------------------------------
total_housing_units_CT_2010 <- ACS_CT_2010$total_housing_units_2010 %>%
  rename(total_housing_units = estimate) %>% st_as_sf %>% st_transform(crs = projection)

total_housing_units_parcel_spatialjoin_2010 <- sf::st_join(p_2010_household, total_housing_units_CT_2010) #conduct spatial join on sf objects
total_housing_units_parcel_spatialjoin_2010 <- total_housing_units_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
total_housing_units_parcel_spatialjoin_2010 <- total_housing_units_parcel_spatialjoin_2010[!duplicated(total_housing_units_parcel_spatialjoin_2010$STANPAR),]

total_housing_units_sums_2010 <- total_housing_units_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_total_housing_units_parcel_2010 <- total_housing_units_parcel_spatialjoin_2010 %>% left_join(total_housing_units_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = total_housing_units*frac_LA, dasy_pop_DU =total_housing_units*frac_DU)

st_write(dasy_total_housing_units_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/total-housing-units", layer = "2010_parcel_dasy_total_housing_units",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_total_housing_units_parcel_2010 = dasy_total_housing_units_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/total-housing-units", "dasy_total_housing_units_parcel_2010.Rds"))

##------------------------multi-family-housing-------------------------------------------------------
multi_family_housing_CT_2010 <- ACS_CT_2010$multi_family_housing_2010 %>%
  rename(multi_family_housing = estimate) %>% st_as_sf %>% st_transform(crs = projection)

multi_family_housing <- c("DUPLEX","RESIDENTIAL CONDOMINIUM","COMMERCIAL CONDOMINIUM","APARTMENT LOW-RISE","DORMITORY/BOARDING HOUSE","RESIDENTIAL COMBO. OR MISC.","TRIPLEX","APARTMENT HIGH-RISE","QUADRAPLEX","APARTMENT HIGH-RISE", "DUPLEX(S) - RURAL","TRIPLEX(S) - RURAL")
p_2010_multifamily <- p_2010
p_2010_multifamily$LA_SQ_FT <- ifelse(p_2010_multifamily$LAND_USE_D %in% multi_family_housing, p_2010$LA_SQ_FT,0)
p_2010_multifamily$DU_COUNT <- ifelse(p_2010_multifamily$LAND_USE_D %in% multi_family_housing, p_2010$DU_COUNT,0)

multi_family_housing_parcel_spatialjoin_2010 <- sf::st_join(p_2010_multifamily, multi_family_housing_CT_2010) #conduct spatial join on sf objects
multi_family_housing_parcel_spatialjoin_2010 <- multi_family_housing_parcel_spatialjoin_2010[, c(1,12,13,18,19,21)] 
multi_family_housing_parcel_spatialjoin_2010 <- multi_family_housing_parcel_spatialjoin_2010[!duplicated(multi_family_housing_parcel_spatialjoin_2010$STANPAR),]

multi_family_housing_sums_2010 <- multi_family_housing_parcel_spatialjoin_2010 %>%
  group_by (GEOID) %>% summarise (totLA=sum(LA_SQ_FT), totDU=sum(DU_COUNT)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_multi_family_housing_parcel_2010 <- multi_family_housing_parcel_spatialjoin_2010 %>% left_join(multi_family_housing_sums_2010, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA == 0, 0, LA_SQ_FT/totLA), frac_DU = ifelse(totDU == 0, 0, DU_COUNT / totDU)) %>%
  mutate(dasy_pop_LA = multi_family_housing*frac_LA, dasy_pop_DU = multi_family_housing*frac_DU)

st_write(dasy_multi_family_housing_parcel_2010, dsn = "~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/multi-family-housing", layer = "2010_parcel_dasy_multi_family_housing",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_multi_family_housing_parcel_2010 = dasy_multi_family_housing_parcel_2010),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2010_Data/2010_parcel_dasymetric/multi-family-housing", "dasy_multi_family_housing_parcel_2010.Rds"))

# ---------------------------------------2018 year-------------------------------------------------------------
##---------------------------------------population-----------------------------------------------------------
# Read in ACS population data
setwd('~/Desktop/MarTREC_project/Data')
wd <- getwd()

ACS_CT_2018 <- readRDS(file = 'ACS_census_tract_2018.Rds')
population_CT_2018 <- ACS_CT_2018$population_2018 %>%
  rename(pop = estimate ) %>% st_as_sf %>% st_transform(crs = projection)

# Read in parcel data
pb_2018 <- readRDS(paste0(wd,"/2018_Data/2018_parcel_dasymetric/","pb2018.RDS")) %>% st_as_sf()
#pb_2018 <- spTransform(pb_2018, CRS(projection)) 
pb_2018 <- pb_2018 %>%
  mutate(appr_story = abs(HEIGHT)/14, appr_sqft = appr_story * SHAPE_STAr)


#proj4string(parcel_2018)==projection #check proj
population_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , population_CT_2018)
pop_pb_spatialjoin_2018 <- population_pb_spatialjoin_2018[, c(6,38,45:50)] 
pop_pb_spatialjoin_2018 <- pop_pb_spatialjoin_2018[!duplicated(pop_pb_spatialjoin_2018$BLDG_ID),]
pop_pb_spatialjoin_2018 <- pop_pb_spatialjoin_2018[!is.na(pop_pb_spatialjoin_2018$GEOID),]

pop_sums_2018 <- pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_pop_pb_2018 <- pop_pb_spatialjoin_2018 %>% left_join(pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = pop*frac_LA, dasy_pop_DU = pop*frac_DU)

st_write(dasy_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/population", layer = "2018_pb_dasy_population",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_pop_pb_2018 = dasy_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/population", "dasy_population_pb_2018.Rds"))

##---------------------------------------male-population-----------------------------------------------------------

male_pop_CT_2018 <- ACS_CT_2018$male_population_2018 %>%
  rename(male_pop = estimate ) %>% st_as_sf %>% st_transform(crs = projection)

#proj4string(parcel_2018)==projection #check proj
male_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , male_pop_CT_2018)
male_pop_pb_spatialjoin_2018 <- male_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
male_pop_pb_spatialjoin_2018 <- male_pop_pb_spatialjoin_2018[!duplicated(male_pop_pb_spatialjoin_2018$BLDG_ID),]
male_pop_pb_spatialjoin_2018 <- male_pop_pb_spatialjoin_2018[!is.na(male_pop_pb_spatialjoin_2018$GEOID),]


male_pop_sums_2018 <- male_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_male_pop_pb_2018 <- male_pop_pb_spatialjoin_2018 %>% left_join(male_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = male_pop*frac_LA, dasy_pop_DU = male_pop*frac_DU)

st_write(dasy_male_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/male-pop", layer = "2018_pb_dasy_male_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_male_pop_pb_2018 = dasy_male_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/male-pop", "dasy_male_pop_pb_2018.Rds"))

##---------------------------------------female-population-----------------------------------------------------------
female_pop_CT_2018 <- ACS_CT_2018$female_population_2018 %>%
  rename(female_pop = estimate ) %>% st_as_sf %>% st_transform(crs = projection)

female_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , female_pop_CT_2018)
female_pop_pb_spatialjoin_2018 <- female_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
female_pop_pb_spatialjoin_2018 <- female_pop_pb_spatialjoin_2018[!duplicated(female_pop_pb_spatialjoin_2018$BLDG_ID),]
female_pop_pb_spatialjoin_2018 <- female_pop_pb_spatialjoin_2018[!is.na(female_pop_pb_spatialjoin_2018$GEOID),]

female_pop_sums_2018 <- female_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_female_pop_pb_2018 <- female_pop_pb_spatialjoin_2018 %>% left_join(female_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = female_pop*frac_LA, dasy_pop_DU = female_pop*frac_DU)

st_write(dasy_female_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/female-pop", layer = "2018_pb_dasy_female_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_female_pop_pb_2018 = dasy_female_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/female-pop", "dasy_female_pop_pb_2018.Rds"))

##------------------------------------------pop-disable---------------------------------------------------------

disable_pop_CT_2018 <- ACS_CT_2018$pop_disability_2018 %>%
  rename(disable_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)

disable_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , disable_pop_CT_2018)
disable_pop_pb_spatialjoin_2018 <- disable_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
disable_pop_pb_spatialjoin_2018 <- disable_pop_pb_spatialjoin_2018[!duplicated(disable_pop_pb_spatialjoin_2018$BLDG_ID),]
disable_pop_pb_spatialjoin_2018 <- disable_pop_pb_spatialjoin_2018[!is.na(disable_pop_pb_spatialjoin_2018$GEOID),]

disable_pop_sums_2018 <- disable_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_disable_pop_pb_2018 <- disable_pop_pb_spatialjoin_2018 %>% left_join(disable_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = disable_pop*frac_LA, dasy_pop_DU = disable_pop*frac_DU)

st_write(dasy_disable_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/disable-pop", layer = "2018_pb_dasy_disable_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_disable_pop_pb_2018 = dasy_disable_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/disable-pop", "dasy_disable_pop_pb_2018.Rds"))

##-----------------------------------poverty----------------------------------------------------
poverty_pop_CT_2018 <- ACS_CT_2018$poverty_2018 %>%
  rename(poverty_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)

poverty_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , poverty_pop_CT_2018)
poverty_pop_pb_spatialjoin_2018 <- poverty_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
poverty_pop_pb_spatialjoin_2018 <- poverty_pop_pb_spatialjoin_2018[!duplicated(poverty_pop_pb_spatialjoin_2018$BLDG_ID),]
poverty_pop_pb_spatialjoin_2018 <- poverty_pop_pb_spatialjoin_2018[!is.na(poverty_pop_pb_spatialjoin_2018$GEOID),]

poverty_pop_sums_2018 <- poverty_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_poverty_pop_pb_2018 <- poverty_pop_pb_spatialjoin_2018 %>% left_join(poverty_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = poverty_pop*frac_LA, dasy_pop_DU = poverty_pop*frac_DU)

st_write(dasy_poverty_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/poverty", layer = "2018_pb_dasy_poverty_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_poverty_pop_pb_2018 = dasy_poverty_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/poverty", "dasy_poverty_pop_pb_2018.Rds"))

##---------------------------------limit-education-attain---------------------------------------
limit_education_attain_pop_CT_2018 <- ACS_CT_2018$limit_education_attain_2018 %>%
  rename(limit_edu_pop = estimate) %>% st_as_sf %>% st_transform(crs = projection)

limit_education_attain_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , limit_education_attain_pop_CT_2018)
limit_education_attain_pop_pb_spatialjoin_2018 <- limit_education_attain_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
limit_education_attain_pop_pb_spatialjoin_2018 <- limit_education_attain_pop_pb_spatialjoin_2018[!duplicated(limit_education_attain_pop_pb_spatialjoin_2018$BLDG_ID),]
limit_education_attain_pop_pb_spatialjoin_2018 <- limit_education_attain_pop_pb_spatialjoin_2018[!is.na(limit_education_attain_pop_pb_spatialjoin_2018$GEOID),]

limit_education_attain_pop_sums_2018 <- limit_education_attain_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_limit_education_attain_pop_pb_2018 <- limit_education_attain_pop_pb_spatialjoin_2018 %>% left_join(limit_education_attain_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = limit_edu_pop * frac_LA, dasy_pop_DU = limit_edu_pop * frac_DU)

st_write(dasy_limit_education_attain_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/limit-education-attain", layer = "2018_pb_dasy_limit_education_attain_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_limit_education_attain_pop_pb_2018 = dasy_limit_education_attain_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/limit-education-attain", "dasy_limit_education_attain_pop_pb_2018.Rds"))

##--------------------------------africa-america-------------------------------------------------
black_pop_CT_2018 <- ACS_CT_2018$african_american_2018 %>%
  rename(black = estimate) %>% st_as_sf %>% st_transform(crs = projection)

black_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , black_pop_CT_2018)
black_pop_pb_spatialjoin_2018 <- black_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
black_pop_pb_spatialjoin_2018 <- black_pop_pb_spatialjoin_2018[!duplicated(black_pop_pb_spatialjoin_2018$BLDG_ID),]
black_pop_pb_spatialjoin_2018 <- black_pop_pb_spatialjoin_2018[!is.na(black_pop_pb_spatialjoin_2018$GEOID),]

black_pop_sums_2018 <- black_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_black_pop_pb_2018 <- black_pop_pb_spatialjoin_2018 %>% left_join(black_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = black * frac_LA, dasy_pop_DU = black * frac_DU)

st_write(dasy_black_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/africa-america", layer = "2018_pb_dasy_black_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_black_pop_pb_2018 = dasy_black_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/africa-america", "dasy_black_pop_pb_2018.Rds"))

##--------------------------------hispanic------------------------------------------------------
hispanic_pop_CT_2018 <- ACS_CT_2018$hispanic_2018 %>%
  rename(hispanic = estimate) %>% st_as_sf %>% st_transform(crs = projection)

hispanic_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , hispanic_pop_CT_2018)
hispanic_pop_pb_spatialjoin_2018 <- hispanic_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
hispanic_pop_pb_spatialjoin_2018 <- hispanic_pop_pb_spatialjoin_2018[!duplicated(hispanic_pop_pb_spatialjoin_2018$BLDG_ID),]
hispanic_pop_pb_spatialjoin_2018 <- hispanic_pop_pb_spatialjoin_2018[!is.na(hispanic_pop_pb_spatialjoin_2018$GEOID),]

hispanic_pop_sums_2018 <- hispanic_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_hispanic_pop_pb_2018 <- hispanic_pop_pb_spatialjoin_2018 %>% left_join(hispanic_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = hispanic * frac_LA, dasy_pop_DU = hispanic * frac_DU)

st_write(dasy_hispanic_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/hispanic", layer = "2018_pb_dasy_hispanic_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_hispanic_pop_pb_2018 = dasy_hispanic_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/hispanic", "dasy_hispanic_pop_pb_2018.Rds"))

##--------------------------------white----------------------------------------------------------
white_pop_CT_2018 <- ACS_CT_2018$white_only_2018 %>%
  rename(white = estimate) %>% st_as_sf %>% st_transform(crs = projection)

white_pop_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , white_pop_CT_2018)
white_pop_pb_spatialjoin_2018 <- white_pop_pb_spatialjoin_2018[, c(6,38,45:50)] 
white_pop_pb_spatialjoin_2018 <- white_pop_pb_spatialjoin_2018[!duplicated(white_pop_pb_spatialjoin_2018$BLDG_ID),]
white_pop_pb_spatialjoin_2018 <- white_pop_pb_spatialjoin_2018[!is.na(white_pop_pb_spatialjoin_2018$GEOID),]

white_pop_sums_2018 <- white_pop_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_white_pop_pb_2018 <- white_pop_pb_spatialjoin_2018 %>% left_join(white_pop_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = white * frac_LA, dasy_pop_DU = white * frac_DU)


st_write(dasy_white_pop_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/white", layer = "2018_pb_dasy_white_pop",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_white_pop_pb_2018 = dasy_white_pop_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/white", "dasy_white_pop_pb_2018.Rds"))

##---------------------------household-senior-----------------------------------------------------
household_senior_CT_2018 <- ACS_CT_2018$household_senior_2018 %>%
  rename(household_senior = estimate) %>% st_as_sf %>% st_transform(crs = projection)

household_senior_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , household_senior_CT_2018)
household_senior_pb_spatialjoin_2018 <- household_senior_pb_spatialjoin_2018[, c(6,38,45:50)] 
household_senior_pb_spatialjoin_2018 <- household_senior_pb_spatialjoin_2018[!duplicated(household_senior_pb_spatialjoin_2018$BLDG_ID),]
household_senior_pb_spatialjoin_2018 <- household_senior_pb_spatialjoin_2018[!is.na(household_senior_pb_spatialjoin_2018$GEOID),]

household_senior_sums_2018 <- household_senior_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_household_senior_pb_2018 <- household_senior_pb_spatialjoin_2018 %>% left_join(household_senior_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = household_senior * frac_LA, dasy_pop_DU = household_senior* frac_DU)

st_write(dasy_household_senior_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/household-senior", layer = "2018_pb_dasy_household_senior",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_household_senior_pb_2018 = dasy_household_senior_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/household-senior", "dasy_household_senior_pb_2018.Rds"))

##--------------------------family-own-children----------------------------------------------------
family_own_children_CT_2018 <- ACS_CT_2018$family_own_children_2018 %>%
  rename(family_own_children = estimate) %>% st_as_sf %>% st_transform(crs = projection)

family_own_children_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , family_own_children_CT_2018)
family_own_children_pb_spatialjoin_2018 <- family_own_children_pb_spatialjoin_2018[, c(6,38,45:50)] 
family_own_children_pb_spatialjoin_2018 <- family_own_children_pb_spatialjoin_2018[!duplicated(family_own_children_pb_spatialjoin_2018$BLDG_ID),]
family_own_children_pb_spatialjoin_2018 <- family_own_children_pb_spatialjoin_2018[!is.na(family_own_children_pb_spatialjoin_2018$GEOID),]

family_own_children_sums_2018 <- family_own_children_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_family_own_children_pb_2018 <- family_own_children_pb_spatialjoin_2018 %>% left_join(family_own_children_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = family_own_children * frac_LA, dasy_pop_DU = family_own_children * frac_DU)

st_write(dasy_family_own_children_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/family-own-children", layer = "2018_pb_dasy_family_own_children",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_family_own_children_pb_2018 = dasy_family_own_children_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/family-own-children", "dasy_family_own_children_pb_2018.Rds"))

##-------------------------total-household-----------------------------------------------------------
total_household_CT_2018 <- ACS_CT_2018$total_household_2018 %>%
  rename(total_household = estimate) %>% st_as_sf %>% st_transform(crs = projection)

total_household_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , total_household_CT_2018)
total_household_pb_spatialjoin_2018 <- total_household_pb_spatialjoin_2018[, c(6,38,45:50)] 
total_household_pb_spatialjoin_2018 <- total_household_pb_spatialjoin_2018[!duplicated(total_household_pb_spatialjoin_2018$BLDG_ID),]
total_household_pb_spatialjoin_2018 <- total_household_pb_spatialjoin_2018[!is.na(total_household_pb_spatialjoin_2018$GEOID),]

total_household_sums_2018 <- total_household_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_total_household_pb_2018 <- total_household_pb_spatialjoin_2018 %>% left_join(total_household_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = total_household * frac_LA, dasy_pop_DU = total_household * frac_DU)

st_write(dasy_total_household_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/total-household", layer = "2018_pb_dasy_total_household",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_total_household_pb_2018 = dasy_total_household_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/total-household", "dasy_total_household_pb_2018.Rds"))

##-----------------------single-parent-family-------------------------------------------------------
single_parent_family_CT_2018 <- ACS_CT_2018$single_parent_family_2018 %>%
  rename(single_parent_family = estimate) %>% st_as_sf %>% st_transform(crs = projection)

single_parent_family_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , single_parent_family_CT_2018)
single_parent_family_pb_spatialjoin_2018 <- single_parent_family_pb_spatialjoin_2018[, c(6,38,45:50)] 
single_parent_family_pb_spatialjoin_2018 <- single_parent_family_pb_spatialjoin_2018[!duplicated(single_parent_family_pb_spatialjoin_2018$BLDG_ID),]
single_parent_family_pb_spatialjoin_2018 <- single_parent_family_pb_spatialjoin_2018[!is.na(single_parent_family_pb_spatialjoin_2018$GEOID),]

single_parent_family_sums_2018 <- single_parent_family_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_single_parent_family_pb_2018 <- single_parent_family_pb_spatialjoin_2018 %>% left_join(single_parent_family_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = single_parent_family * frac_LA, dasy_pop_DU = single_parent_family * frac_DU)

st_write(dasy_single_parent_family_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/single-parent-family", layer = "2018_pb_dasy_single_parent_family",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_single_parent_family_pb_2018 = dasy_single_parent_family_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/single-parent-family", "dasy_single_parent_family_pb_2018.Rds"))

##---------------------single-person-household------------------------------------------------------
single_person_household_CT_2018 <- ACS_CT_2018$single_person_household_2018 %>%
  rename(single_person_household = estimate) %>% st_as_sf %>% st_transform(crs = projection)

single_person_household_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , single_person_household_CT_2018)
single_person_household_pb_spatialjoin_2018 <- single_person_household_pb_spatialjoin_2018[, c(6,38,45:50)] 
single_person_household_pb_spatialjoin_2018 <- single_person_household_pb_spatialjoin_2018[!duplicated(single_person_household_pb_spatialjoin_2018$BLDG_ID),]
single_person_household_pb_spatialjoin_2018 <- single_person_household_pb_spatialjoin_2018[!is.na(single_person_household_pb_spatialjoin_2018$GEOID),]

single_person_household_sums_2018 <- single_person_household_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_single_person_household_pb_2018 <- single_person_household_pb_spatialjoin_2018 %>% left_join(single_person_household_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = single_person_household * frac_LA, dasy_pop_DU = single_person_household * frac_DU)

st_write(dasy_single_person_household_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/single-person-household", layer = "2018_pb_dasy_single_person_household",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_single_person_household_pb_2018 = dasy_single_person_household_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/single-person-household", "dasy_single_person_household_pb_2018.Rds"))

##------------------------total-housing-units-------------------------------------------------------
total_housing_units_CT_2018 <- ACS_CT_2018$total_housing_units_2018 %>%
  rename(total_housing_units = estimate) %>% st_as_sf %>% st_transform(crs = projection)

total_housing_units_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , total_housing_units_CT_2018)
total_housing_units_pb_spatialjoin_2018 <- total_housing_units_pb_spatialjoin_2018[, c(6,38,45:50)] 
total_housing_units_pb_spatialjoin_2018 <- total_housing_units_pb_spatialjoin_2018[!duplicated(total_housing_units_pb_spatialjoin_2018$BLDG_ID),]
total_housing_units_pb_spatialjoin_2018 <- total_housing_units_pb_spatialjoin_2018[!is.na(total_housing_units_pb_spatialjoin_2018$GEOID),]

total_housing_units_sums_2018 <- total_housing_units_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_total_housing_units_pb_2018 <- total_housing_units_pb_spatialjoin_2018 %>% left_join(total_housing_units_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = total_housing_units * frac_LA, dasy_pop_DU = total_housing_units * frac_DU)

st_write(dasy_total_housing_units_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/total-housing-units", layer = "2018_pb_dasy_total_housing_units",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_total_housing_units_pb_2018 = dasy_total_housing_units_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/total-housing-units", "dasy_total_housing_units_pb_2018.Rds"))

##------------------------multi-family-housing-------------------------------------------------------
multi_family_housing_CT_2018 <- ACS_CT_2018$multi_family_housing_2018 %>%
  rename(multi_family_housing = estimate) %>% st_as_sf %>% st_transform(crs = projection)

multi_family_housing_pb_spatialjoin_2018 <- sf::st_join(pb_2018 , multi_family_housing_CT_2018)
multi_family_housing_pb_spatialjoin_2018 <- multi_family_housing_pb_spatialjoin_2018[, c(6,38,45:50)] 
multi_family_housing_pb_spatialjoin_2018 <- multi_family_housing_pb_spatialjoin_2018[!duplicated(multi_family_housing_pb_spatialjoin_2018$BLDG_ID),]
multi_family_housing_pb_spatialjoin_2018 <- multi_family_housing_pb_spatialjoin_2018[!is.na(multi_family_housing_pb_spatialjoin_2018$GEOID),]

multi_family_housing_sums_2018 <- multi_family_housing_pb_spatialjoin_2018 %>%
  group_by(GEOID) %>% summarise (totLA_CT=sum(appr_sqft), total_DU_CT=sum(totDU)) %>% 
  ungroup() %>% st_drop_geometry()

dasy_multi_family_housing_pb_2018 <- multi_family_housing_pb_spatialjoin_2018 %>% left_join(multi_family_housing_sums_2018, by = "GEOID") %>%
  mutate(frac_LA = ifelse(totLA_CT == 0, 0, appr_sqft/totLA_CT), frac_DU = ifelse(total_DU_CT == 0, 0, totDU / total_DU_CT)) %>%
  mutate(dasy_pop_LA = multi_family_housing * frac_LA, dasy_pop_DU = multi_family_housing * frac_DU)

st_write(dasy_multi_family_housing_pb_2018, dsn = "~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/multi-family-housing", layer = "2018_pb_dasy_multi_family_housing",
         driver = "ESRI Shapefile" )

write_rds(list(dasy_multi_family_housing_pb_2018 = dasy_multi_family_housing_pb_2018),
          path = file.path(data_dir ="~/Desktop/MarTREC_project/Data/2018_Data/2018_parcel_dasymetric/multi-family-housing", "dasy_multi_family_housing_pb_2018.Rds"))














