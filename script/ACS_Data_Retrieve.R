#---------------------Retrieve-Census-Tract-Data---------------------------
# Here we retrieve the census data of both 2010 and 2018 in this script document.
# Install the census api key
my_api_key <- '017245e9a9b21859ce5997e6be9fb6074d69597e'
#tidycensus
tidycensus::census_api_key(my_api_key, overwrite = TRUE, install = TRUE)
#acs key set
acs::api.key.install(my_api_key)


library(tidyverse)
library(tidycensus)

#Load variables from tidycensus
vars2010 <- load_variables(2010, "acs5", cache = TRUE)
vars2018 <- load_variables(2018, "acs5", cache = TRUE)
#--------------------------------------------------------------2010 census tract-----------------------------------------------------------------------
##-----------------------------------Demographic Diversity--------------------------------------

#1 Population

# year of 2010
population_2010 <- get_acs(geo = "tract", table = "B01003", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE)

p <- ggplot(population_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "magma") +
  ggtitle('Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
write.xlsx(as.data.frame(population_2010), file = '~/Desktop/Population_2010.xlsx', sheetName = "Population_2010", 
           col.names = TRUE, row.names = FALSE, append = FALSE)
-------------------------------------------------------------------------------------------------
# year of 2018
population_2018 <- get_acs(geo  = "tract", table = "B01003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE)

p <- ggplot(population_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "magma") +
  ggtitle('Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
  
--------------------------------------------------------------------------------------
#2 Male Population
# year of 2010
male_population_2010 <- get_acs(geo = "tract", table = "B01001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B01001_002')
p <- ggplot(male_population_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Male Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------
# year of 2018
male_population_2018 <- get_acs(geo = "tract", table = "B01001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B01001_002')
p <- ggplot(male_population_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Male Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))

---------------------------------------------------------------------------------------
#3 Female Population
# year of 2010
female_population_2010 <- get_acs(geo = "tract", table = "B01001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B01001_026')
p <- ggplot(female_population_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Female Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------
# year of 2018
female_population_2018 <- get_acs(geo = "tract", table = "B01001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B01001_026')
p <- ggplot(female_population_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Female Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------
# Disability population (only available starting 2012)
# year of 2010
vars2012 <- load_variables(2012, "acs5", cache = TRUE)

pop_disability_18_1_2012 <-  get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'C18108_003')
pop_disability_18_2_2012 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_004')
pop_disability_18_64_1_2012 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_007')
pop_disability_18_64_2_2012 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_008')
pop_disability_64_1_2012 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_011')
pop_disability_64_2_2012 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_012')

pop_disability_2012 <-  pop_disability_18_1_2012 %>%
  rename(estimate_18_1_2012 = estimate) %>%
  right_join(pop_disability_18_2_2012 %>%select("GEOID", estimate_18_2_2012 = estimate), by = "GEOID") %>%
  right_join(pop_disability_18_64_1_2012 %>%select("GEOID", estimate_18_64_1_2012 = estimate), by = "GEOID") %>%
  right_join(pop_disability_18_64_2_2012 %>%select("GEOID", estimate_18_64_2_2012 = estimate), by = "GEOID") %>%
  right_join(pop_disability_64_1_2012 %>%select("GEOID", estimate_64_1_2012 = estimate), by = "GEOID") %>%
  right_join(pop_disability_64_2_2012 %>%select("GEOID", estimate_64_2_2012 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate_18_1_2012 + estimate_18_2_2012 + estimate_18_64_1_2012 + estimate_18_64_2_2012 + estimate_64_1_2012 + estimate_64_2_2012) %>%
  select(-moe, -estimate_18_1_2012, -estimate_18_2_2012, -estimate_18_64_1_2012, -estimate_18_64_2_2012, -estimate_64_1_2012, -estimate_64_2_2012)

p <- ggplot(pop_disability_2012, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Disability Population (2012 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-------------------------------------------------------------------------------------------------
# year of 2018

pop_disability_18_1_2018 <-  get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'C18108_003')
pop_disability_18_2_2018 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_004')
pop_disability_18_64_1_2018 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_007')
pop_disability_18_64_2_2018 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_008')
pop_disability_64_1_2018 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_011')
pop_disability_64_2_2018 <- get_acs(geo = "tract", table = "C18108", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'C18108_012')

pop_disability_2018 <-  pop_disability_18_1_2018 %>%
  dplyr::rename(estimate_18_1_2018 = estimate) %>%
  right_join(pop_disability_18_2_2018 %>%select("GEOID", estimate_18_2_2018 = estimate), by = "GEOID") %>%
  right_join(pop_disability_18_64_1_2018 %>%select("GEOID", estimate_18_64_1_2018 = estimate), by = "GEOID") %>%
  right_join(pop_disability_18_64_2_2018 %>%select("GEOID", estimate_18_64_2_2018 = estimate), by = "GEOID") %>%
  right_join(pop_disability_64_1_2018 %>%select("GEOID", estimate_64_1_2018 = estimate), by = "GEOID") %>%
  right_join(pop_disability_64_2_2018 %>%select("GEOID", estimate_64_2_2018 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate_18_1_2018 + estimate_18_2_2018 + estimate_18_64_1_2018 + estimate_18_64_2_2018 + estimate_64_1_2018 + estimate_64_2_2018) %>%
  select(-moe, -estimate_18_1_2018, -estimate_18_2_2018, -estimate_18_64_1_2018, -estimate_18_64_2_2018, -estimate_64_1_2018, -estimate_64_2_2018)

p <- ggplot(pop_disability_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Disability Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------------
#4 African American Population
# year of 2010
african_american_2010 <- get_acs(geo = "tract", table = "B02001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B02001_003")
p <- ggplot(african_american_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('African American Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-------------------------------------------------------------------------------------------
# year of 2018
african_american_2018 <- get_acs(geo = "tract", table = "B02001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B02001_003")
p <- ggplot(african_american_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('African American Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------------
#5 Hispanic or Latino Population
# year of 2010
hispanic_2010 <- get_acs(geo = "tract", table = "B03003", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B03003_003")
p <- ggplot(hispanic_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Hispanic or Latino Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------------
#year of 2018
hispanic_2018 <- get_acs(geo = "tract", table = "B03003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B03003_003")
p <- ggplot(hispanic_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Hispanic or Latino Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------------
#6 White Only Population
# year of 2010
white_only_2010 <- get_acs(geo = "tract", table = "B02001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B02001_002")
p <- ggplot(white_only_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('White Population (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
----------------------------------------------------------------------------------------------
# year of 2018
white_only_2018 <- get_acs(geo = "tract", table = "B02001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == "B02001_002")
p <- ggplot(white_only_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('White Population (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))  


## ------------------------Family Composition--------------------------------------
#1 Family own children under 18
# year of 2010
family_own_children_2010 <- get_acs(geo = "tract", table = "B09002", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B09002_001') 
p <- ggplot(family_own_children_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Family with children under 18 (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-----------------------------------------------------------------------------------
# year of 2018
family_own_children_2018 <- get_acs(geo = "tract", table = "B09002", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B09002_001') 
p <- ggplot(family_own_children_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Family with children under 18 (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
  
------------------------------------------------------------------------------------
# Total household
# year of 2010
total_household_2010 <- get_acs(geo = "tract", table = "B11001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11001_001') 
p <- ggplot(total_household_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Total household (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-------------------------------------------------------------------------------------
# year of 2018
total_household_2018 <- get_acs(geo = "tract", table = "B11001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11001_001') 
p <- ggplot(total_household_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Total household (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-------------------------------------------------------------------------------------
# Total occupied housing units
# year of 2010
total_housing_units_2010 <- get_acs(geo = "tract", table = "B25003", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B25003_001') 
p <- ggplot(total_housing_units_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Total occupied housing units (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------
# year of 2018
total_housing_units_2018 <- get_acs(geo = "tract", table = "B25003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B25003_001') 
p <- ggplot(total_housing_units_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Total occupied housing units (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------  
#2 Single parent family
# year of 2010
# single parent family with children aged under 6
single_parent_family_under_6_2010 <- get_acs(geo = "tract", table = "B23008", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B23008_008') 

# single parent family with children aged between 6 and 17
single_parent_family_6_17_2010 <- get_acs(geo = "tract", table = "B23008", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B23008_021')

single_parent_family_2010 <- single_parent_family_under_6_2010 %>%
  right_join(single_parent_family_6_17_2010 %>% select(GEOID, estimate_6_17_2010 = estimate), by = 'GEOID') %>%
 mutate(estimate = estimate + estimate_6_17_2010) %>%
  select(-variable, -moe, -estimate_6_17_2010)

p <- ggplot(single_parent_family_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Single parent family (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------
# year of 2018
single_parent_family_under_6_2018 <- get_acs(geo = "tract", table = "B23008", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B23008_008') 

# single parent family with children aged between 6 and 17
single_parent_family_6_17_2018 <- get_acs(geo = "tract", table = "B23008", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B23008_021')

single_parent_family_2018 <- single_parent_family_under_6_2018 %>%
  right_join(single_parent_family_6_17_2018 %>% select(GEOID, estimate_6_17_2018 = estimate), by = 'GEOID') %>%
  mutate(estimate = estimate + estimate_6_17_2018) %>%
  select(-variable, -moe, -estimate_6_17_2018)

p <- ggplot(single_parent_family_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Single parent family (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5)) 
---------------------------------------------------------------------------------------
# Single person household
# year of 2010
single_person_household_2010 <- get_acs(geo = "tract", table = "B11001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11001_007') 

p <- ggplot(single_person_household_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Single person household (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
----------------------------------------------------------------------------------------
# year of 2018
single_person_household_2018 <- get_acs(geo = "tract", table = "B11001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11001_007') 

p <- ggplot(single_person_household_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Single person household (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
---------------------------------------------------------------------------------------
# Multifamily housing
# year of 2010
multi_family_owner_2_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B25032_005') 

multi_family_owner_3_4_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_006') 

multi_family_owner_5_9_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_007') 

multi_family_owner_10_19_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_008') 

multi_family_owner_20_49_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_009')

multi_family_owner_50_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_010')

multi_family_renter_2_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_016') 

multi_family_renter_3_4_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_017') 

multi_family_renter_5_9_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_018') 

multi_family_renter_10_19_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_019') 

multi_family_renter_20_49_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_020')

multi_family_renter_50_2010 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_021')

multi_family_housing_2010 <- multi_family_owner_2_2010 %>%
  right_join(multi_family_owner_3_4_2010 %>% select("GEOID", estimate_owner_3_4_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_5_9_2010 %>% select("GEOID", estimate_owner_5_9_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_10_19_2010 %>% select("GEOID", estimate_owner_10_19_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_20_49_2010 %>% select("GEOID", estimate_owner_20_49_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_50_2010 %>% select("GEOID", estimate_owner_50_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_2_2010 %>% select("GEOID", estimate_renter_2_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_3_4_2010 %>% select("GEOID", estimate_renter_3_4_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_5_9_2010 %>% select("GEOID", estimate_renter_5_9_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_10_19_2010 %>% select("GEOID", estimate_renter_10_19_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_20_49_2010 %>% select("GEOID", estimate_renter_20_49_2010 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_50_2010 %>% select("GEOID", estimate_renter_50_2010 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate + estimate_owner_3_4_2010 + estimate_owner_5_9_2010 + estimate_owner_10_19_2010 + estimate_owner_20_49_2010 + estimate_owner_50_2010 + estimate_renter_2_2010 + estimate_renter_3_4_2010 + estimate_renter_5_9_2010 + estimate_renter_10_19_2010 + estimate_renter_20_49_2010 + estimate_renter_50_2010) %>%
  select(-moe, -estimate_owner_3_4_2010, -estimate_owner_5_9_2010, -estimate_owner_10_19_2010, -estimate_owner_20_49_2010, -estimate_owner_50_2010, -estimate_renter_2_2010, -estimate_renter_3_4_2010, -estimate_renter_5_9_2010, -estimate_renter_10_19_2010, -estimate_renter_20_49_2010, -estimate_renter_50_2010)

p <- ggplot(multi_family_housing_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Multifamily housing (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------------------
# year of 2018
multi_family_owner_2_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B25032_005') 

multi_family_owner_3_4_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_006') 

multi_family_owner_5_9_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_007') 

multi_family_owner_10_19_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_008') 

multi_family_owner_20_49_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_009')

multi_family_owner_50_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_010')

multi_family_renter_2_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_016') 

multi_family_renter_3_4_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_017') 

multi_family_renter_5_9_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_018') 

multi_family_renter_10_19_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_019') 

multi_family_renter_20_49_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_020')

multi_family_renter_50_2018 <- get_acs(geo = "tract", table = "B25032", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B25032_021')

multi_family_housing_2018 <- multi_family_owner_2_2018 %>%
  right_join(multi_family_owner_3_4_2018 %>% select("GEOID", estimate_owner_3_4_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_5_9_2018 %>% select("GEOID", estimate_owner_5_9_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_10_19_2018 %>% select("GEOID", estimate_owner_10_19_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_20_49_2018 %>% select("GEOID", estimate_owner_20_49_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_owner_50_2018 %>% select("GEOID", estimate_owner_50_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_2_2018 %>% select("GEOID", estimate_renter_2_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_3_4_2018 %>% select("GEOID", estimate_renter_3_4_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_5_9_2018 %>% select("GEOID", estimate_renter_5_9_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_10_19_2018 %>% select("GEOID", estimate_renter_10_19_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_20_49_2018 %>% select("GEOID", estimate_renter_20_49_2018 = estimate), by = "GEOID") %>%
  right_join(multi_family_renter_50_2018 %>% select("GEOID", estimate_renter_50_2018 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate + estimate_owner_3_4_2018 + estimate_owner_5_9_2018 + estimate_owner_10_19_2018 + estimate_owner_20_49_2018 + estimate_owner_50_2018 + estimate_renter_2_2018 + estimate_renter_3_4_2018 + estimate_renter_5_9_2018 + estimate_renter_10_19_2018 + estimate_renter_20_49_2018 + estimate_renter_50_2018) %>%
  select(-moe, -estimate_owner_3_4_2018, -estimate_owner_5_9_2018, -estimate_owner_10_19_2018, -estimate_owner_20_49_2018, -estimate_owner_50_2018, -estimate_renter_2_2018, -estimate_renter_3_4_2018, -estimate_renter_5_9_2018, -estimate_renter_10_19_2018, -estimate_renter_20_49_2018, -estimate_renter_50_2018)

p <- ggplot(multi_family_housing_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Multifamily housing (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
  
---------------------------------------------------------------------------------------------------
#3 Households caring for an elderly/senior parent/grandparent
# year of 2010
household_senior_2010 <- get_acs(geo = "tract", table = "B11006", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11006_002')

p <- ggplot(household_senior_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Households with senior people (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
--------------------------------------------------------------------------------------------------
# year of 2018
household_senior_2018 <- get_acs(geo = "tract", table = "B11006", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B11006_002')

p <- ggplot(household_senior_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Households with senior people (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
------------------------------------------------------------------------------------------------------
# Poverty rate
# year of 2010
poverty_2010 <- get_acs(geo = "tract", table = "B17001", state = "TN", county = "Davidson", year = 2010, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B17001_002')

p <- ggplot(poverty_2010, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Population under poverty level (2010 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
------------------------------------------------------------------------------------------------------
# year of 2018
poverty_2018 <- get_acs(geo = "tract", table = "B17001", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B17001_002')

p <- ggplot(poverty_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Population under poverty level (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
-------------------------------------------------------------------------------------------------------
# Limited Educational Attainment (only available starting from 2012)
# year of 2010
limit_education_attain_no_school_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B15003_002')
limit_education_attain_nursery_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_003')
limit_education_attain_kindergarten_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_004')
limit_education_attain_grade_1_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_005')
limit_education_attain_grade_2_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_006')
limit_education_attain_grade_3_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_007')
limit_education_attain_grade_4_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_008')
limit_education_attain_grade_5_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_009')
limit_education_attain_grade_6_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_010')
limit_education_attain_grade_7_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_011')
limit_education_attain_grade_8_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_012')
limit_education_attain_grade_9_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_013')
limit_education_attain_grade_10_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_014')
limit_education_attain_grade_11_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_015')
limit_education_attain_grade_12_2012 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2012, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_016')
  
limit_education_attain_2012 <- limit_education_attain_no_school_2012 %>% rename(estimate_no_school_2012 = estimate) %>%
  right_join(limit_education_attain_nursery_2012 %>% select("GEOID", estimate_nursery_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_kindergarten_2012 %>% select("GEOID", estimate_kindergarten_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_1_2012 %>% select("GEOID", estimate_grade_1_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_2_2012 %>% select("GEOID", estimate_grade_2_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_3_2012 %>% select("GEOID", estimate_grade_3_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_4_2012 %>% select("GEOID", estimate_grade_4_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_5_2012 %>% select("GEOID", estimate_grade_5_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_6_2012 %>% select("GEOID", estimate_grade_6_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_7_2012 %>% select("GEOID", estimate_grade_7_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_8_2012 %>% select("GEOID", estimate_grade_8_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_9_2012 %>% select("GEOID", estimate_grade_9_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_10_2012 %>% select("GEOID", estimate_grade_10_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_11_2012 %>% select("GEOID", estimate_grade_11_2012 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_12_2012 %>% select("GEOID", estimate_grade_12_2012 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate_no_school_2012 + estimate_nursery_2012 + estimate_kindergarten_2012 + estimate_grade_1_2012 + estimate_grade_2_2012 + estimate_grade_3_2012 + estimate_grade_4_2012 + estimate_grade_5_2012 + estimate_grade_6_2012 + estimate_grade_7_2012 + estimate_grade_8_2012 + estimate_grade_9_2012 + estimate_grade_10_2012 + estimate_grade_11_2012 + estimate_grade_12_2012) %>%
  select(-moe, -estimate_no_school_2012, -estimate_nursery_2012, -estimate_kindergarten_2012, -estimate_grade_1_2012, -estimate_grade_2_2012, -estimate_grade_3_2012, -estimate_grade_4_2012, -estimate_grade_5_2012, -estimate_grade_6_2012, -estimate_grade_7_2012, -estimate_grade_8_2012, -estimate_grade_9_2012, -estimate_grade_10_2012, -estimate_grade_11_2012, -estimate_grade_12_2012)

p <- ggplot(limit_education_attain_2012, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Limited Educational Attainment (2012 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
----------------------------------------------------------------------------------------------------------
# year of 2018
limit_education_attain_no_school_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = TRUE) %>%
  filter(variable == 'B15003_002')
limit_education_attain_nursery_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_003')
limit_education_attain_kindergarten_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_004')
limit_education_attain_grade_1_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_005')
limit_education_attain_grade_2_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_006')
limit_education_attain_grade_3_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_007')
limit_education_attain_grade_4_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_008')
limit_education_attain_grade_5_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_009')
limit_education_attain_grade_6_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_010')
limit_education_attain_grade_7_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_011')
limit_education_attain_grade_8_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_012')
limit_education_attain_grade_9_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_013')
limit_education_attain_grade_10_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_014')
limit_education_attain_grade_11_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_015')
limit_education_attain_grade_12_2018 <- get_acs(geo = "tract", table = "B15003", state = "TN", county = "Davidson", year = 2018, cache_table = TRUE, geometry = FALSE) %>%
  filter(variable == 'B15003_016')

limit_education_attain_2018 <- limit_education_attain_no_school_2018 %>% dplyr::rename(estimate_no_school_2018 = estimate) %>%
  right_join(limit_education_attain_nursery_2018 %>% select("GEOID", estimate_nursery_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_kindergarten_2018 %>% select("GEOID", estimate_kindergarten_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_1_2018 %>% select("GEOID", estimate_grade_1_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_2_2018 %>% select("GEOID", estimate_grade_2_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_3_2018 %>% select("GEOID", estimate_grade_3_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_4_2018 %>% select("GEOID", estimate_grade_4_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_5_2018 %>% select("GEOID", estimate_grade_5_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_6_2018 %>% select("GEOID", estimate_grade_6_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_7_2018 %>% select("GEOID", estimate_grade_7_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_8_2018 %>% select("GEOID", estimate_grade_8_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_9_2018 %>% select("GEOID", estimate_grade_9_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_10_2018 %>% select("GEOID", estimate_grade_10_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_11_2018 %>% select("GEOID", estimate_grade_11_2018 = estimate), by = "GEOID") %>%
  right_join(limit_education_attain_grade_12_2018 %>% select("GEOID", estimate_grade_12_2018 = estimate), by = "GEOID") %>%
  mutate(estimate = estimate_no_school_2018 + estimate_nursery_2018 + estimate_kindergarten_2018 + estimate_grade_1_2018 + estimate_grade_2_2018 + estimate_grade_3_2018 + estimate_grade_4_2018 + estimate_grade_5_2018 + estimate_grade_6_2018 + estimate_grade_7_2018 + estimate_grade_8_2018 + estimate_grade_9_2018 + estimate_grade_10_2018 + estimate_grade_11_2018 + estimate_grade_12_2018) %>%
  select(-moe, -estimate_no_school_2018, -estimate_nursery_2018, -estimate_kindergarten_2018, -estimate_grade_1_2018, -estimate_grade_2_2018, -estimate_grade_3_2018, -estimate_grade_4_2018, -estimate_grade_5_2018, -estimate_grade_6_2018, -estimate_grade_7_2018, -estimate_grade_8_2018, -estimate_grade_9_2018, -estimate_grade_10_2018, -estimate_grade_11_2018, -estimate_grade_12_2018)

p <- ggplot(limit_education_attain_2018, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "B") +
  ggtitle('Limited Educational Attainment (2018 census tract)')
p + theme(plot.title = element_text(color="black", size=20, face="bold", hjust = 0.5))
  

#------------------------------------------write data-----------------------------------------------------------------
# year of 2010
  write_rds(list(population_2010 = population_2010, male_population_2010 = male_population_2010,
                 female_population_2010 = female_population_2010, pop_disability_2012 = pop_disability_2012,
                 african_american_2010 = african_american_2010, hispanic_2010 = hispanic_2010,
                 white_only_2010 = white_only_2010, family_own_children_2010 = family_own_children_2010,
                 total_household_2010 = total_household_2010, single_parent_family_2010 = single_parent_family_2010,
                 total_housing_units_2010 = total_housing_units_2010, single_person_household_2010 = single_person_household_2010, 
                 multi_family_housing_2010 = multi_family_housing_2010, household_senior_2010 = household_senior_2010, 
                 poverty_2010 = poverty_2010, limit_education_attain_2012 = limit_education_attain_2012),
            path = file.path(data_dir ="/Users/hebowen/Desktop/MarTREC-project/Data", "ACS_census_tract_2010.Rds"))
---------------------------------------------------------------------------------------------------------------------
# year of 2018
  write_rds(list(population_2018 = population_2018, male_population_2018 = male_population_2018,
                 female_population_2018 = female_population_2018, pop_disability_2018 = pop_disability_2018,
                 african_american_2018 = african_american_2018, hispanic_2018 = hispanic_2018,
                 white_only_2018 = white_only_2018, family_own_children_2018 = family_own_children_2018,
                 total_household_2018 = total_household_2018, single_parent_family_2018 = single_parent_family_2018,
                 total_housing_units_2018 = total_housing_units_2018, single_person_household_2018 = single_person_household_2018, 
                 multi_family_housing_2018 = multi_family_housing_2018, household_senior_2018 = household_senior_2018, 
                 poverty_2018 = poverty_2018, limit_education_attain_2018 = limit_education_attain_2018),
            path = file.path(data_dir ="/Users/hebowen/Desktop/MarTREC-project/Data", "ACS_census_tract_2018.Rds"))
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
###Set census call variable years and geography ("tract" or "block group")
year <- 2016
geo = "tract"
State = "TN"
County = "Davidson"
#Median Household Income
acs_data <- get_acs(geography = geo, table = "B19013", state = State, county = County,
                    year = year, cache_table = TRUE) %>%
  mutate(year = !!year)

head(vars2010)


population <- get_acs(geo = "tract", table = "B00001", state = "TN", county = "Davidson", year = 2016, cache_table = TRUE, geometry = TRUE)
sex_by_age <- get_acs(geo = "tract", table = "B01001", state = "TN", county = "Davidson", year = 2016, cache_table = TRUE, geometry = TRUE)

tm_shape(population) + tm_polygons("estimate")
ggplot(population, aes(fill = estimate)) + geom_sf(color = NA) + scale_fill_viridis_c(option = "magma")

head(sex_by_age)

sex_by_age %>% left_join(vars2010, by = c("variable" = "name"))