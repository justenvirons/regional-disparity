cc_properties_type <- read_csv("https://datacatalog.cookcountyil.gov/api/views/bcnq-qi2z/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
cc_properties_address <- read_csv("https://datacatalog.cookcountyil.gov/api/views/c49d-89sn/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
cc_towns <- read_csv("https://datacatalog.cookcountyil.gov/api/views/wyzt-dzf8/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

cc_towns_unique <- cc_towns %>% group_by(`township_code`) %>% summarise(n())

cc_properties_type_address <- cc_properties_type %>%
  select(pin = PIN,
         age = Age,
         decade = `Age Decade`,
         town_neigh_code = `Town and Neighborhood`) %>%
  left_join(cc_properties_address, by="pin")

cc_properties_type_address_age <- cc_properties_type_address %>% group_by(property_city, decade) %>% 
  summarise(properties = n()) %>%
  mutate(years_old = decade*10,
         year_built = 2020-years_old,
         pre_1978 = if_else(year_built<1978,"YES","NO"),
         pre_1940 = if_else(year_built<1940,"YES","NO"))

cc_properties_type_address_age_sum <- cc_properties_type_address_age %>% filter(pre_1978=="YES") %>% group_by(property_city) %>% summarise(properties = sum(properties))
cc_properties_type_address_age_sum <- cc_properties_type_address_age %>% filter(pre_1940=="YES") %>% group_by(property_city) %>% summarise(properties = sum(properties))
cc_properties_type_address_age_sum <- cc_properties_type_address_age %>%  group_by(property_city) %>% summarise(properties = sum(properties))


