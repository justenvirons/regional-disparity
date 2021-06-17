# geocode partners
library("ggmap")

partners <- read_excel("data/partner_organizations_20210617.xlsx", sheet="PartnersAddresses")

register_google(key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
getOption("ggmap")
addresses_full <- partners %>%
  mutate(FullAddressFormatted = toupper(paste0(Street," ",City,", IL")))
addresses = addresses_full$FullAddressFormatted
length(addresses) # check number of addresses
locations <- geocode(addresses, key="AIzaSyCrN9Rd0lIz5I55yPdrDtGY4943dyKGm2s")
locations <- cbind(partners,locations)

coordinates(locations) = ~lon+lat
locations_sf = st_as_sf(locations)
st_crs(locations_sf) = 4326
locations_sf = st_transform(locations_sf, crs = 26916)

st_write(locations_sf,"layers/healthpartners_20210615.shp")
