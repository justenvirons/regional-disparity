## Script name: disparity_region_analysis_dataprep.R
## Purpose of script: prepare PLACES, TIGER, ACS data for hot spot analysis
## Author: C. Scott Smith, PhD AICP
## Date Created: 2021-05-14
## Date Last Updated: 2021-6-14
## Email: christopher.smith@cookcountyhealth.org

# Specify packages, options --------------------------------------------------------

library(dplyr)
library(data.table)
library(tidyverse)
library(tigris)
library(censusapi)
library(sf)
library(readxl)
library(ggmap)
library(units)

# Download data ---------------------------------------------------------------

## PLACES ---------------------------------------------------------------
# Download and write PLACES data for tracts, places, counties
places_tracts <- read_csv("https://chronicdata.cdc.gov/api/views/yjkw-uj5s/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
places_places <- read_csv("https://chronicdata.cdc.gov/api/views/vgc8-iyc4/rows.csv?accessType=DOWNLOAD&bom=true&format=true")
places_counties <- read_csv("https://chronicdata.cdc.gov/api/views/i46a-9kgh/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# Write csvs to working directory
write_csv(places_tracts, "data/places_tracts.csv")
write_csv(places_places, "data/places_places.csv")
write_csv(places_counties, "data/places_counties.csv")

## TIGER/Line ---------------------------------------------------------------
# Download  census tract geographies
# cb = cartographic boundary
# Project both to UTM North Zone 16
# downloads geographies for IL

IN_Tracts_geom <- tracts("IN", cb=TRUE, class="sf")
IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")
INIL_Tracts_geom <- IN_Tracts_geom %>% bind_rows(IL_Tracts_geom)
INIL_Places_geom <- places(c("IN","IL"), cb=TRUE, class="sf")
INIL_Counties_geom <- counties(c("IN","IL"), cb=TRUE, class="sf")
INIL_Tracts_geom <- st_transform(INIL_Tracts_geom, crs = 26916)
INIL_Places_geom <- st_transform(INIL_Places_geom, crs = 26916)
INIL_Counties_geom <- st_transform(INIL_Counties_geom, crs = 26916)

# write PLACES shapefiles to working directory
st_write(INIL_Counties_geom,"layers/INIL_Counties_geom.shp", append = FALSE)
st_write(INIL_Places_geom,"layers/INIL_Places_geom.shp", append = FALSE)
st_write(INIL_Tracts_geom,"layers/INIL_Tracts_geom.shp", append = FALSE)
st_write(CCDPH_geom,"layers/CCDPH_geom.shp", append = FALSE)

## ACS 2015-2019 ---------------------------------------------------------------
# download ACS data
grouplist <- c("B01001","B17001","B03002")

# Tables
# B01001: SEX BY AGE
# B17001: POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
# B03002: HISPANIC OR LATINO ORIGIN BY RACE

# By census tract
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    print(paste0("importing ",ayear," data for table ",agroup))
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract:*", # tracts
                           regionin="state:17,18", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    acs_group <- acs_group %>% 
      select(-contains(c("EA","MA","NAME_1","GEO_ID","M_1"))) %>%
      mutate(year=ayear,
             GEOID=paste0(state,county,tract))
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
  }
}

# Transform, subset tables
# B01001: SEX BY AGE
B01001_2019_sub <- B01001_2019 %>% 
  rowwise() %>%
  mutate(
    TotalPop = B01001_001E, 
    PopUnd5 = sum(B01001_003E,B01001_027E),
    Pop5to9 = sum(B01001_004E,B01001_028E),
    Pop10to14 = sum(B01001_005E,B01001_029E),
    Pop15to19 = sum(c_across(B01001_006E:B01001_007E),c_across(B01001_030E:B01001_031E)),
    Pop20to24 = sum(c_across(B01001_008E:B01001_010E),c_across(B01001_032E:B01001_034E)),
    Pop25to29 = sum(B01001_011E,B01001_035E),
    Pop30to34 = sum(B01001_012E,B01001_036E),
    Pop35to39 = sum(B01001_013E,B01001_037E),
    Pop40to44 = sum(B01001_014E,B01001_038E),
    Pop45to49 = sum(B01001_015E,B01001_039E),
    Pop50to54 = sum(B01001_016E,B01001_040E),
    Pop55to59 = sum(B01001_017E,B01001_041E),
    Pop60to64 = sum(c_across(B01001_018E:B01001_019E),c_across(B01001_042E:B01001_043E)),
    Pop65to69 = sum(c_across(B01001_020E:B01001_021E),c_across(B01001_044E:B01001_045E)),
    Pop70to74 = sum(B01001_022E,B01001_046E),
    Pop75to79 = sum(B01001_023E,B01001_047E),
    Pop80to84 = sum(B01001_024E,B01001_048E),
    Pop85Pl = sum(B01001_025E,B01001_049E),
    PopUnd12 = sum(Pop10to14*0.4, c_across(PopUnd5:Pop5to9)),
    Pop12to15 = sum(Pop10to14*0.6, Pop15to19*0.2),
    Pop12Pl = sum(Pop10to14*0.6, c_across(Pop15to19:Pop85Pl)),
    Pop16Pl = sum(B01001_006E*2/3,B01001_007E,c_across(Pop20to24:Pop85Pl)),
    Pop18Pl = sum(B01001_008E*0.5,c_across(Pop20to24:Pop85Pl))
  ) %>% 
  select(GEOID, TotalPop:Pop18Pl) %>%
  arrange(GEOID)

# B03002: HISPANIC OR LATINO ORIGIN BY RACE
B03002_2019_sub <- B03002_2019 %>%
  arrange(GEOID) %>%
  rowwise() %>%
  mutate(TotalRace = B03002_001E,
         PopBlk = B03002_004E,
         PopAsn = B03002_006E,
         PopLat = B03002_012E,
         PopClr = B03002_001E-B03002_003E,
         PopWht = B03002_003E) %>%
  select(GEOID, TotalRace:PopWht) %>%
  mutate_at(vars(PopBlk:PopWht), .funs = (list(pct = ~(./TotalRace*100))))

# POVERTY LEVEL INCOME
B17001_2019_sub <- B17001_2019 %>%
  arrange(GEOID)%>%
  mutate(TotPov_19 = B17001_001E,
         PopPov_19 = B17001_002E,
         PctPov_19 = B17001_002E/B17001_001E*100) %>%
  select(GEOID, TotPov_19:PctPov_19)

# bind processed tables
acs2019_bytract_df <- B01001_2019_sub %>%
  left_join(B03002_2019_sub, by = "GEOID") %>%
  left_join(B17001_2019_sub, by = "GEOID")

# write ACS csv tables to working directory
write_csv(acs2019_bytract_df, "data/acs2019_bytract.csv")
