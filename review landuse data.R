# review options for landuse data, to be used for finding landuse of favourite spots

library(dplyr)
library(sf)
library(fs)  # for dir_walk
dir_walk(path="./functions/",source, recurse=T, type = "file")


# vicmap landuse 2016-17, downloaded from https://datashare.maps.vic.gov.au/
# however, note that this file has not been retained in data
LU <- read_zipped_GIS(zipfile = "./data/vicmap_landuse_2016_17.zip",
                    subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/CATCHMENTS")


descriptions <- LU %>%
  st_drop_geometry() %>%
  group_by(LU_DESC, LU_DESCR_A) %>%
  summarise(n = n())
# table of 1136 combinations

LU_DESC <- LU$LU_DESC %>% unique() %>% sort()  # 311 types
LU_DESCR_A <- LU$LU_DESCR_A %>% unique %>% sort() #123 types
# but not useful, because most of the inner and middle suburbs, including
# entire LGAs, are simply 'Built Up Area/Urban residential'


# vicmap property, downloaded from  https://datashare.maps.vic.gov.au/
# see https://www.land.vic.gov.au/__data/assets/pdf_file/0037/494578/Vicmap-Property-Product-Description-7.2.1.pdf 
# for field descriptions; most of them are clearly no use, but check 'Further Description' in 'PARCEL'
# however, note that this file has not been retained in data
VP <- read_zipped_GIS(zipfile = "./data/vicmap_property.zip",
                      subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/VMPROP",
                      file = "/PARCEL.shp")


VP.further.desc <- VP %>%
  st_drop_geometry() %>%
  group_by(FUR_DESC) %>%
  summarise(n = n())


# meshblocks, downloaded from https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files 
MB <- read_zipped_GIS(zipfile = "./data/MB_2021_AUST_SHP_GDA2020.zip")

MB.categories <- MB$MB_CAT21 %>% unique() %>% sort()

