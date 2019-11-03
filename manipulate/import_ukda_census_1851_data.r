# ####################################### #
# Import and clean / merge original files #
# ####################################### #

library(sf)
library(mapview)
library(tmap)
library(assertr)
library(readr)
library(janitor)

census1851_districts <- st_read(
  file.path(Sys.getenv('DATADIR_HIVA_LOCAL'), 'ukda_occuphist_1851/1851_england_wales_census_registration_districts/1851EngWalesRegistrationDistrict.shp')) %>%
  verify(dim(.) == c(1194, 7)) %>%
  clean_names()

raillines_1851 <- st_read(
  file.path(Sys.getenv('DATADIR_HIVA_LOCAL'), 'ukda_occuphist_1851/1851_england_wales_scotland_rail_lines/1851EngWalesScotRail_Lines.shp')) %>%
  verify(dim(.) == c(1431, 3)) %>%
  clean_names()

census1851_pop <- read_tsv(
  file.path(Sys.getenv('DATADIR_HIVA_LOCAL'), 'ukda_occuphist_1851/ukda_5433_1851_census_report_registration_district_occupational_data/tab/1851_cen_men_women_20_over_eng.tab'),
  skip = 1, # double header row
  col_types = cols(
    regcounty = col_character(),
    rdname = col_character(),
    `rd ID` = col_character(),
    m1851 = col_double(),
    f1851 = col_double() )) %>%
  clean_names() %>%
  verify(dim(.) == c(576, 5))
  
census1851_districts <- census1851_districts %>%
  left_join(census1851_pop, by = c('cen1' = 'rd_id'))

census1851_districts <- census1851_districts %>%
  mutate(t1851 = m1851 + f1851)

census1851_districts %>%
  filter(r_dist == 'LUTON')

mapview(census1851_districts)

qtm(census1851_districts, fill = 't1851')


mapview(raillines_1851)

qtm(raillines_1851)


pleiades_url <- 'http://atlantides.org/downloads/pleiades/kml/pleiades-latest.kmz'
download.file(pleiades_url, 'pleiades-latest.kmz')
pleiades <- st_read('pleiades-latest.kmz')

ew = st_read(here::here('data/england_wales_1851/1851EngWalesParishandPlace.shp'))

mapview(ew %>% slice(1:500))

mapview(pleiades %>%
  slice(1:42, 44:50))



qtm(pleiades[0:10])

st_crs(pleiades) = NA
mapview(pleiades)

u_kmz <- "http://coagisweb.cabq.gov/datadownload/BikePaths.kmz"
download.file(u_kmz, "BikePaths.kmz")
bikraces <- st_read("bikeraces.kml")