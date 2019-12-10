# ####################################### #
# Import and clean / merge original files #
# ####################################### #

library(sf)
library(mapview)
library(tmap)
library(assertr)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)

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
  

census1851_occp <- bind_rows(
  read_tsv(
    file.path(
      Sys.getenv('DATADIR_HIVA_LOCAL'), 
      'ukda_occuphist_1851/ukda_5433_1851_census_report_registration_district_occupational_data/tab/1851_cen_occu_rd_eng_1.tab'),
    col_types = cols(
      regcounty = col_character(),
      rdname = col_character(),
      rd_id = col_double(),
      sex = col_character(),
      occ = col_character(),
      n = col_double() )),
  
  read_tsv(file.path(
    Sys.getenv('DATADIR_HIVA_LOCAL'), 
    'ukda_occuphist_1851/ukda_5433_1851_census_report_registration_district_occupational_data/tab/1851_cen_occu_rd_eng_2.tab'),
    col_types = cols(
      regcounty = col_character(),
      rdname = col_character(),
      rd_id = col_double(),
      sex = col_character(),
      occ = col_character(),
      n = col_double() )),
  
  read_tsv(file.path(
    Sys.getenv('DATADIR_HIVA_LOCAL'), 
    'ukda_occuphist_1851/ukda_5433_1851_census_report_registration_district_occupational_data/tab/1851_cen_occu_rd_eng_3.tab'),
    col_types = cols(
      regcounty = col_character(),
      rdname = col_character(),
      rd_id = col_double(),
      sex = col_character(),
      occ = col_character(),
      n = col_double() ))
)

census1851_occp <- census1851_occp %>%
  filter(sex != 'sex')

census1851_occup_mapping <- read_tsv(file.path(
  Sys.getenv('DATADIR_HIVA_LOCAL'), 
  'ukda_occuphist_1851/ukda_5434_1851_pst_occ_code/tab/pst_occ_code_1851.tab'),
  col_types = cols(
    `Original occupation` = col_character(),
    PST1 = col_character(),
    pst1a = col_double(),
    pst1b = col_double(),
    pst1c = col_double(),
    pst1d = col_double(),
    PST2 = col_character(),
    pst2a = col_double(),
    pst2b = col_double(),
    pst2c = col_double(),
    pst2d = col_double(),
    PST3 = col_character(),
    pst3a = col_double(),
    pst3b = col_double(),
    pst3c = col_double(),
    pst3d = col_double() )) %>%
  verify(dim(.) == c(458, 16)) %>%
  clean_names() %>%
  mutate(original_occupation = str_to_lower(original_occupation))

census1851_occp <- census1851_occp %>%
  mutate(occ = str_to_lower(occ))

census1851_occp <- census1851_occp %>%
  left_join(
    census1851_occup_mapping %>% select(original_occupation, pst1a, pst1b, pst1c, pst1d),
    by = c('occ' = 'original_occupation'))

# census1851_occp <- census1851_occp %>%
#   mutate(rd_id = as.character(rd_id)) %>%
#   left_join(census1851_pop %>% select(rd_id, m1851, f1851),
#             by = 'rd_id') %>%
#   rename(pop_county_male = m1851, pop_county_female = f1851) %>%
#   mutate(pop_county_total = pop_county_male + pop_county_female)

census1851_occp %>%
  filter(occ == 'baker') %>%
  group_by(regcounty) %>%
  summarise(pop_county_total)


census1851_occp 

census1851_occp %>%
  group_by(regcounty) %>%
  tally(n)





%>%
  filter(pst1a == 2) %>%
  tally(n)







census1851_occp %>%
  filter(sex == 'sex')

census1851_occp %>%
  group_by(sex) %>%
  tally(n)

census1851_occp %>%
  group_by(pst1a) %>%
  tally(n)

census1851_occp %>%
  group_by(pst1a) %>%
  tally(n)

census1851_occp %>%
  group_by(sex, pst1a) %>%
  tally(n) %>%
  spread(sex, n)

census1851_occp %>%
  group_by(sex, pst1a) %>%
  tally(n) %>%
  spread(sex, n) %>%
  mutate(pct_women = F / ( F + M) )

census1851_occp %>%
  #filter(str_detect(occ, 'baker')) %>%
  filter(occ == 'baker') %>%
  group_by(regcounty) %>%
  tally(n) %>%
  arrange(desc(n))



census1851_occp %>%
  filter(!(occ %in% census1851_occup_mapping$original_occupation))
  # filter(occ %in% census1851_occup_mapping$original_occupation) %>%
  tally(n)

table(census1851_occp$occ %in% census1851_occup_mapping$original_occupation)

census1851_districts <- census1851_districts %>%
  left_join(census1851_pop, by = c('cen1' = 'rd_id'))

census1851_districts <- census1851_districts %>%
  mutate(t1851 = m1851 + f1851)

census1851_districts %>%
  filter(r_dist == 'LUTON')

mapview(census1851_districts)



qtm(census1851_districts, fill = 't1851')

census1851_counties <- census1851_districts %>%
  group_by(r_cty) %>%
  summarise()

mapview(census1851_districts)
mapview(census1851_counties)

qtm(census1851_districts %>%
  group_by(r_cty) %>%
  summarise())

tm_shape(census1851_counties) +
  tm_borders(col = 'grey') +
  tm_shape(raillines_1851) +
  tm_lines(col = 'blue')

tm_shape(census1851_counties) +
  tm_borders(col = 'black') +
  tm_shape(census1851_districts) +
  tm_borders(col = 'grey') +
  tm_shape(raillines_1851) +
  tm_lines(col = 'blue')



census1851_districts %>%
  filter(cen1 == 181)

mapview(raillines_1851)

qtm(raillines_1851)

# percentage of secondary sector employment

district_pct_secondary <- census1851_occp %>%
  group_by(rd_id, rdname, pst1a) %>%
  tally(n) %>%
  spread(pst1a, n) %>%
  mutate(total = `1` + `2` + `3` + `4` + `5` + `90` + `99`) %>%
  mutate(pct_secondary = `2` / total)

d <- census1851_districts %>%
  mutate(cen1 = as.numeric(as.character(cen1))) %>%
  left_join(district_pct_secondary, by = c('cen1' = 'rd_id'))

d %>%
  group_by(r_ctry) %>%
  tally()

d %>% filter(r_ctry != 'WALES')

qtm(d, fill = 'pct_secondary')

tm_shape(d %>% filter(r_ctry != 'WALES')) +
  tm_borders(col = 'white') +
  tm_fill(col = 'pct_secondary') +
  tm_shape(raillines_1851[d,]) +
  tm_lines(col = 'black')




raillines_1851 %>%
  filter(st_intersects(x = ., y = d, sparse = FALSE))



st_overlaps(raillines_1851, d)

+
  tm_shape(census1851_counties) +
  tm_borders(col = 'grey')
