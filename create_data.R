# create dataset of unemployment rates

require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
require(blscrapeR)
require(lubridate)
require(tigris)
require(sf)
require(areal)

# load county-level unemployment & other data
countun <- read_delim('data/la_county.txt',delim="\t")

countun <- mutate(countun,series_id=trimws(series_id)) %>%
  filter(period!="M13")

saveRDS(countun,'data/countun.rds')


# # load series indicators
id_data <- read_tsv('data/la_series.txt')
#
county_series <- filter(id_data,measure_code %in% c("04","06"),
                        area_type_code=="F") %>%
  mutate(series_id=trimws(series_id))
#
# #merge in unemployment data at series level
#
county_series <- left_join(county_series,countun, by="series_id")
#
# # need to split to create separate columns for unemp/labor force
#
county_series <- county_series %>%
  mutate(measure_code=recode(measure_code,`04`="unemp",
                             `06`="labor_force"),
         value=as.numeric(value)) %>%
  select(-series_id,-matches('footnote'),-seasonal,-srd_code,-series_title,-area_type_code) %>%
  spread(key = "measure_code",value="value")


# need FIPS codes

bls_fips <- get_bls_county() %>%
  select(-period,
         -labor_force,
         -unemployed,
         -employed,
         -unemployed_rate) %>%
  distinct

saveRDS(bls_fips,'data/bls_fips.rds')

bls_fips <- readRDS('data/bls_fips.rds')
#
# # merge in FIPS codes and drop unnecessary data
#
county_series <- select(county_series,-begin_year,-begin_period,-end_year,
                        -end_period) %>%
  left_join(bls_fips,by="area_code")
#
# # check unemployment rates over time
#
county_series <- county_series %>%
  filter(period!="M13") %>%
  mutate(period=recode(period,M01="January",
                           M02="February",
                           M03="March",
                           M04="April",
                           M05="May",
                           M06="June",
                           M07="July",
                           M08="August",
                           M09="September",
                           M10="October",
                           M11="November",
                           M12="December"),
         date_recode=paste0(year,"-",period,"-1"),
         date_recode=ymd(date_recode))
#
# # need state labels for FIPS codes
#
fips_state <- tigris::fips_codes %>%
  select(state,state_code) %>%
  distinct

county_series <- left_join(county_series,fips_state,by=c(fips_state='state_code'))

saveRDS(county_series,'data/county_series.rds')

county_series <- readRDS('data/county_series.rds')

# Now we want to merge with congressional district

# we need to re-project the data to a common coordinate system to
# do areal interpolation. To do so I use the Albers projection
# as it is supposed to preserve area within the continguous U.S.
# see https://gis.stackexchange.com/questions/141580/which-projection-is-best-for-mapping-the-contiguous-united-states

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# we  need districts for each apportionment (4 going back to the 1980s)
districts2018 <- st_read('data/congress_shape/districtShapes/districts114.shp') %>% st_transform(albers)
districts2008 <- st_read('data/congress_shape/districtShapes/districts110.shp') %>% st_transform(albers)
districts1998 <- st_read('data/congress_shape/districtShapes/districts105.shp') %>% st_transform(albers)
districts1988 <- st_read('data/congress_shape/districtShapes/districts100.shp') %>% st_transform(albers)
#
dist_list <- list(d2018=districts2018,
                  d2008=districts2008,
                  d1998=districts1998,
                  d1988=districts1988)
## need to add in fips codes
fips_all <- tigris::fips_codes
dist_list <- lapply(dist_list,function(d) {
  left_join(d,distinct(select(fips_all,state_name,state_code)),by=c(STATENAME='state_name'))
})
saveRDS(dist_list,'data/dist_list.rds')

county_space <- tigris::counties() %>%
  st_as_sf
saveRDS(county_space,'data/county_space.rds')


# merge in our county data
# we will do areal-weighted interpolation to aggregate to the district level
# this gets to be too big so we need to do it one state at a time
# need to do the merge three times for the four apportionments:
#   1. 2013-2023 (present)
#   2. 2003 - 2013
#   3. 1993-2003
#   4. 1983 - 1993 (last)

# ids over which to map
# we map over each value individually as areal interpolation doesn't do
# more than one time point/value at a time

ids <- select(county_series,year,period) %>%
        distinct

# need 4 county series to merge with distinct districts

county1 <- filter(county_series,year>2012)
county2 <- filter(county_series,year<=2012 & year>2002)
county3 <- filter(county_series,year<=2002 & year>1992)
county4 <- filter(county_series,year<=1992 & year>1982)

rm(county_series)

all_counties <- list(county1=county1,
                     county2=county2,
                     county3=county3,
                     county4=county4)
rm(county1,county2,county3,county4)
# only select vars we need

all_counties <- lapply(all_counties,function(c) {
  # collapse to county
  group_by(c,year,period,fips_state,fips_county) %>%
    summarize(labor_force=mean(labor_force,na.rm=T),
              unemp=mean(unemp,na.rm=T))
  })

# interpolate across districts


county_space <- readRDS('data/county_space.rds') %>%
              st_transform(albers) %>%
            st_buffer(dist = 0) %>%
  select(STATEFP,COUNTYFP,GEOID,geometry)

dist_list <- readRDS('data/dist_list.rds') %>%
  lapply(st_buffer,dist=0) %>%
  lapply(select,DISTRICT,ID,geometry)

# create intersections and save them to save space

int_list <- lapply(names(dist_list),function(d) {
  aw_intersect(source=county_space,.data=dist_list[[d]],
               areaVar="area") %>%
    aw_total(source = county_space, id = GEOID, areaVar = "area", totalVar = "totalArea",
             type = "extensive", weight = "total") %>%
    aw_weight(areaVar = "area", totalVar = "totalArea",
            areaWeight = "areaWeight") %>%
    saveRDS(paste0('data/',d,'_int.rds'))
})

all_ints_names <- rev(list.files(path = "data/",pattern="int.rds",full.names = T))

# we can now load up one intersection at a time and average the covariates

over_states <- purrr::pmap(list(all_counties,
                                all_ints_names,
                                dist_list),function(c,d1,d2) {

  d1 <- readRDS(d1)

  # merge in the covariates we want

  d_join <- left_join(d1,c,by=c(STATEFP="fips_state",
                               COUNTYFP="fips_county"))

  d_join <- split(d_join,list(d_join$year,
                                             d_join$period))

  # re-weight covariates

  out_data_labor <- lapply(d_join,
                           function(this_join) {
                      out_d <- aw_calculate(.data=this_join,
                           value=labor_force,
                           areaWeight="areaWeight") %>%
                        aw_aggregate(target=d2,tid=ID,interVar=labor_force)
                      cat(paste0("Now on year ",unique(this_join$year)," and month ",
                                   unique(this_join$period)),file="output.txt",append=T)
                      out_d$year <- unique(this_join$year)
                      out_d$period <- unique(this_join$period)
                      sf::st_geometry(out_d) <- NULL
                      return(out_d)
                           }) %>% bind_rows

  out_data_unemp <- lapply(d_join,
                           function(this_join) {
                      out_d <- aw_calculate(.data=this_join,
                           value=unemp,
                           areaWeight="areaWeight") %>%
                        aw_aggregate(target=d2,tid=ID,interVar=unemp)
                      cat(paste0("Now on year ",unique(this_join$year)," and month ",
                                   unique(this_join$period)),file="output.txt",append=T)
                      out_d$year <- unique(this_join$year)
                      out_d$period <- unique(this_join$period)
                      sf::st_geometry(out_d) <- NULL
                      return(out_d)
                           }) %>% bind_rows

  # merge and output

  out_data_labor <- left_join(out_data_labor,
                              out_data_unemp,
                              by=c("DISTRICT",
                                   "ID",
                                   "year",
                                   "period")) %>%
    mutate(unemp_rate=unemp/labor_force)

  return(out_data_labor)
  }) %>% bind_rows

# final step: impute missing (only 50 missing month-year values)

# convert our year to a linear time counter so that MissRanger will use it correctly to impute

over_imp <- over_states %>%
  mutate(date_recode=ymd(paste0(year,"-",period,"-1")),
         date_recode=as.numeric(date_recode),
         row_num=1:n(),
         ID=factor(ID),
         DISTRICT=factor(DISTRICT)) %>%
  select(-year,-period)

over_imp <- missRanger(over_imp,pmm.k=5,num.trees=100,returnOOB = T,seed=666112,verbose=2)

# re-create over_states

over_states <- arrange(over_imp,row_num) %>%
                  mutate(year=over_states$year,
                      period=over_states$period)

# check OOB (out of bag prediction error)

attr(over_imp,"oob")

# add to existing data
# need to put IDs back in to over_states

#merge district covariates

dist_state <- readRDS('data/dist_list.rds') %>%
  lapply(function(d) st_drop_geometry(d)) %>%
  lapply(select,ID,
         DISTRICT,
         state_code) %>%
  bind_rows

over_states <- left_join(over_states,
                         dist_state,
                         by=c('ID',
                              'DISTRICT'))
fips_all <- tigris::fips_codes %>%
  select(state_code,
         state) %>%
  distinct

over_states <- left_join(over_states,
                         fips_all,
                         by="state_code") %>%
  mutate(DISTRICT=as.numeric(as.character(DISTRICT)))

# # remove U.S. areas without representation (islands & Puerto Rico)

over_states %>%
  filter(DISTRICT!='98') %>%
  saveRDS('data/over_states.rds')