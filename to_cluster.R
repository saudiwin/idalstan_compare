## ----setup, include=FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE,
                      warning=FALSE,
                      message = FALSE,
                      fig.align = 'center')
# for debugging

options(tinytex.verbose = TRUE)

require(dplyr)
require(ggplot2)
require(tidyr)
require(readr)
#require(blscrapeR)
require(lubridate)
#require(tigris)
#require(sf)
#require(areal)
require(missRanger)
require(idealstan)
require(forcats)


# whether to use a subset of items for sampling to reduce time (for testing purposes only)

test <- F

# whether to use the prior predictive distribution for all models

prior_only <- FALSE

# set to true to create congressional datasets (will take a long time)
create_data <- F

cluster <- TRUE

fit_type <- as.numeric(Sys.getenv("FITTYPE"))

fit_type <- switch(fit_type,"spline1","spline2","spline3","china",
                   "GP","ar1","rw")

spline_degree <- 4

niters <- 300
nwarmup <- 300


## ----load_cong,include=F------------------------------------------------------------------------

# UNCOMMENT COMPLETELY TO LOAD FROM RAW DATA
# UNCOMMENT LINES WITH "readRDS" TO RUN FROM PRE-CALCULATED DATA
# THIS SCRIPT WILL TAKE A SIGNIFICANT AMOUNT OF TIME TO RUN & REQUIRE SIGNIFICANT MEMORY

if(create_data) {
  # load county-level unemployment & other data
# countun <- read_delim('data/la_county.txt',delim="\t")
#
# countun <- mutate(countun,series_id=trimws(series_id)) %>%
#   filter(period!="M13")
# saveRDS(countun,'data/countun.rds')

# countun <- readRDS('data/countun.rds')

# # load series indicators
# id_data <- read_tsv('data/la_series.txt')
#
# county_series <- filter(id_data,measure_code %in% c("04","06"),
#                         area_type_code=="F") %>%
#   mutate(series_id=trimws(series_id))
#
# #merge in unemployment data at series level
#
# county_series <- left_join(county_series,countun, by="series_id")
#
# # need to split to create separate columns for unemp/labor force
#
# county_series <- county_series %>%
#   mutate(measure_code=recode(measure_code,`04`="unemp",
#                              `06`="labor_force"),
#          value=as.numeric(value)) %>%
#   select(-series_id,-matches('footnote'),-seasonal,-srd_code,-series_title,-area_type_code) %>%
#   spread(key = "measure_code",value="value")
#

# need FIPS codes

# bls_fips <- get_bls_county() %>%
#   select(-period,
#          -labor_force,
#          -unemployed,
#          -employed,
#          -unemployed_rate) %>%
#   distinct
#
# saveRDS(bls_fips,'data/bls_fips.rds')

# bls_fips <- readRDS('data/bls_fips.rds')
#
# # merge in FIPS codes and drop unnecessary data
#
# county_series <- select(county_series,-begin_year,-begin_period,-end_year,
#                         -end_period) %>%
#   left_join(bls_fips,by="area_code")
#
# # check unemployment rates over time
#
# county_series <- county_series %>%
#   filter(period!="M13") %>%
#   mutate(period=recode(period,M01="January",
#                            M02="February",
#                            M03="March",
#                            M04="April",
#                            M05="May",
#                            M06="June",
#                            M07="July",
#                            M08="August",
#                            M09="September",
#                            M10="October",
#                            M11="November",
#                            M12="December"),
#          date_recode=paste0(year,"-",period,"-1"),
#          date_recode=ymd(date_recode))
#
# # need state labels for FIPS codes
#
# fips_state <- tigris::fips_codes %>%
#   select(state,state_code) %>%
#   distinct
#
# county_series <- left_join(county_series,fips_state,by=c(fips_state='state_code'))

# saveRDS(county_series,'data/county_series.rds')

# county_series <- readRDS('data/county_series.rds')

# Now we want to merge with congressional district

# we need to re-project the data to a common coordinate system to
# do areal interpolation. To do so I use the Albers projection
# as it is supposed to preserve area within the continguous U.S.
# see https://gis.stackexchange.com/questions/141580/which-projection-is-best-for-mapping-the-contiguous-united-states

# albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# we  need districts for each apportionment (4 going back to the 1980s)
# districts2018 <- st_read('data/congress_shape/districtShapes/districts114.shp') %>% st_transform(albers)
# districts2008 <- st_read('data/congress_shape/districtShapes/districts110.shp') %>% st_transform(albers)
# districts1998 <- st_read('data/congress_shape/districtShapes/districts105.shp') %>% st_transform(albers)
# districts1988 <- st_read('data/congress_shape/districtShapes/districts100.shp') %>% st_transform(albers)
#
# dist_list <- list(d2018=districts2018,
#                   d2008=districts2008,
#                   d1998=districts1998,
#                   d1988=districts1988)
## need to add in fips codes
# fips_all <- tigris::fips_codes
# dist_list <- lapply(dist_list,function(d) {
#   left_join(d,distinct(select(fips_all,state_name,state_code)),by=c(STATENAME='state_name'))
# })
# saveRDS(dist_list,'data/dist_list.rds')

# county_space <- tigris::counties() %>%
#   st_as_sf
# saveRDS(county_space,'data/county_space.rds')


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

# ids <- select(county_series,year,period) %>%
#         distinct

# need 4 county series to merge with distinct districts

# county1 <- filter(county_series,year>2012)
# county2 <- filter(county_series,year<=2012 & year>2002)
# county3 <- filter(county_series,year<=2002 & year>1992)
# county4 <- filter(county_series,year<=1992 & year>1982)
#
# rm(county_series)
#
# all_counties <- list(county1=county1,
#                      county2=county2,
#                      county3=county3,
#                      county4=county4)
# rm(county1,county2,county3,county4)
# # only select vars we need
#
# all_counties <- lapply(all_counties,function(c) {
#   # collapse to county
#   group_by(c,year,period,fips_state,fips_county) %>%
#     summarize(labor_force=mean(labor_force,na.rm=T),
#               unemp=mean(unemp,na.rm=T))
#   })
#
# # interpolate across districts
#
#
# county_space <- readRDS('data/county_space.rds') %>%
#               st_transform(albers) %>%
#             st_buffer(dist = 0) %>%
#   select(STATEFP,COUNTYFP,GEOID,geometry)
#
# dist_list <- readRDS('data/dist_list.rds') %>%
#   lapply(st_buffer,dist=0) %>%
#   lapply(select,DISTRICT,ID,geometry)
#
# # create intersections and save them to save space
#
# int_list <- lapply(names(dist_list),function(d) {
#   aw_intersect(source=county_space,.data=dist_list[[d]],
#                areaVar="area") %>%
#     aw_total(source = county_space, id = GEOID, areaVar = "area", totalVar = "totalArea",
#              type = "extensive", weight = "total") %>%
#     aw_weight(areaVar = "area", totalVar = "totalArea",
#             areaWeight = "areaWeight") %>%
#     saveRDS(paste0('data/',d,'_int.rds'))
# })
#
# all_ints_names <- rev(list.files(path = "data/",pattern="int.rds",full.names = T))
#
# # we can now load up one intersection at a time and average the covariates
#
# over_states <- purrr::pmap(list(all_counties,
#                                 all_ints_names,
#                                 dist_list),function(c,d1,d2) {
#
#   d1 <- readRDS(d1)
#
#   # merge in the covariates we want
#
#   d_join <- left_join(d1,c,by=c(STATEFP="fips_state",
#                                COUNTYFP="fips_county"))
#
#   d_join <- split(d_join,list(d_join$year,
#                                              d_join$period))
#
#   # re-weight covariates
#
#   out_data_labor <- lapply(d_join,
#                            function(this_join) {
#                       out_d <- aw_calculate(.data=this_join,
#                            value=labor_force,
#                            areaWeight="areaWeight") %>%
#                         aw_aggregate(target=d2,tid=ID,interVar=labor_force)
#                       cat(paste0("Now on year ",unique(this_join$year)," and month ",
#                                    unique(this_join$period)),file="output.txt",append=T)
#                       out_d$year <- unique(this_join$year)
#                       out_d$period <- unique(this_join$period)
#                       sf::st_geometry(out_d) <- NULL
#                       return(out_d)
#                            }) %>% bind_rows
#
#   out_data_unemp <- lapply(d_join,
#                            function(this_join) {
#                       out_d <- aw_calculate(.data=this_join,
#                            value=unemp,
#                            areaWeight="areaWeight") %>%
#                         aw_aggregate(target=d2,tid=ID,interVar=unemp)
#                       cat(paste0("Now on year ",unique(this_join$year)," and month ",
#                                    unique(this_join$period)),file="output.txt",append=T)
#                       out_d$year <- unique(this_join$year)
#                       out_d$period <- unique(this_join$period)
#                       sf::st_geometry(out_d) <- NULL
#                       return(out_d)
#                            }) %>% bind_rows
#
#   # merge and output
#
#   out_data_labor <- left_join(out_data_labor,
#                               out_data_unemp,
#                               by=c("DISTRICT",
#                                    "ID",
#                                    "year",
#                                    "period")) %>%
#     mutate(unemp_rate=unemp/labor_force)
#
#   return(out_data_labor)
#   }) %>% bind_rows

# final step: impute missing (only 50 missing month-year values)

# convert our year to a linear time counter so that MissRanger will use it correctly to impute

# over_imp <- over_states %>%
#   mutate(date_recode=ymd(paste0(year,"-",period,"-1")),
#          date_recode=as.numeric(date_recode),
#          row_num=1:n(),
#          ID=factor(ID),
#          DISTRICT=factor(DISTRICT)) %>%
#   select(-year,-period)
#
# over_imp <- missRanger(over_imp,pmm.k=5,num.trees=100,returnOOB = T,seed=666112,verbose=2)
#
# # re-create over_states
#
# over_states <- arrange(over_imp,row_num) %>%
#                   mutate(year=over_states$year,
#                       period=over_states$period)
#
# # check OOB (out of bag prediction error)
#
# attr(over_imp,"oob")

# add to existing data
# need to put IDs back in to over_states

#merge district covariates

# dist_state <- readRDS('data/dist_list.rds') %>%
#   lapply(function(d) st_drop_geometry(d)) %>%
#   lapply(select,ID,
#          DISTRICT,
#          state_code) %>%
#   bind_rows
#
# over_states <- left_join(over_states,
#                          dist_state,
#                          by=c('ID',
#                               'DISTRICT'))
# fips_all <- tigris::fips_codes %>%
#   select(state_code,
#          state) %>%
#   distinct
#
# over_states <- left_join(over_states,
#                          fips_all,
#                          by="state_code") %>%
#   mutate(DISTRICT=as.numeric(as.character(DISTRICT)))
#
# # remove U.S. areas without representation (islands & Puerto Rico)
#
# over_states %>%
#   filter(DISTRICT!='98') %>%
#   saveRDS('data/over_states.rds')
}



over_states <- readRDS('data/over_states.rds')



## ----preparecong--------------------------------------------------------------------------------

# UNCOMMENT TO RUN FROM SCRATCH

# rm(county_series)
# 
# # need Congress rollcall info
# 
# rollinfo <- read_csv('data/Hall_rollcalls.csv')
# 
# unam_roll <- filter(rollinfo,
#                     yea_count==0|nay_count==0)
# 
# # member votes
# 
# rollcalls <- read_csv('data/Hall_votes.csv') %>% 
#   filter(congress>100)
# 
# #remove unanmous votes
# 
# rollcalls <- anti_join(rollcalls,
#                        unam_roll,
#                        by=c("congress",
#                             "rollnumber"))
# 
# # need member info
# 
# meminfo <- read_csv('data/Hall_members.csv')
# 
# # merge member info with rollcall data
# 
# rollcalls <- left_join(select(rollcalls,-prob,-chamber),
#                           select(meminfo,
#                                  -chamber,
#                                  -occupancy,
#                                  -last_means,
#                                  -bioguide_id,
#                                  -born,
#                                  -(died:nokken_poole_dim2)),
#                           by=c('icpsr','congress'))
# 
# 
# rollcalls <- left_join(rollcalls,
#                         select(rollinfo,
#                                congress,
#                                rollnumber,
#                                date),
#                        by=c("congress",
#                             "rollnumber"))
# 
# 
# # need to recode DISTRICT in over_states from 0 to 1 (at-large is coded as 1 in rollcall data)
# 
# over_states <- mutate(over_states,DISTRICT=if_else(DISTRICT==0,DISTRICT+1,DISTRICT))
# 
# # there were a series of votes held on Jan. 1st, 2013 that really belonged to the old 2012 
# # Congress. To avoid a mismatch, I recode those votes to December 31st, 2012.
# 
# rollcalls <- mutate(rollcalls,
#                     date=if_else(date==ymd('2013-01-01'),date-1,date),
#                     year=year(date),
#                     month=month(date,label=T,abbr=F)) %>% 
#             left_join(over_states,
#                        by=c(district_code="DISTRICT",
#                             state_abbrev="state",
#                             "year",
#                             month="period"))
# 
# # check for missing
# 
# lookat <- filter(rollcalls,district_code!=0,year>1989,year<2019,is.na(unemp_rate))
# 
# # remove remaining missing data (not relevant and prior/post unemployment data is recorded)
# 
# rollcalls <- filter(rollcalls,!is.na(unemp_rate))
# 
# # not present in legislature = missing data
# 
# rollcalls <- mutate(rollcalls,
#                  cast_code=factor(cast_code,exclude=0L),
#                  cast_code=fct_collapse(cast_code,
#                                         Yes=c("1","2","3"),
#                                         Nay=c("4","5","6"),
#                                         Abstention=c("7","8","9")),
#                  cast_code=fct_relevel(cast_code,"Nay","Yes","Abstention"),
#                  party_code=factor(party_code,
#                                    labels=c("D",
#                                             "R",
#                                             "I"))) %>% 
#   distinct
# 
# rollcalls$date_month <- rollcalls$date
# day(rollcalls$date_month) <- 1

# saveRDS(rollcalls,'data/rollcalls.rds')

# remove unnecesssary objects

# rm(over_states,rollinfo,meminfo)



# Load rollcall data ------------------------------------------------------


rollcalls <- readRDS('data/rollcalls.rds') 

# need to figure out knots

collapse_rollcall <- group_by(rollcalls, congress) %>% 
  arrange(congress, date) %>% 
  slice(c(1, n()))

# dates of presidential administrations

spline_knots_all <- c(min(collapse_rollcall$date_month),
                      ymd("1993-01-01"),
                      ymd("2001-01-01"),
                      ymd("2009-01-01"),
                      ymd("2017-01-01"),
                      max(collapse_rollcall$date_month))

collapse_china <- filter(collapse_rollcall, year<2010)

spline_knots_china <- c(min(collapse_china$date_month),
                      ymd("1993-01-01"),
                      ymd("2001-01-01"),
                      max(collapse_china$date_month))

spline_knots_year <- c(min(collapse_rollcall$year),
                           1992,
                           2000,
                           2008,
                           2016,
                          max(collapse_rollcall$year))

print(paste0("Processing model: ",fit_type))


## ----runcong1-----------------------------------------------------------------------------------


unemp1 <- rollcalls %>% 
  select(cast_code,rollnumber,congress,
         bioname,party_code,date_month,unemp_rate) %>% 
  mutate(item=paste0(congress,"_",rollnumber),
         cast_code=recode_factor(cast_code,Abstention=NA_character_),
         cast_code=as.numeric(cast_code)-1,
         bioname=factor(bioname),
         unemp_rate=100*unemp_rate,
         bioname=relevel(bioname,"DeFAZIO, Peter Anthony")) %>% 
  distinct %>% 
  filter(party_code %in% c("R","D")) %>% 
  mutate(party_code=factor(party_code))

# drop legislators who vote on fewer than 25 unanimous bills

check_bills <- group_by(unemp1,item,cast_code) %>% count %>% 
  group_by(item) %>% 
  summarize(prop=n[cast_code==1] / sum(n),
            n_vote=sum(n)) %>% 
  ungroup %>% 
  mutate(prop=ifelse(prop==.5,
                          sample(c(.49,.51),1),
                     prop),
    util_func=(1 / sqrt((.5 - prop)^2))*n_vote) %>% 
  arrange(desc(util_func))

legis_count <- group_by(unemp1, item) %>% 
  mutate(unan=all(cast_code[!is.na(cast_code)]==1) || all(cast_code[!is.na(cast_code)]==0)) %>% 
  group_by(bioname) %>% 
  summarize(n_votes_nonunam=length(unique(item[!unan])))

# check number of days in legislature

num_days <- distinct(unemp1,bioname,date_month) %>% 
  count(bioname)

if(test) {
  
  # only use a subset of bills from full time period
  
  unemp1 <- filter(unemp1, (item %in% sample(unique(item),
                                  3000)) | item %in% c("105_919","115_1050"))

  # use only last congress
  
  # unemp1 <- filter(unemp1, congress==115) %>% 
  #   filter((item %in% sample(unique(item),
  #                                  200)) | item %in% c("115_588","115_1050"))
  # 
}

# you had to have voted on at least 10 separate days

unemp1 <- anti_join(unemp1, filter(legis_count, n_votes_nonunam<25),by="bioname") %>% 
  anti_join(filter(num_days,n<10),by="bioname") %>% 
                 id_make(outcome_disc="cast_code",
                         item_id="item",
                  person_id="bioname",
                  group_id="party_code",
                  time_id = "date_month",
                  person_cov = ~unemp_rate*party_code)

if(test) {
  
  unemp1_fit <- id_estimate(unemp1,model_type=2,
                          vary_ideal_pts = 'splines',
                          niters=niters,
                          warmup=nwarmup,
                          spline_knots=spline_knots_all,
                          spline_degree = spline_degree-2,
                          nchains=2,
                          ncores=parallel::detectCores(),
                          const_type = "items",prior_only = prior_only,
                          restrict_ind_high = "115_588",
                          restrict_sd_high = .01,
                          restrict_sd_low = .01,
                          restrict_ind_low="115_1050",
                          fixtype="prefix",
                          adapt_delta=0.95,
                          # pars=c("steps_votes_grm",
                          #        "steps_votes",
                          #        "B_int_free",
                          #        "A_int_free"),
                          #include=F,
                          id_refresh=100)

unemp2_fit <- id_estimate(unemp1,model_type=2,
                          vary_ideal_pts = 'splines',
                          niters=niters,
                          warmup=nwarmup,
                          spline_knots=spline_knots_all,
                          spline_degree = spline_degree - 1,
                          nchains=2,
                          ncores=parallel::detectCores(),
                          const_type = "items",
                          restrict_ind_high = "115_588",
                          restrict_ind_low="115_1050",
                          fixtype="prefix",
                          adapt_delta=0.95,
                          # pars=c("steps_votes_grm",
                          #        "steps_votes",
                          #        "B_int_free",
                          #        "A_int_free"),
                          #include=F,
                          id_refresh=100)

unemp3_fit <- id_estimate(unemp1,model_type=2,
                          vary_ideal_pts = 'splines',
                          niters=niters,
                          warmup=nwarmup,
                          spline_knots=spline_knots_all,
                          spline_degree = spline_degree,
                          nchains=2,
                          ncores=parallel::detectCores(),
                          const_type = "items",
                          restrict_ind_high = "115_588",
                          restrict_ind_low="115_1050",
                          fixtype="prefix",
                          adapt_delta=0.95,
                          # pars=c("steps_votes_grm",
                          #        "steps_votes",
                          #        "B_int_free",
                          #        "A_int_free"),
                          #include=F,
                          id_refresh=100)
  
} else {
  
  if(fit_type=="spline1") {
    
    unemp1_fit <- id_estimate(unemp1,model_type=2,
                              vary_ideal_pts = 'splines',
                              niters=niters,
                              warmup=nwarmup,
                              spline_knots=spline_knots_all,
                              spline_degree = spline_degree - 2,
                              nchains=2,
                              ncores=parallel::detectCores(),
                              const_type = "items",prior_only = TRUE,
                              restrict_ind_high = "105_919",
                              restrict_ind_low="115_1050",
                              fixtype="prefix",
                              adapt_delta=0.95,
                              # pars=c("steps_votes_grm",
                              #        "steps_votes",
                              #        "B_int_free",
                              #        "A_int_free"),
                              #include=F,
                              id_refresh=100)
    
    unemp1_fit@stan_samples$save_object("/scratch/rmk7/unemp1_fit.rds")
    
  }
  
  if(fit_type=="spline2") {
    
    unemp2_fit <- id_estimate(unemp1,model_type=2,
                              vary_ideal_pts = 'splines',
                              niters=niters,
                              warmup=nwarmup,
                              spline_knots=spline_knots_all,
                              spline_degree = spline_degree - 1,
                              nchains=2,
                              ncores=parallel::detectCores(),
                              const_type = "items",
                              restrict_ind_high = "105_919",
                              restrict_ind_low="115_1050",
                              fixtype="prefix",
                              adapt_delta=0.95,
                              # pars=c("steps_votes_grm",
                              #        "steps_votes",
                              #        "B_int_free",
                              #        "A_int_free"),
                              #include=F,
                              id_refresh=100)
    
    unemp2_fit@stan_samples$save_object("/scratch/rmk7/unemp2_fit.rds")
    
    
  }
  
  if(fit_type=="spline3") {
    
    
    unemp3_fit <- id_estimate(unemp1,model_type=2,
                              vary_ideal_pts = 'splines',
                              niters=niters,
                              warmup=nwarmup,
                              spline_knots=spline_knots_all,
                              spline_degree = spline_degree,
                              nchains=2,
                              ncores=parallel::detectCores(),
                              const_type = "items",
                              restrict_ind_high = "105_919",
                              restrict_ind_low="115_1050",
                              restrict_sd_low = .01,
                              restrict_sd_high = .01,
                              fixtype="prefix",
                              adapt_delta=0.95,
                              # pars=c("steps_votes_grm",
                              #        "steps_votes",
                              #        "B_int_free",
                              #        "A_int_free"),
                              #include=F,
                              id_refresh=100)
    
    unemp3_fit@stan_samples$save_object("/scratch/rmk7/unemp3_fit.rds")
    
  }
  
  
}



## ----fitgp,eval=F-------------------------------------------------------------------------------
## 

  
  if(fit_type=="GP") {
    
    rollcalls <- readRDS('data/rollcalls.rds')
    unemp2 <- rollcalls %>%
      select(cast_code,rollnumber,
             bioname,party_code,date,unemp_rate,congress) %>%
      mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
             cast_code=as.numeric(cast_code)-1,
             item_id=paste0(congress,"_",rollnumber)) %>%
      distinct %>%
      #filter(date>lubridate::ymd("2008-06-01")) %>%
      id_make(outcome_disc="cast_code",
              item_id="item_id",
              person_id="bioname",
              group_id="party_code",
              time_id = "year",
              remove_cov_int = T,
              person_cov = ~unemp_rate*party_code)

    unemp_gp_fit <- id_estimate(unemp2,model_type=2,vary_ideal_pts = 'GP',
                              niters=niters,
                              warmup=nwarmup,
                              ncores=parallel::detectCores(),nchain=2,
                              fixtype="prefix",
                              const_type = "items",
                              restrict_ind_high = "105_919",
                              restrict_ind_low="115_1050",
                              restrict_sd_low = .01,
                              restrict_sd_high = .01,
                              spline_knots = spline_knots_year,
                              use_groups = T,
                              #  output_samples=100,
                              #  pars=c("steps_votes_grm",
                              #         "steps_votes",
                              #         "B_int_free",
                              #         "A_int_free"),
                              id_refresh=100)
    
   unemp_gp_fit@stan_samples$save_object('/scratch/rmk7/unemp_gp_fit.rds')
    
  }



# fitar1 ------------------------------------------------------------------

if(fit_type=="ar1") {
  
  rollcalls <- readRDS('data/rollcalls.rds')
  unemp2 <- rollcalls %>%
    select(cast_code,rollnumber,
           bioname,party_code,date,unemp_rate,congress) %>%
    mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1,
           item_id=paste0(congress,"_",rollnumber)) %>%
    distinct %>%
    #filter(date>lubridate::ymd("2008-06-01")) %>%
    id_make(outcome_disc="cast_code",
            item_id="item_id",
            person_id="bioname",
            group_id="party_code",
            time_id = "year",
            remove_cov_int = T,
            person_cov = ~unemp_rate*party_code)
  
  unemp1_ar_fit <- id_estimate(unemp2,model_type=2,
                            vary_ideal_pts = 'AR1',
                            niters=niters,
                            warmup=nwarmup,
                            spline_knots=spline_knots_year,
                            spline_degree = spline_degree,
                            nchains=2,
                            ncores=parallel::detectCores(),
                            const_type = "items",prior_only = TRUE,
                            restrict_ind_high = "105_919",
                            restrict_ind_low="115_1050",
                            restrict_sd_low = .01,
                            restrict_sd_high = .01,
                            fixtype="prefix",
                            adapt_delta=0.95,
                            # pars=c("steps_votes_grm",
                            #        "steps_votes",
                            #        "B_int_free",
                            #        "A_int_free"),
                            #include=F,
                            id_refresh=100)
  
  unemp1_ar_fit@stan_samples$save_object("/scratch/rmk7/unemp1_ar_fit.rds")
  
  
}

# fitrw ------------------------------------------------------------------

if(fit_type=="rw") {
  
  rollcalls <- readRDS('data/rollcalls.rds')
  unemp2 <- rollcalls %>%
    select(cast_code,rollnumber,
           bioname,party_code,date,unemp_rate,congress) %>%
    mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1,
           item_id=paste0(congress,"_",rollnumber)) %>%
    distinct %>%
    #filter(date>lubridate::ymd("2008-06-01")) %>%
    id_make(outcome_disc="cast_code",
            item_id="item_id",
            person_id="bioname",
            group_id="party_code",
            time_id = "year",
            remove_cov_int = T,
            person_cov = ~unemp_rate*party_code)
  
  unemp1_rw_fit <- id_estimate(unemp2,model_type=2,
                               vary_ideal_pts = 'random_walk',
                               niters=niters,
                               warmup=nwarmup,
                               spline_knots=spline_knots_year,
                               spline_degree = spline_degree,
                               nchains=2,
                               ncores=parallel::detectCores(),
                               const_type = "items",prior_only = TRUE,
                               restrict_ind_high = "105_919",
                               restrict_ind_low="115_1050",
                               restrict_sd_low = .01,
                               restrict_sd_high = .01,
                               fixtype="prefix",
                               adapt_delta=0.95,
                               # pars=c("steps_votes_grm",
                               #        "steps_votes",
                               #        "B_int_free",
                               #        "A_int_free"),
                               #include=F,
                               id_refresh=100)
  
  unemp1_rw_fit@stan_samples$save_object("/scratch/rmk7/unemp1_rw_fit.rds")
  
  
}



## ----fitchina, include=F------------------------------------------------------------------------

if(fit_type=="china") {
  
  rollcalls <- readRDS('data/rollcalls.rds') %>% 
    select(cast_code,rollnumber,year,
           bioname,party_code,date_month,unemp_rate,
           district_code,state_abbrev,congress) %>% 
    mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1,
           item_id=paste0(congress,"_",rollnumber)) %>% 
    distinct %>% 
    mutate(decade=case_when(year<2000~1L,
                            year<2010~2L,
                            TRUE~3L)) %>% 
    filter(decade<3)
  load('data/andy_hall_jop/fh_final_analysis.rdata')
  
  x <- filter(x,!is.na(x)) %>% 
    distinct
  
  rollcalls <- left_join(rollcalls,select(x,dist,decade,x,z,state),
                         by=c(district_code="dist",
                              "decade",state_abbrev="state"))
  
  rollcalls <- filter(rollcalls,!is.na(x))
  
  china_data <- id_make(rollcalls,outcome_disc="cast_code",
                        item_id="item_id",
                        person_id="bioname",
                        group_id="party_code",
                        time_id = "date_month",
                        remove_cov_int = T,
                        person_cov = ~unemp_rate*party_code*x)
  rm(rollcalls)
  china_fit1 <- id_estimate(china_data,model_type=2,
                            vary_ideal_pts = 'splines',
                            niters=niters,
                            warmup=nwarmup,
                            spline_knots=spline_knots_china,
                            spline_degree = spline_degree,
                            nchains=2,
                            ncores=parallel::detectCores(),
                            const_type = "items",
                            restrict_ind_high = "105_238",
                            restrict_ind_low="105_962",
                            fixtype="prefix",
                            adapt_delta=0.95,
                            # pars=c("steps_votes_grm",
                            #        "steps_votes",
                            #        "B_int_free",
                            #        "A_int_free"),
                            #include=F,
                            id_refresh=100)
  
  saveRDS(china_fit1,'/scratch/rmk7/china_fit1.rds')
  
  
}


## ----ardistconst, fig.cap="Over-time Ideal Points for Constrained Legislators by Month, 1990-2018",eval=FALSE----
## 
## #unemp1_fit <- readRDS("data/unemp1_run1fit.rds")
## 
## # need to calculate ideal points manually b/c of hierarchical covariates
## library(posterior)
## 
## time_points <- unemp1_fit@time_varying
## legis_cov <- unemp1_fit@stan_samples$draws("legis_x") %>% as_draws_matrix()
## 
## 
## 
## id_plot_legis_dyn(unemp1_fit,use_ci=F,plot_text = T,include=c("BARTON, Joe Linus",
##                                                               "NEAL, Richard Edmund")) +
##   scale_color_manual(values=c(R="red",
##                               D="blue"),guide="none")
## 
## ggsave("constrained_dist.png")
## 

if(fit_type=="postprocess") {
  

## ----ardistall, fig.cap="Over-time Ideal Points for All U.S. House Legislators by Month, 1990-2018"----

dem_ids <- select(unemp1_fit@score_data@score_matrix,person_id,group_id) %>% 
  distinct %>% filter(group_id %in% c("D","R")) %>% 
  mutate(group_id=factor(group_id))

id_plot_legis_dyn(unemp1_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1,
                  include=dem_ids$person_id) + scale_color_manual(values=c(R="red",
                              D="blue",I="green"),name="")

ggsave("overall_dist.png")



## ----ardistallconst, fig.cap="Overlay of Constrained Legislators on Unconstrained Legislators, 1990-2018",eval=FALSE----
## 
## dem_ids <- select(unemp1_fit@score_data@score_matrix,person_id,group_id) %>%
##   distinct %>% filter(group_id %in% c("D","R"))
## 
## id_plot_legis_dyn(unemp1_fit,use_ci=F,plot_text = F,
##                   highlight = c("BARTON, Joe Linus","NEAL, Richard Edmund"),
##                   include=dem_ids$person_id)
## 
## ggsave("overlay_dist.png")
## 


## ----ar1plot,fig.cap="Effect of District-Level Unemployment Rates (AR1 Model) on Legislators' Ideal Points 1990-2018"----

# load AR(1) fit and plot resulting data

id_plot_cov(unemp1_fit,label_high = "Conservative",
            label_low="Liberal",pred_outcome = "Yes",
            new_cov_names = c(`unemp_rate:party_codeR`="Republican X\nUnemployment",
                              `unemp_rate:party_codeI`="Independent X\nUnemployment",
                              unemp_rate="Unemployment",
                              party_codeR="Republican",
                              party_codeI="Independent",
                              `(Intercept)`="Intercept"),
            recalc_vals = c("Republican X\nUnemployment",
                            "Unemployment",
                            "Combined\nRepublican"),
            filter_cov = "Intercept")

ggsave("overall_eff_ar1.png")



## ----gp1plot,fig.cap="Effect of District-Level Unemployment Rates (GP Model) on Legislators' Ideal Points 1990-2018",eval=F,include=F----
## 
## # load AR(1) fit and plot resulting data
## 
## id_plot_cov(unemp2_fit,label_high = "Conservative",
##             label_low="Liberal",pred_outcome = "Yes",
##             new_cov_names = c(`unemp_rate:party_codeR`="Republican X\nUnemployment",
##                               `unemp_rate:party_codeI`="Independent X\nUnemployment",
##                               unemp_rate="Unemployment",
##                               party_codeR="Republican",
##                               party_codeI="Independent",
##                               `(Intercept)`="Intercept"),
##             recalc_vals = c("Republican X\nUnemployment",
##                             "Unemployment",
##                             "Combined\nRepublican"),
##             filter_cov = "Intercept")
## 
## ggsave("overall_eff_gp1.png")
## 


## ----irf_calc,include=F,eval=FALSE--------------------------------------------------------------
## # need to calculate these in a separate chunk as they produce too much output otherwise
## dem_ids <- select(unemp1_fit@score_data@score_matrix,person_id,group_id) %>%
##   distinct %>% filter(group_id=="D")
## outplot_dem <- id_plot_irf(unemp1_fit,label_high = "Conservative",
##             label_low="Liberal",pred_outcome = "Yes",
##             recalc_vals = F,
##             line_type=1,
##             line_width = .4,
##             line_alpha = 0.3,
##             time_label= "Months Since Unemployment Rate Increase",
##             line_color='black',
##             include=dem_ids$person_id,
##             cov_name = c("unemp_rate"),
##             use_ci=F)
## 
## dem_ids <- select(unemp1_fit@score_data@score_matrix,person_id,group_id) %>%
##   distinct %>% filter(group_id=="R")
## 
## outplot_rep <- id_plot_irf(unemp1_fit,label_high = "Conservative",
##             label_low="Liberal",pred_outcome = "Yes",
##             recalc_vals = F,
##             line_type=1,
##             line_width = .4,
##             line_alpha = 0.3,
##             time_label= "Months Since Unemployment Rate Increase",
##             line_color='black',
##             include=dem_ids$person_id,
##             cov_name = c("unemp_rate"),
##             use_ci=F)
## 


## ----chinapl,fig.cap="Interaction of Unemployment Rates and Chinese Import Exposure on Legislative Votes 1990-2010"----

# load AR(1) fit and plot resulting data

id_plot_cov(china_fit1,label_high = "Conservative",
            label_low="Liberal",pred_outcome = "Yes",
            new_cov_names = c(`unemp_rate:party_codeR`="Republican X\nUnemployment",
                              `unemp_rate:party_codeI`="Independent X\nUnemployment",
                              unemp_rate="Unemployment",
                              x="Import Exposure\nPer Worker",
                              `(Intercept)`="Intercept",
                              `unemp_rate:x`="Unemployment X\nImports",
                              `unemp_rate:party_codeR:x`="Unemployment X\nRepublican\nImports",
                              `unemp_rate:party_codeI:x`="Unemployment X\nIndependent\nImports",
                              `party_codeR:x`="Republican X\nImports",
                              `party_codeI:x`="Independent X\nImports"),
            # recalc_vals = c("Republican X\nUnemployment",
            #                 "Unemployment",
            #                 "Combined\nRepublican"),
            filter_cov = "Intercept") 

ggsave("overall_eff_china.png")



## ----irfdem,fig.cap="Impulse Response Functions for Unemployment on Democrats' Ideal Points",eval=FALSE----
## 
## 
## print(outplot_dem)
## 
## ggsave("irf_dem.png")


## ----irfrep,fig.cap="Impulse Response Functions for Unemployment on Republicans' Ideal Points",eval=FALSE----
## 
## print(outplot_rep)
## 
## ggsave("irf_rep.png")


## ----chinaint,fig.cap="Effect of Unemployment Conditional on Chinese Import Exposure per Worker, 1990-2010"----

# need to extract the values of the covariates

legis_x <- rstan::extract(china_fit1@stan_samples,"legis_x")[[1]]

# let's separate out the different values of the covariates

unemp_rate <- legis_x[,2]
import <- legis_x[,3]
rep_unemp <- legis_x[,4]
unemp_import <- legis_x[,6]
rep_import <- legis_x[,7]
rep_import_unemp <- legis_x[,9]

# now we need make a grid of all values in the data

import_range <- seq(min(china_fit@score_data@score_matrix$x,na.rm=T),
                   max(china_fit@score_data@score_matrix$x,na.rm=T),
                   by=0.1)


# iterate over data and calculate average values of response
# conditional for discrimination of different types

china_pars <- rstan::extract(china_fit@stan_samples)

all_plot_vals_rep <- lapply(import_range, function(i) {
  # returns a data frame of all posterior draws for this particular 
  # data combination
  # iterate over discrimination vectors
  
  this_est <- lapply(1:nrow(china_pars$L_free), function(d) {
    all_discrim <- china_pars$sigma_reg_free[d,]
    pos_discrim <- all_discrim[all_discrim>0]
    neg_discrim <- all_discrim[all_discrim<0]
    pos <- tibble(y_pred_mean=mean(plogis((unemp_rate[d] + rep_unemp[d] +
                                      unemp_import[d]*i +
           rep_import_unemp[d]*i)*pos_discrim)
           - 0.5),
                     import=i,
                     type="Pr(Yes|Conservative)")
    
    neg <- tibble(y_pred_mean=mean(plogis((unemp_rate[d] + rep_unemp[d] +
                                      unemp_import[d]*i +
           rep_import_unemp[d]*i)*neg_discrim)
           - 0.5),
                     import=i,
                     type="Pr(Yes|Liberal)")
    
    combined <- bind_rows(pos,neg)
    combined$iter <- d
    combined$party <- "R"
    combined
  }) %>% bind_rows
  
  this_est
}) %>% bind_rows

all_plot_vals_dem <- lapply(import_range, function(i) {
    
  this_est <- lapply(1:nrow(china_pars$L_free), function(d) {
    all_discrim <- china_pars$sigma_reg_free[d,]
    pos_discrim <- all_discrim[all_discrim>0]
    neg_discrim <- all_discrim[all_discrim<0]
    pos <- tibble(y_pred_mean=mean(plogis((unemp_rate[d] + 
                                      unemp_import[d]*i)*pos_discrim)-0.5),
                     import=i,
                     type="Pr(Yes|Conservative)")
    
    neg <- tibble(y_pred_mean=mean(plogis((unemp_rate[d] + 
                                      unemp_import[d]*i)*neg_discrim)-0.5),
                     import=i,
                     type="Pr(Yes|Liberal)")
    
    combined <- bind_rows(pos,neg)
    combined$iter <- d
    combined$party <- "D"
    combined
  }) %>% bind_rows
  
  this_est
}) %>% bind_rows

# combine dems and reps

combined_all <- bind_rows(all_plot_vals_rep,all_plot_vals_dem)

# plot the bugger

combined_all %>%
  group_by(party,import,type) %>% 
  summarize(y_pred_mean_mean=mean(y_pred_mean),
            y_pred_low=quantile(y_pred_mean,.05),
            y_pred_high=quantile(y_pred_mean,.95)) %>% 
  ungroup %>% 
  ggplot(aes(y=y_pred_mean_mean,x=import)) +
  geom_errorbar(aes(ymin=y_pred_low,
                     ymax=y_pred_high,
                     color=party)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold")) +
  facet_wrap(~type,scales="free") +
  xlab("Import Exposure per Worker") +
  ylab("Marginal Effect of Unemployment") +
  scale_colour_manual(values=c("D"="blue",
                               "R"="red")) +
  scale_y_continuous(labels=scales::percent)
#   

ggsave("china_int_plot.png")

# end of post-processing

}

