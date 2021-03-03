# Run models on cluster

require(idealstan)
require(dplyr)
require(tidyr)


this_mod <- Sys.getenv("MODTYPE")


if(this_mod=="first_ar") {
  
  rollcalls <- readRDS('data/rollcalls.rds')
  
  unemp1 <- rollcalls %>% 
    select(cast_code,rollnumber,congress,
           bioname,party_code,date_month,unemp_rate) %>% 
    mutate(item=paste0(congress,"_",rollnumber),
           cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1,
           bioname=factor(bioname),
           bioname=relevel(bioname,"DeFAZIO, Peter Anthony")) %>% 
    filter(bioname %in% c("BARTON, Joe Linus",
                          "DeFAZIO, Peter Anthony",
                          "LEVIN, Sander Martin",
                          "ROGERS, Harold Dallas (Hal)")) %>%
    distinct
  
  # drop legislators who vote on fewer than 25 unanimous bills
  
  check_bills <- group_by(unemp1,item,cast_code) %>% count
  
  legis_count <- group_by(unemp1, item) %>% 
    mutate(unan=all(cast_code[!is.na(cast_code)]==1) || all(cast_code[!is.na(cast_code)]==0)) %>% 
    group_by(bioname) %>% 
    summarize(n_votes_nonunam=length(unique(item[!unan])))
  
  # check number of days in legislature
  
  num_days <- distinct(unemp1,bioname,date_month) %>% 
    count(bioname)
  
  # you had to have voted on at least 10 separate days
  
  unemp1 <- anti_join(unemp1, filter(legis_count, n_votes_nonunam<25),by="bioname") %>% 
    anti_join(filter(num_days,n<10),by="bioname") %>% 
    id_make(outcome_disc="cast_code",
            item_id="item",
            person_id="bioname",
            group_id="party_code",
            time_id = "date_month")
  #person_cov = ~unemp_rate*party_code)
  
  # remove original data
  
  rm(rollcalls)
  
  unemp1_fit <- id_estimate(unemp1,model_type=2,
                            vary_ideal_pts = 'AR1',
                            niters=300,
                            warmup=300,
                            within_chain="threads",
                            nchains=1,
                            ncores=parallel::detectCores(),
                            grainsize=floor(length(unique(unemp1@score_matrix$person_id))/parallel::detectCores()),
                            restrict_ind_high = "BARTON, Joe Linus",
                            restrict_ind_low="DeFAZIO, Peter Anthony",
                            restrict_sd_low = 3,
                            fix_low=0,
                            fixtype="prefix",save_files="/scratch/rmk7/idalstan_compare/",
                            cmdstan_path_user="/home/rmk7/cmdstan",
                            # pars=c("steps_votes_grm",
                            #        "steps_votes",
                            #        "B_int_free",
                            #        "A_int_free"),
                            #include=F,
                            id_refresh=100)
  
  saveRDS(unemp1_fit,'/scratch/rmk7/idalstan_compare/unemp1_fit.rds')
  
  
} else if(this_mod=='gp_groups') {
  
  rollcalls <- readRDS('data/rollcalls.rds')
  unemp2 <- rollcalls %>% 
    select(cast_code,rollnumber,
           bioname,party_code,date,unemp_rate) %>% 
    mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1) %>% 
    distinct %>% 
    filter(date>lubridate::ymd("2008-06-01")) %>% 
    id_make(outcome_disc="cast_code",
            item_id="rollnumber",
            person_id="bioname",
            group_id="party_code",
            time_id = "date",
            remove_cov_int = T,
            person_cov = ~unemp_rate*party_code)

  unemp2_fit <- id_estimate(unemp2,model_type=2,vary_ideal_pts = 'GP',
                            niters=300,
                            warmup=300,gpu=T,
                            ncores=2,nchain=1,save_files="/scratch/rmk7/idalstan_compare/",
                            fixtype="prefix",
                            restrict_ind_high = "R",
                            restrict_ind_low="D",
                            restrict_sd_low = 3,
                            fix_low=0,
                            use_groups = T,
                            cmdstan_path_user="/home/rmk7/cmdstan",
                            within_chain="threads",
                            #  output_samples=100,
                            #  pars=c("steps_votes_grm",
                            #         "steps_votes",
                            #         "B_int_free",
                            #         "A_int_free"),
                            id_refresh=100)
  
  saveRDS(unemp2_fit,'/scratch/rmk7/idalstan_compare/unemp2_fit.rds')
  
  
} else if(this_mod=="chinafit") {
  
  rollcalls <- readRDS('data/rollcalls.rds') %>% 
    select(cast_code,rollnumber,year,
           bioname,party_code,date,unemp_rate,
           district_code,state_abbrev) %>% 
    mutate(cast_code=recode_factor(cast_code,Abstention=NA_character_),
           cast_code=as.numeric(cast_code)-1) %>% 
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
                        item_id="rollnumber",
                        person_id="bioname",
                        group_id="party_code",
                        time_id = "date",
                        remove_cov_int = T,
                        person_cov = ~unemp_rate*party_code*x)
  rm(rollcalls)
  china_fit <- id_estimate(china_data,vary_ideal_pts = 'AR1',
                           niters=300,
                           warmup=300,gpu=F,
                           ncores=parallel::detectCores(),nchains=1,
                           fixtype="prefix",save_files="/scratch/rmk7/idalstan_compare/",
                           restrict_ind_high = "R",
                           restrict_ind_low="D",
                           restrict_sd_low = 3,
                           cmdstan_path_user="/home/rmk7/cmdstan",
                           fix_low=0,
                           use_groups = T,
                           map_over_id="items",
                           within_chain="threads",
                           # output_samples=100,
                           # pars=c("steps_votes_grm",
                           #        "steps_votes",
                           #        "B_int_free",
                           #        "A_int_free"),
                           id_refresh=100)
  
  saveRDS(china_fit,'/scratch/rmk7/idalstan_compare/china_fit.rds')
  
}

