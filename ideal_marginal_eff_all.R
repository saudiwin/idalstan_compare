# calculate ideal point marginal effects for different models

library(idealstan)
library(tidyverse)
library(marginaleffects)
library(modelsummary)
library(bayesplot)

rollcalls <- readRDS('data/rollcalls.rds')

# create unemployment series as year-on-year changes

unemp_changes <- select(rollcalls, bioname, year, month, unemp_rate) %>% 
  distinct %>% 
  group_by(bioname) %>% 
  arrange(bioname, month, year) %>% 
  mutate(unemp_rate_yoy=unemp_rate - lag(unemp_rate),
         unemp_rate_yoy=coalesce(unemp_rate_yoy, 0))

rollcalls <- left_join(rollcalls, select(ungroup(unemp_changes),
                                         -unemp_rate),
                       by=c('bioname','year','month'))

set.seed(01022024)

unemp1 <- rollcalls %>% 
  select(cast_code,rollnumber,congress,
         bioname,party_code,date_month,unemp_rate_yoy) %>% 
  mutate(item=paste0(congress,"_",rollnumber),
         cast_code=recode_factor(cast_code,Abstention=NA_character_),
         cast_code=as.numeric(cast_code)-1,
         bioname=factor(bioname),
         unemp_rate_yoy=100*unemp_rate_yoy,
         bioname=relevel(bioname,"DeFAZIO, Peter Anthony")) %>% 
  distinct %>% 
  filter(party_code %in% c("R","D")) %>% 
  mutate(party_code=factor(party_code))

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
  filter(congress==115)

# load different fitted models 

m_loc <- "data/1151_12_1_fit.rds"
  
  
  test_mod <- readRDS(m_loc)
  
  # need new data
  
  eps <- 1e-4
  
  new_data1 <- mutate(unemp1,
                      unemp_rate_yoy = unemp_rate_yoy - eps / 2)
  
  new_data2 <- mutate(unemp1,
                      unemp_rate_yoy = unemp_rate_yoy + eps / 2)
  
  l_full <- test_mod@stan_samples$draws("L_full")
  
  #draws <- sample(1:dim(l_full)[1], 100)
  
  draws <- readRDS("data/draws.rds")
  
  # figure out this partition\
  
  # remainder <- length(levels(test_mod_data$item)) %% num_partition
  # 
  # indices <- c(rep(1:num_partition, each=floor(length(levels(test_mod_data$item))/num_partition)),
  #              rep(num_partition, times=remainder))
  
  print("Predicting one")
  
  test_mod_pred1 <- id_post_pred(test_mod,newdata=new_data1,
                                 use_cores=floor(parallel::detectCores()/2),
                                 item_subset=levels(new_data1$item),
                                 type="epred",
                                 draws=draws)
  print("Predicting two")
  
  test_mod_pred2 <- id_post_pred(test_mod,newdata=new_data2,
                                 use_cores=floor(parallel::detectCores()/2),
                                 item_subset=levels(new_data1$item_id),
                                 type="epred",
                                 draws=draws)
  
  saveRDS(test_mod_pred1, "data/unemp_all_pred1.rds")
  saveRDS(test_mod_pred2, "data/unemp_all_pred2.rds")
  
  # walk over both predictions to get item and overall effects
  # AMEs per item
  
  print("Looping over items")
  
  c1 <- purrr::map2(test_mod_pred1[[1]],
                    test_mod_pred2[[1]],
                    function(small,big) {
                      
                      # difference the effects
                      
                      (big - small) / eps
                      
                    })
  
  c2 <- lapply(c1, function(mat) {
    
    mat <- t(mat)
    out_data <- attr(mat, "data")
    colnames(mat) <- out_data$person_id
    
    as_tibble(mat) %>% 
      mutate(draws=1:n(),
             item_id=unique(out_data$item_id)) %>% 
      gather(key="person_id",value="estimate",-draws,-item_id) %>% 
      mutate(person_id=as.numeric(person_id),
             estimate=as.numeric(estimate))
    
  }) %>% bind_rows
  
  c3 <- lapply(c1, function(mat) {
    
    
    apply(mat, 1, mean)
    
  })
  
  old_style_version <- tibble(item_id=sapply(c1, function(c) unique(attr(c, "data")$item_id)),
                              mean_est=sapply(c3, mean),
                              low_est=sapply(c3, quantile, .05),
                              high_est=sapply(c3, quantile, .95))
  
  # merge in some original data
  to_merge <- mutate(test_mod@score_data@score_matrix, 
                     item_orig=item_id,
                     person_orig=person_id,
                     person_id=as.numeric(person_id),
                     item_id=as.numeric(item_id)) %>% 
    select(person_id, item_id, group_id,item_orig, person_orig) %>% 
    distinct
  
  c2 <- left_join(c2, to_merge, 
                  by=c("item_id","person_id"))
  
  # get effect separately by democrats/republicans
  
  by_party <- group_by(c2, draws, group_id, item_id, item_orig) %>% 
    summarize(mean_est1=mean(estimate)) %>% 
    group_by(group_id, item_id, item_orig) %>% 
    summarize(mean_est=mean(mean_est1),
              low_est=quantile(mean_est1, .05),
              high_est=quantile(mean_est1, .95))
  
  # merge in item discrimination
  
  item_discrim <- filter(test_mod@summary,
                         grepl(x=variable, pattern="sigma\\_reg\\_free")) %>% 
    mutate(item_id=as.numeric(str_extract(variable, "[0-9]+")))
  
  by_party <- left_join(by_party,
                        select(item_discrim, median, item_id))
  
  saveRDS(by_party,"data/by_party.rds")
  
  by_party %>% 
    mutate(group_id=factor(group_id,levels=c("D","R"),
                           labels=c("Democrats", "Republicans"))) %>% 
    filter(!(item_orig %in% c("115_1050","115_588"))) %>% 
    ggplot(aes(y=mean_est,
               x=reorder(item_id,mean_est))) +
    geom_linerange(aes(ymin=low_est,
                       ymax=high_est,
                       colour=`median`)) +
    facet_wrap(~group_id) +
    ggthemes::theme_tufte() + 
    scale_colour_viridis_c(name="Discrimination") +
    coord_flip() +
    labs(y="Marginal Change in Probability of Voting",
         x="Rollcalls") +
    geom_hline(yintercept=0,linetype=2) +
    ggdark::dark_mode() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle("Marginal Effect of District Monthly Unemployment on Rollcall Votes in 115th Congress",
            subtitle="Marginal Effect of Unemployment Mediated by Legislator Ideal Point and Bill Discrimination")
  
  ggsave(paste0("/lustre/scratch/rkubinec/rollcalls_all_",m,".jpg"),height=7,width=7)
  
  by_party %>% 
    ungroup %>% 
    mutate(group_id=factor(group_id,levels=c("D","R"),
                           labels=c("Democrats", "Republicans")),
           item_rank=rank(mean_est)) %>% 
    filter(!(item_orig %in% c("115_1050","115_588"))) %>% 
    ggplot(aes(y=mean_est,
               x=item_rank)) +
    geom_ribbon(aes(ymin=low_est,
                    ymax=high_est,
                    fill=group_id),alpha=0.5) +
    #facet_wrap(~group_id) +
    ggthemes::theme_tufte() + 
    #scale_fill_viridis_c(name="Discrimination") +
    coord_flip() +
    labs(y="Marginal Change in Probability of Voting",
         x="Rollcalls") +
    geom_hline(yintercept=0,linetype=2) +
    #ggdark::dark_mode() +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle("Marginal Effect of District Monthly Unemployment on Rollcall Votes in 115th Congress",
            subtitle="Marginal Effect of Unemployment Mediated by Legislator Ideal Point and Bill Discrimination")
  
  ggsave(paste0("/lustre/scratch/rkubinec/rollcalls_overlay_all_",m,".jpg"),height=7,width=7)


# test_mod <- id_estimate(unemp1_obj,model_type=2,
#                           vary_ideal_pts = 'splines',
#                           niters=300,
#                           warmup=600,
#                           spline_knots=NULL,
#                           spline_degree = 3,
#                           nchains=2,
#                           ncores=parallel::detectCores(),
#                           const_type = "items",prior_only = FALSE,
#                           restrict_ind_high = c("115_251","115_986","115_1","115_632"),
#                           restrict_sd_high = .001,
#                           restrict_sd_low = .001,restrict_var = F,
#                           restrict_ind_low=c("115_433","115_988","115_4","115_396"),
#                         person_sd=1,
#                         time_var = 1,
#                           fixtype="prefix",
#                           adapt_delta=0.95,
#                           # pars=c("steps_votes_grm",
#                           #        "steps_votes",
#                           #        "B_int_free",
#                           #        "A_int_free"),
#                           #include=F,
#                           id_refresh=100)
# 
# saveRDS(test_mod, "data/unemp1_test.rds")

# full predicted distribution

