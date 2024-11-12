# look at UTokyo-Asahi survey data
# generously provided by Yuki Shuraito

library(idealstan)
library(emIRT)
library(tidyverse)
library(readr)
library(haven)

rando_seed <- 20241020

set.seed(rando_seed)

data("AsahiTodai")

# estimate with emIRT

out.varinf <- ordIRT(.rc = AsahiTodai$dat.all, .starts = AsahiTodai$start.values,
                     .priors = AsahiTodai$priors, .D = 1,
                     .control = {list(verbose = TRUE,
                                      thresh = 1e-6, maxit = 500)})

# use the min/max from this to fix the items high/low

# high values = foreign policy should be Asia-centric
# low values = foreign policy should be US-centric

restrict_ind_high <- "constitu"

# High = conservative, aggressive Japanese policy 
# Low = liberal, more peaceful foreign policy

# high values - leave constitution as-is
# low values - revise constitution (allow for independent foreign policy)

restrict_ind_low <- "foreign"

# although apparently the original matrix does have missing data,
# it just codes it as zero
# so let's use the package's original missing data

asahi_em_ideal <- mutate(as_tibble(AsahiTodai$dat.all),
                         person_id=1:n()) %>% 
  # sample_n(500) %>% 
  gather(key = "item_id",
         value="outcome_disc",
         -person_id) %>% 
  mutate(outcome_disc=na_if(outcome_disc, 0),
         ordered_id=3,
         model_id=4)

# need to filter out NAs where the item is missing for the whole survey wave
# sometimes Qs only available for voters or candidates but not vice versa
# no need to consider these NAs in the same way
# use external covariates from emIRT

ext_cov <- AsahiTodai$obs.attri %>% 
  mutate(person_id=1:n())

asahi_em_ideal <- left_join(asahi_em_ideal,ext_cov,
                            by="person_id")

asahi_em_ideal_small <- group_by(asahi_em_ideal,
                           wave,voter,item_id) %>% 
  filter(!all(is.na(outcome_disc)))

# see which Qs have the highest missingness

asahi_em_ideal_small %>% 
  group_by(item_id) %>% 
  summarize(count_nas=paste0(round(sum(is.na(outcome_disc))/n(),3)*100,"%")) %>% 
  arrange(desc(count_nas))

# see if it varies by wave

asahi_em_ideal_small %>% 
  group_by(wave,item_id) %>% 
  summarize(count_nas=sum(is.na(outcome_disc))) %>% 
  arrange(desc(count_nas))

asahi_em_ideal_small <- id_make(asahi_em_ideal_small,group_id="party")

# give it a go with a small dataset

asahi_est <- id_estimate(asahi_em_ideal_small,restrict_ind_high=names(restrict_ind_high),
                         restrict_ind_low=names(restrict_ind_low),
                         const_type = "items",map_over_id = "items",
                         restrict_N_high = 5000,
                         restrict_N_low=5000,
                         restrict_sd_high = 5000*.010101,
                         restrict_sd_low=5000*.010101,
                         nchains = 3,niters = 300,warmup = 500,
                         ncores=parallel::detectCores())

saveRDS(asahi_est,"/lustre/scratch/rkubinec/asahi_est.rds")

asahi_em_ideal2 <- mutate(as_tibble(AsahiTodai$dat.all),
                         person_id=1:n()) %>% 
  # sample_n(500) %>% 
  gather(key = "item_id",
         value="outcome_disc",
         -person_id) %>% 
  mutate(outcome_disc=na_if(outcome_disc, 0),
         ordered_id=3,
         model_id=3)

asahi_em_ideal2 <- left_join(asahi_em_ideal2,ext_cov,
                            by="person_id")

asahi_em_ideal_small2 <- group_by(asahi_em_ideal2,
                                 wave,voter,item_id) %>% 
  filter(!all(is.na(outcome_disc)))

asahi_em_ideal_small2 <- id_make(asahi_em_ideal_small2)

asahi_est2 <- id_estimate(asahi_em_ideal_small2,restrict_ind_high=names(restrict_ind_high),
                         restrict_ind_low=names(restrict_ind_low),
                         const_type = "items",niters = 500,warmup = 300,
                         map_over_id = "items",
                         restrict_N_high = 5000,
                         restrict_N_low=5000,
                         restrict_sd_high = 5000*.010101,
                         restrict_sd_low=5000*.010101,
                         nchains = 3,
                         ncores=parallel::detectCores())

saveRDS(asahi_est2,"/lustre/scratch/rkubinec/asahi_est2.rds")

# compare the two dists

asahi_est <- readRDS("data/asahi_est.rds")

nomiss_sum <- summary(asahi_est)

# merge with em IRT estimates
# also need to standardize them

nomiss_sum <- arrange(nomiss_sum, Person)

cor(as.numeric(scale(nomiss_sum$`Posterior Median`)), as.numeric(scale(out.varinf$means$x)))

asahi_est2 <- readRDS("data/asahi_est2.rds")

nomiss_sum2 <- summary(asahi_est2)

# merge with em IRT estimates
# also need to standardize them

nomiss_sum2 <- arrange(nomiss_sum2, Person)

cor(nomiss_sum2$`Posterior Median`, out.varinf$means$x)

# make a tibble where we compare with missing data and without missing data

emIRT_res <- tibble(em_estimate=out.varinf$means$x[,1]) %>% 
  mutate(Person=factor(1:n()))

combine_tibble <- bind_rows(tibble(model="idealstan with Missing Data",
                                   estimate=nomiss_sum$`Posterior Median`,
                                   Person=nomiss_sum$Person),
                            tibble(model="idealstan with Only Observed Data",
                                   estimate=nomiss_sum2$`Posterior Median`,
                                   Person=nomiss_sum2$Person))

combine_tibble <- left_join(combine_tibble, emIRT_res,by="Person")

saveRDS(combine_tibble,"data/combine_tibble_emIRT.rds")

combine_tibble %>% 
  ggplot(aes(y=em_estimate,x=estimate)) +
  labs(y="emIRT Ideal Points",x="idealstan Ideal Points",
       caption=stringr::str_wrap("Plot shows survey respondent ideal point estimates from the idealstan package for an IRT ideal point model with only observed data from the Asahi Todai survey and an idealstan model that incorporates non-ignorable missing data. These ideal point estimates are plotted against emIRT estimates, which also only use observed data.")) +
  ggthemes::theme_clean() +
  facet_wrap(~model) +
  stat_smooth(method="lm") +
  ggtitle("Comparison of emIRT to idealstan\nIdeal Point Estimates With and Without Missing Data",
          subtitle="Data from the Asahi Todai Survey (emIRT package)")

ggsave("emirt_vs_idealstan.jpg")

ggplot(aes(y=nomiss_sum$`Posterior Median`,
           x=nomiss_sum2$`Posterior Median`)) +
  geom_point()

# presence of missing data

sort(apply(AsahiTodai$dat.all, 2, function(col) sum(col==0)))

# compare discrimination parameters

discrim_emirt <- tibble(est=out.varinf$means$beta[,1],
                        param=row.names(out.varinf$means$beta))

item_ideal_obs <- summary(asahi_est2,pars="items") %>% 
  filter(grepl(x=`Item Type`,pattern="Non-Inflated Discrimination"))
item_ideal_abs <- summary(asahi_est,pars="items") %>% 
  filter(grepl(x=`Item Type`,pattern="Non-Inflated Discrimination"))

item_ideal_obs %>% 
  left_join(discrim_emirt, by=c("Item Name"="param")) %>% 
  ggplot(aes(y=est,x=`Posterior Median`)) +
  geom_point() +
  ggthemes::theme_clean() +
  geom_abline(slope=1,intercept=0,linetype=2)

item_ideal_abs %>% 
  left_join(discrim_emirt, by=c("Item Name"="param")) %>% 
  ggplot(aes(y=est,x=`Posterior Median`)) +
  geom_point() +
  ggthemes::theme_clean() +
  geom_abline(slope=1,intercept=0,linetype=2)

# compare missingness rates and absence discrimination parameters

item_ideal_abs_discrim <- summary(asahi_est,pars="items") %>% 
  filter(grepl(x=`Item Type`,pattern="\\bInflated Discrimination"),
         !grepl(x=`Item Type`,pattern="Non-Inflated Discrimination"))



miss_data <- asahi_em_ideal_small %>% 
  group_by(item_id) %>% 
  summarize(prop_miss=sum(is.na(outcome_disc))/n()) %>% 
  left_join(select(item_ideal_abs_discrim,`Posterior Median`,item_id="Item Name"))

saveRDS(miss_data, "data/miss_data_emIRT.rds")

miss_data %>% 
  ggplot(aes(x=prop_miss,y=`Posterior Median`,label=param)) +
  geom_text(check_overlap=TRUE) +
  ggthemes::theme_clean() +
  scale_x_continuous(labels=scales::percent) +
  labs(x="Proportion Missing in Survey Item",y="Item Missingness Discrimination",
       caption=stringr::str_wrap("Plot shows proportion of missing data at the question/item level for the Asahi Todai survey on Japanese political issues versus the size of the discrimination parameter for missing data at the item level from the idealstan IRT ideal point model. Large values imply that missingness is non-ignorable (i.e., correlated with the latent trait being estimated).")) +
  ggtitle("Proportion Missing in Survey Question vs.\nIRT Missingness Discrimination",
          subtitle="Data from Asahi Todai Survey (emIRT package)")

ggsave("asahi_todai_missing.jpg")

item_ideal_abs_discrim2 <- summary(asahi_est,pars="items")

item_ideal_abs_discrim2 %>% 
  filter(grepl(x=`Item Type`,pattern="Inflated Discrimination")) %>% 
  select(`Item Type`,`Posterior Median`,`Item Name`) %>% 
  spread(key="Item Type",value=`Posterior Median`) %>% 
  ggplot(aes(y=`Inflated Discrimination`,x=`Non-Inflated Discrimination`)) +
  geom_text(aes(label=`Item Name`),check_overlap = T) +
  geom_smooth(method="lm")

# see if we can merge in ideology and look at comparisons


