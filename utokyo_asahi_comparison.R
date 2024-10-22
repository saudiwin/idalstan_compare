# look at UTokyo-Asahi survey data
# generously provided by Yuki Shuraito

library(idealstan)
library(emIRT)
library(tidyverse)
library(readr)

rando_seed <- 20241020

set.seed(rando_seed)

data("AsahiTodai")

# estimate with emIRT

out.varinf <- ordIRT(.rc = AsahiTodai$dat.all, .starts = AsahiTodai$start.values,
                     .priors = AsahiTodai$priors, .D = 1,
                     .control = {list(verbose = TRUE,
                                      thresh = 1e-6, maxit = 500)})

# use the min/max from this to fix the items high/low

restrict_ind_high <- which(out.varinf$means$beta[,1]==max(out.varinf$means$beta[,1]))
restrict_ind_low <- which(out.varinf$means$beta[,1]==min(out.varinf$means$beta[,1]))

# load original data

#asahi_orig <- read_csv("data/dat_full.csv")

# get original data that matches names from data fed to emIRT

#asahi_orig_em <- select(asahi_orig, one_of(colnames(AsahiTodai$dat.all)))

# check if they match

# check_mat <- dplyr::filter(as_tibble(AsahiTodai$dat.all),
#                            complete.cases(as_tibble(AsahiTodai$dat.all)))
# 
# identical(as.matrix(check_mat),AsahiTodai$dat.all)

# we have a match, proceed with modeling the full missing dataset
# convert to long form for idealstan

# asahi_ideal <- mutate(asahi_orig_em,
#                       person_id=1:n()) %>% 
#   gather(key = "item_id",
#          value="outcome_disc",
#          -person_id)

# we have to dichotomize down to 3 categories

# asahi_ideal_3cat <- mutate(asahi_ideal, outcome_disc=case_match(outcome_disc,
#                                                            1~1,
#                                                            2~1,
#                                                            3~3,
#                                                            4~5,
#                                                            5~5,
#                                                            NA_real_~NA_real_))

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

asahi_em_ideal <- id_make(asahi_em_ideal)

# give it a go with a small dataset

asahi_est <- id_estimate(asahi_em_ideal,restrict_ind_high=names(restrict_ind_high),
                         restrict_ind_low=names(restrict_ind_low),
                         const_type = "items",
                         restrict_N_high = 5000,
                         restrict_N_low=5000,
                         nchains = 3,
                         ncores=parallel::detectCores())

saveRDS(asahi_est,"/lustre/scratch/rkubinec/asahi_est.rds")
