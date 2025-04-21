# analyze simulation results

library(tidyverse)
library(ggplot2)
library(lubridate)

all_sims <- list.files("data/",pattern="sim\\_models\\_",
                       full.names = T)

# convert to data 

# Function to extract parameters from filename and read data
read_and_annotate <- function(filename) {
  # Remove the .rds extension
  base <- str_remove(basename(filename), "\\.rds$")
  
  # Remove the leading "sim_models_" part
  params_str <- str_remove(base, "^sim_models_")
  
  # fix random walk
  
  params_str <- str_replace_all(params_str, "random(_?walk)?", "randomwalk")
  
  # Split into key-value pairs
  parts <- str_split(params_str, "_", simplify = TRUE)
  
  # Convert alternating keys and values into a named list
  keys <- parts[seq(1, ncol(parts), 2)]
  values <- parts[seq(2, ncol(parts), 2)]
  
  if(! ("timepoints" %in% keys)) return(NULL)
  
  # Create a named list of parameter values
  param_list <- as.list(values)
  names(param_list) <- keys
  
  # Read in the RDS file
  df <- readRDS(filename)
  
  if(all(sapply(df, function(c) 'try-error' %in% class(c)))) return(NULL)
  
  # null out the try errors
  
  df <- lapply(df, function(item) {
    
    if('try-error' %in% class(item)) return(NULL)
    
    item
    
  })
  # get rid of runs that failed
  if(all(sapply(df, is.null))) return(NULL)
  
  df <- df %>% bind_rows %>% 
  bind_cols(as_tibble(param_list)) %>% 
    mutate(across(one_of(c("nsims","missingness",'timevar','numpers','numitems',
                           "timepoints")),
           as.numeric))
  
  if(df$numpers[1]==30) return(NULL)
  
  return(df)
}

sim_draws <- lapply(all_sims, function(i) {
  
  this_data <- read_and_annotate(i)
  
}) %>% bind_rows %>% ungroup

rmse <- function(col1,col2) {
  
  mean(sqrt((col1 - col2)^2),na.rm=T)
  
}

rse <- function(col1,col2) {
  
  sqrt((col1 - col2)^2)
  
}

# add in sign_rotation parameter

sim_draws <- ungroup(sim_draws) %>% 
  mutate(sign_rotation=ifelse(rse(ideal_point,true_ideal_point)>(rse(-1*ideal_point,
                                                                      true_ideal_point)),
                              1,0),
         coef_rmse=rse(as.numeric(true_est_coef),as.numeric(est_coef)))

calc_sum <- group_by(sim_draws, model, timevar, missingness, timeproc, numitems,
                     timepoints) %>% 
  summarize(mean_rmse=mean(rmse[!sign_rotation], na.rm=T),
            coef_rmse=mean(coef_rmse[!sign_rotation], na.rm=T),
            elapsed_time=mean(as.numeric(time_elapsed), na.rm=T),
            cov_ideal=mean(in_interval[!sign_rotation], na.rm=T),
            s_error=mean(est_coef_pval[!sign_rotation]<0.05 & sign(est_coef[!sign_rotation]) != sign(true_est_coef[!sign_rotation]),
                         na.rm=T),
            m_error=mean(ifelse(est_coef_pval[!sign_rotation]<0.05,
                                abs(est_coef[!sign_rotation]) / abs(true_est_coef[!sign_rotation]),
                                NA_real_), na.rm=T),
            missing_CIs=mean(is.na(ideal_point_low)),
            missing_est=mean(is.na(ideal_point)),
            sign_rotate=mean(sign_rotation,na.rm=T))

calc_sum_norotate <- filter(sim_draws, sign_rotation==0) %>% 
  group_by(model, timevar, missingness, timeproc, numitems) %>% 
  summarize(mean_rmse=mean(rmse, na.rm=T),
            coef_rmse=mean(coef_rmse, na.rm=T),
            elapsed_time=mean(as.numeric(time_elapsed), na.rm=T),
            cov_ideal=mean(in_interval, na.rm=T),
            s_error=mean(est_coef_pval<0.05 & sign(est_coef) != sign(true_est_coef),
                         na.rm=T),
            m_error=mean(ifelse(est_coef_pval<0.05,
                                abs(est_coef) / abs(true_est_coef),
                                NA_real_), na.rm=T),
            missing_CIs=mean(is.na(ideal_point_low)),
            missing_est=mean(is.na(ideal_point)),
            sign_rotate=mean(sign_rotation,na.rm=T))

missing_rmse_norotate <- filter(sim_draws, sign_rotation==0) %>% 
  group_by(model,missingness) %>% 
  summarize(mean_rmse=mean(rmse, na.rm=T),
            coef_rmse=mean(coef_rmse, na.rm=T))

sim_draws %>% 
  distinct(model,true_est_coef, est_coef, timeproc,missingness) %>% 
 # filter(timeproc=="GP") %>% 
  ggplot(aes(y=true_est_coef,
             x=est_coef)) +
  geom_point() +
  facet_wrap(~model) + 
  geom_abline(slope=1, intercept=0, linetype=2, colour="red", size=2)

# plot coverage rates

calc_sum %>% 
  ggplot(aes(x=cov_ideal)) +
  geom_histogram() +
  facet_wrap(~model) +
  geom_vline(xintercept = .95, linetype=2)

calc_sum %>% 
  #filter(timeproc!="GP") %>% 
  ggplot(aes(x=mean_rmse)) +
  geom_histogram() +
  facet_wrap(~model)

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_cov=mean(cov_ideal,na.rm=T))

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_rmse=mean(mean_rmse,na.rm=T)) %>% 
  arrange(mean_rmse)

calc_sum %>% 
  group_by(model,missingness) %>% 
  summarize(mean_rmse=mean(mean_rmse,na.rm=T)) %>% 
  arrange(mean_rmse)

calc_sum %>% 
  group_by(model,missingness) %>% 
  summarize(mean_s=mean(s_error,na.rm=T)) %>% 
  arrange(mean_s)

calc_sum %>% 
  group_by(model,missingness) %>% 
  summarize(mean_rmse=mean(mean_rmse,na.rm=T)) %>% 
  arrange(mean_rmse)

sim_draws %>% 
  ggplot(aes(y=rmse,
             x=reorder(model,rmse))) +
  stat_summary(fun.data="mean_cl_normal") +
  facet_wrap(~timeproc)

sim_draws %>% 
  ggplot(aes(y=coef_rmse,
             x=reorder(model,coef_rmse))) +
  stat_summary(fun.data="mean_cl_normal") +
  facet_wrap(~timeproc)

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_rmse=mean(mean_rmse,na.rm=T)) %>% 
  arrange(mean_rmse)

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_s_error=mean(s_error,na.rm=T)) %>% 
  arrange(mean_s_error)

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_m_error=mean(m_error,na.rm=T)) %>% 
  arrange(mean_m_error)

sim_draws %>% 
  ggplot(aes(x=ideal_point)) +
  geom_histogram() +
  facet_wrap(~model)

sim_draws %>% 
  mutate(timevar=factor(timevar)) %>% 
  ggplot(aes(x=rmse)) +
  geom_density(aes(fill=timevar),alpha=0.5) +
  facet_wrap(~model)

# coverage

calc_sum %>%
  ggplot(aes(x=cov_ideal)) +
  geom_histogram() +
  facet_wrap(~model)



sim_draws %>% 
  group_by(model) %>% 
  mutate(mean_rmse=mean(rmse,na.rm=T)) %>% 
  ggplot(aes(x=rmse)) +
  geom_histogram() +
  geom_vline(aes(xintercept=mean_rmse),linetype=2,colour="red") +
  facet_wrap(~model + timevar)

sim_draws %>% 
  group_by(model,sim) %>% 
  summarize(mean_rmse=mean(rmse, na.rm=T),
coef_rmse=rmse(as.numeric(true_est_coef) - as.numeric(est_coef))) %>% 
  ggplot(aes(x=coef_rmse,y=mean_rmse)) +
  geom_point() +
  facet_wrap(~model)

# see if DW NOMINATE convergence failures are correlated with greater rmse

wide_sims <- sample_n(ungroup(sim_draws), 3000) 

wide_sims <- wide_sims %>% 
  dplyr::select(model, sim, person_id, time_id, rmse,
                missingness,timevar,timeproc,nsims) %>% 
  spread(key = "model",value="rmse") %>% 
  mutate(dw_miss=as.numeric(is.na(`DW-NOMINATE`)))

dw_errs <- lm(dw_miss ~ idealstan + emIRT + MCMCpack + timevar, data=wide_sims) 

# library(missRanger)
# 
# dw_impute <- missRanger(wide_sims)
# 
# # merge with original data
# 
# wide_sims <- left_join(wide_sims, 
#                        select(dw_impute, dw_impute="DW-NOMINATE", sim:nsims),
#                        by=c("sim",'person_id',"time_id",'missingness',
#                             "timevar",'timeproc'))
# 
# group_by(wide_sims, dw_miss) %>% 
#   summarize(mean_miss_rmse=mean(dw_impute))
