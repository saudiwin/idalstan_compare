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
         coef_rmse=rse(as.numeric(true_est_coef),as.numeric(est_coef)),
         rmse_corrected=ifelse(sign_rotation,
                               rse(-1*ideal_point,
                                   true_ideal_point),
                               rse(ideal_point,true_ideal_point)),
         coef_rmse_corrected=ifelse(sign_rotation,
                                    rse(-1*as.numeric(true_est_coef),
                                        as.numeric(est_coef)),
                                    rse(as.numeric(true_est_coef),as.numeric(est_coef))),
         coverage_correct=ifelse(sign_rotation,
                                 as.numeric(-1 * true_ideal_point > ideal_point_low & -1*true_ideal_point < ideal_point_high),
                                 as.numeric((true_ideal_point > ideal_point_low) & (true_ideal_point < ideal_point_high))))

calc_sum <- group_by(sim_draws, model, timevar, missingness, timeproc, numitems,
                     timepoints) %>% 
  summarize(mean_rmse=mean(rmse_corrected, na.rm=T),
            coef_rmse=mean(coef_rmse_corrected, na.rm=T),
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
  
  out_rmse <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, timeproc) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(rmse))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  select(-list_var)

  p1 <- out_rmse %>% ggplot(aes(y=mean_est,
                                x=reorder(model,mean_est))) +
    geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                        linetype=missingness,
                        colour=missingness),
                    position=position_dodge(.5)) +
    facet_wrap(~timeproc,scales="free_y") +
    labs(y="RMSE",x="",
         caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                   width=60)) + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    ggthemes::theme_clean() +
    theme(legend.position = "top")

saveRDS(p1,"data/plot_rmse_all.rds")

ggsave("plots/rmse_all.png",plot=p1)


  
  out_rmse_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, timeproc) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(rmse_corrected))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  select(-list_var)

  p2 <-  out_rmse_correct %>% 
ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")

saveRDS(p2,"data/plot_rmse_all_corrected.rds")

ggsave("plots/rmse_all_corrected.png",plot=p2)

# now with coef rmse

out_coef_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, timeproc) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(coef_rmse_corrected))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  select(-list_var)

p3 <-  out_coef_correct %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")

saveRDS(p3,"data/plot_coef_corrected.rds")

ggsave("plots/coef_corrected.png",plot=p3)

# coverage

out_coverage_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, timeproc) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(coverage_correct))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  select(-list_var)

p4 <-  out_coverage_correct %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y") +
  geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")

saveRDS(p4,"data/plot_coverage_corrected.rds")

ggsave("plots/coverage_corrected.png",plot=p4)

sim_draws %>% 
  mutate(missingness=factor(missingness)) %>% 
  ggplot(aes(y=coef_rmse_corrected,
             x=reorder(model,coef_rmse))) +
  stat_summary(fun.data="mean_cl_normal",
               aes(linetype=missingness,
                   colour=missingness),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y")

sim_draws %>% 
  mutate(missingness=factor(missingness)) %>% 
  ggplot(aes(y=sign_rotation,
             x=reorder(model,sign_rotation))) +
  stat_summary(fun.data="mean_cl_normal",
               aes(linetype=missingness,
                   colour=missingness),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y")

sim_draws %>% 
  mutate(timepoints=factor(timepoints)) %>% 
  ggplot(aes(y=sign_rotation,
             x=reorder(model,sign_rotation))) +
  stat_summary(fun.data="mean_cl_normal",
               aes(linetype=timepoints,
                   colour=timepoints),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y")

# now try for number of items

sim_draws %>% 
  ggplot(aes(y=rmse,
             x=reorder(numitems,rmse))) +
  stat_summary(fun.data="mean_cl_normal",
               aes(colour=model,linetype=model),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y")

# coverage

sim_draws %>% 
  mutate(missingness=factor(missingness)) %>% 
  ggplot(aes(y=in_interval,
             x=reorder(model,in_interval))) +
  stat_summary(fun.data="mean_cl_normal",
               aes(colour=missingness,linetype=missingness),
               position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y") +
  geom_hline(yintercept = .95, linetype=2)

# time

mean_fun <- function(x) {
  
  round(mean(x,na.rm=T),1)
  
}

sim_draws %>% 
  mutate(time_elapsed=as.numeric(time_elapsed)/60) %>% 
  ggplot(aes(y=time_elapsed,
             x=reorder(timeproc, time_elapsed))) +
  stat_summary(fun.data="mean_cl_normal",
               #aes(colour=model,linetype=model),
               position=position_dodge(.5)) +
  facet_wrap(~model) +
  stat_summary(fun=mean_fun, geom="text",vjust=1, aes(label=..y..)) +
  labs(y="Average # of Minutes per Run",
       x="") +
  scale_y_log10()


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
