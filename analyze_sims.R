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
  group_by(model, numitems, task_time, sim) %>% 
  mutate(sign_rotation=ifelse(mean(rse(ideal_point,true_ideal_point),na.rm=T)>mean((rse(-1*ideal_point,
                                                                      true_ideal_point)),na.rm=T),
                              1,0)) %>% 
  ungroup %>% 
         mutate(coef_rmse=rse(as.numeric(true_est_coef),as.numeric(est_coef)),
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

# sim_draws %>% 
#   distinct(model,true_est_coef, est_coef, timeproc,missingness) %>% 
#  # filter(timeproc=="GP") %>% 
#   ggplot(aes(y=true_est_coef,
#              x=est_coef)) +
#   geom_point() +
#   facet_wrap(~model) + 
#   geom_abline(slope=1, intercept=0, linetype=2, colour="red", size=2)

calc_sum %>% 
  group_by(model) %>% 
  summarize(mean_cov=mean(cov_ideal,na.rm=T))

calc_sum %>% 
  group_by(model,missingness) %>% 
  summarize(mean_rmse=mean(mean_rmse,na.rm=T)) %>% 
  arrange(mean_rmse)

calc_sum %>% 
  group_by(model,missingness) %>% 
  summarize(mean_s=mean(s_error,na.rm=T)) %>% 
  arrange(mean_s)
  
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

out_sign_rotate <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(model, missingness, timeproc, sign_rotation,numitems, timevar,timepoints,
           sim) %>% 
  group_by(model, missingness, timeproc) %>%
  summarize(list_var=list(Hmisc::smean.cl.boot(sign_rotation))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  select(-list_var)

p5 <-  out_sign_rotate %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~timeproc,scales="free_y") +
  #geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top")

saveRDS(p5,"data/plot_sign_rotate.rds")

ggsave("plots/sign_rotate.png",plot=p5)

p6 <- sim_draws %>% 
  mutate(time_elapsed=as.numeric(time_elapsed)/60) %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(time_elapsed, timeproc, model) %>% 
  group_by(timeproc, model) %>% 
  summarize(mean_time=mean(time_elapsed,na.rm=T)) %>% 
         mutate(time_label=round(mean_time, 2)) %>% 
  ggplot(aes(y=mean_time,
             x=reorder(model, mean_time))) +
  geom_col() +
  geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~timeproc) +
  labs(y="Average # of Minutes per Run",
       x="") +
  ggthemes::theme_clean()

saveRDS(p6,"data/plot_time_elapsed.rds")

ggsave("plots/time_elapsed.png",plot=p6)

# S Errors

s_err_data <- sim_draws %>% 
  mutate(time_elapsed=as.numeric(time_elapsed)/60) %>% 
  mutate(missingness=factor(missingness,labels=c("Ignorable","Non-ignorable")),
         timeproc=factor(timeproc, levels=c("AR","GP","randomwalk","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, timevar, missingness, timeproc, numitems,
           timepoints,sign_rotation,sim,est_coef,true_est_coef) %>%
  distinct %>% 
  summarize(s_error=mean(ifelse(sign_rotation,
                                est_coef_pval<0.05 & sign(est_coef) == sign(true_est_coef),
                                est_coef_pval<0.05 & sign(est_coef) != sign(true_est_coef)),na.rm=T)) %>% 
  group_by(timeproc, model) %>% 
  summarize(mean_s_err=mean(s_error,na.rm=T))

p7 <- s_err_data %>% 
  ggplot(aes(y=mean_s_err,
             x=reorder(model, mean_s_err))) +
  geom_col() +
  #geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~timeproc) +
  labs(y="Proportion of S Errors",
       x="") +
  ggthemes::theme_clean()

saveRDS(p7,"data/s_errors.rds")

ggsave("plots/s_errors.png",plot=p7)
