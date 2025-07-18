# analyze simulation results

library(tidyverse)
library(ggplot2)
library(lubridate)

# get simulation key

simulations <- expand_grid(nsims=20,
                           ntimes=1:3,
                           true_coef=0.025,
                           n_persons=c(60),
                           n_items=c(100,200,300,400),
                           time_points=c(10,20),
                           time_sd=c(0.25,1),
                           time_process=c("random","GP","splines","AR"),
                           #time_process="AR",
                           missingness=c(0,1)) %>% 
  mutate(iter=1:n())

all_sims <- list.files("data/",pattern="sim\\_models\\_iter",
                       full.names = T)

# figure out which are missing

miss_sims <- filter(simulations, 
                    !(iter %in% as.numeric(stringr::str_extract(all_sims, "[0-9]+"))))

saveRDS(miss_sims, "miss_sims.rds")

# convert to data 

# Function to extract parameters from filename and read data
read_and_annotate <- function(filename) {
  # Remove the .rds extension
  iter_num <- as.numeric(str_extract(filename, "[0-9]+"))
  
  df <- readRDS(filename) %>% 
    lapply(function(x) {
      
      if('try-error' %in% class(x)) return(NULL)
        
        x
      
    }) %>% 
    bind_rows
  
  df$iter <- iter_num
  
  # merge in cats
  
  df <- left_join(dplyr::select(df,-true_coef),
                  simulations,
                  by="iter")
  
  return(df)
}

sim_draws <- lapply(all_sims, function(i) {
  
  print(paste0("now on iter ",i))
  
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
  group_by(model,sim,iter) %>% 
  mutate(sign_rotation=ifelse(sum(rse(ideal_point,true_ideal_point),na.rm=T)>sum((1.2*rse(-1*ideal_point,
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

# make plots

# now do Kendall's tau

out_kendall <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                             labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(iter, sim, model, time_process, 
           missingness) %>% 
  summarize(kendall_tau=ifelse(any(is.na(ideal_point)),
                               NA_real_,
                               ifelse(!sign_rotation,pcaPP::cor.fk(ideal_point, true_ideal_point),pcaPP::cor.fk(-1*ideal_point, true_ideal_point))),
         kendall_tau2=ifelse(any(is.na(ideal_point)),
                             NA_real_,
                             pcaPP::cor.fk(ideal_point, true_ideal_point_raw))) %>% 
  group_by(model, missingness, time_process) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(kendall_tau)),
            list_var2=list(Hmisc::smean.cl.normal(kendall_tau2))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper']),
         mean_est2=sapply(list_var2, function(x) x['Mean']),
         low_ci2=sapply(list_var2, function(x) x['Lower']),
         high_ci2=sapply(list_var2, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p1a <- out_kendall %>% ggplot(aes(y=mean_est,
                              x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows the average Kendall-Tau estimate for the ranks of ideal points from each model relative to the true ranks. Higher values indicate rankings that are closer to the true rankings.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  scale_colour_viridis_d(name="Missingness") +
  scale_linetype(name="Missingness") +
  theme(plot.caption = element_text(size=8))

saveRDS(p1a,"data/kendall_tau.rds")

ggsave("plots/kendall_tau.png",plot=p1a)

p1b <- out_kendall %>% ggplot(aes(y=mean_est2,
                                  x=reorder(model,mean_est2))) +
  geom_pointrange(aes(ymin=low_ci2, ymax=high_ci2,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows the average Kendall-Tau estimate for the ranks of ideal points from each model relative to the true ranks. Higher values indicate rankings that are closer to the true rankings.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  scale_linetype(name="Missingness") +
  scale_colour_viridis_d(name="Missingness") +
  theme(plot.caption = element_text(size=8))

saveRDS(p1b,"data/kendall_tau2.rds")

ggsave("plots/kendall_tau2.png",plot=p1b)


  
  out_rmse_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, time_process) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(rmse_corrected))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
    dplyr::select(-list_var)

  p2 <-  out_rmse_correct %>% 
ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
               position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different time series processes used to generate the dynamic ideal points.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
    scale_linetype(name="Missingness") +
    scale_colour_viridis_d(name="Missingness") +
    theme(plot.caption = element_text(size=8))

saveRDS(p2,"data/plot_rmse_all_corrected.rds")

ggsave("plots/rmse_all_corrected.png",plot=p2)

# now with coef rmse

out_coef_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  ungroup %>% 
  distinct(sim, iter, model, missingness, time_process,
           coef_rmse_corrected) %>% 
  group_by(model, missingness, time_process) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(coef_rmse_corrected))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p3 <-  out_coef_correct %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows the 5% to 95% coverage of estimated ideal points versus true ideal point scores by time process.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  scale_linetype(name="Missingness") +
  scale_colour_viridis_d(name="Missingness") +
  theme(plot.caption = element_text(size=8))

saveRDS(p3,"data/plot_coef_corrected.rds")

ggsave("plots/coef_corrected.png",plot=p3)

# M errors

out_m_err <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(model,missingness, time_process, sign_rotation, est_coef_pval,
           true_est_coef,true_coef,est_coef) %>% 
  group_by(model,missingness) %>% 
  summarize(list_var=list(Hmisc::smean.cl.boot(ifelse(est_coef_pval<0.05,
                                                        abs(est_coef) / abs(true_est_coef),
                                                        NA_real_)))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p3m <-  out_m_err %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  # geom_point(aes(linetype=missingness,colour=missingness),
  #            position=position_dodge(.5)) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  #facet_wrap(~time_process,scales="free_y") +
  labs(y="M Errors",x="",
       caption=stringr::str_wrap("Plot shows the average M-errors of the coefficients of covariates from the regression of latent ideal point estimates from different models. An M-error is defined as the ratio of the estimated coefficient to the true coefficient conditional on the estimate being statistically significant at the 0.05 level.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top",plot.caption = element_text(size=8)) +
  scale_linetype(name="Missingness") +
  scale_colour_viridis_d(name="Missingness")

saveRDS(p3m,"data/plot_m_errors.rds") +
  theme(plot.caption = element_text(size=8))

ggsave("plots/m_errors.png",plot=p3m)

# coverage

out_coverage_correct <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  group_by(model, missingness, time_process) %>% 
  summarize(list_var=list(Hmisc::smean.cl.normal(coverage_correct))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p4 <-  out_coverage_correct %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows average coverage of true ideal points within the 5% to 95% quantiles of the posterior draws or bootstrap draws depending on the model type.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  scale_linetype(name="Missingness") +
  scale_colour_viridis_d(name="Missingness") +
  theme(plot.caption = element_text(size=8))

saveRDS(p4,"data/plot_coverage_corrected.rds")

ggsave("plots/coverage_corrected.png",plot=p4)

out_sign_rotate <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(model, missingness, time_process, sign_rotation,n_items, time_sd,time_points,iter,
           sim) %>% 
  group_by(model, missingness, time_process) %>%
  summarize(list_var=list(Hmisc::smean.cl.boot(sign_rotation))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p5 <-  out_sign_rotate %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  facet_wrap(~time_process,scales="free_y") +
  #geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows average number of times that the estimated ideal points had less RMSE compared to the true ideal points when multiplied by -1. These rotations are likely indicators of convergence to the wrong mode.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  scale_linetype(name="Missingness") +
  scale_colour_viridis_d(name="Missingness") +
  theme(plot.caption = element_text(size=8))

saveRDS(p5,"data/plot_sign_rotate.rds")

ggsave("plots/sign_rotate.png",plot=p5)

out_sign_rotate <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline")),
         time_points=factor(time_points)) %>% 
  distinct(model, time_process, sign_rotation,n_items, time_sd,time_points,
           sim) %>% 
  filter(time_process=="Random Walk") %>% 
  group_by(model, time_points) %>%
  summarize(list_var=list(Hmisc::smean.cl.boot(sign_rotation))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p5a <-  out_sign_rotate %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=time_points,
                      colour=time_points),
                  position=position_dodge(.5)) +
  #facet_wrap(~time_points,scales="free_y") +
  #geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(size=8))

saveRDS(p5a,"data/plot_sign_rotate_timep.rds")

ggsave("plots/sign_rotate_timep.png",plot=p5a)

out_sign_rotate_miss <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline")),
         time_points=factor(time_points)) %>% 
  distinct(model, time_process, sign_rotation,n_items, time_sd,time_points,missingness,
           sim) %>% 
  filter(time_process=="Random Walk") %>% 
  group_by(model, time_points, missingness) %>%
  summarize(list_var=list(Hmisc::smean.cl.boot(sign_rotation))) %>% 
  ungroup %>% 
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper'])) %>% 
  dplyr::select(-list_var)

p5b <-  out_sign_rotate_miss %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model,mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      linetype=missingness,
                      colour=missingness),
                  position=position_dodge(.5)) +
  #facet_wrap(~time_points,scales="free_y") +
  #geom_hline(yintercept = 0.95, linetype=2) +
  labs(y="RMSE",x="",
       caption=stringr::str_wrap("Plot shows averages with 5% to 95% CIs for RMSE of true to estimated ideal point scores. Facets show different true time series processes used to generate the data and for idealstan/Pathfinder/Laplace methods, also used to estimate.",
                                 width=60)) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  ggthemes::theme_clean() +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(size=8))

saveRDS(p5b,"data/plot_sign_rotate_miss.rds")

ggsave("plots/sign_rotate_miss.png",plot=p5b)

p6 <- sim_draws %>% 
  mutate(time_elapsed=as.numeric(time_elapsed)/60) %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(time_elapsed, time_process, model) %>% 
  group_by(time_process, model) %>% 
  summarize(mean_time=mean(time_elapsed,na.rm=T)) %>% 
         mutate(time_label=round(mean_time, 2)) %>% 
  ggplot(aes(y=mean_time,
             x=reorder(model, mean_time))) +
  geom_col() +
  geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~time_process) +
  labs(y="Average # of Minutes per Run",
       x="") +
  ggthemes::theme_clean() + 
  theme(legend.position = "top") +
  theme(plot.caption = element_text(size=8))


saveRDS(p6,"data/plot_time_elapsed.rds")

ggsave("plots/time_elapsed.png",plot=p6)

time_data_n_items <- sim_draws %>% 
  filter(time_process=="random") %>% 
  distinct(time_elapsed, model, n_items) %>% 
  mutate(time_elapsed=as.numeric(time_elapsed)/60) %>% 
  mutate(n_items=paste(n_items, "Items"),
         model=recode(model,
                      `DW-NOMINATE`="DW-\nNOMINATE")) %>% 
  group_by(model, n_items) %>% 
  summarize(mean_time=mean(time_elapsed,na.rm=T)) %>% 
  mutate(time_label=round(mean_time, 2))
  
  p6a <- time_data_n_items %>% 
  ggplot(aes(y=mean_time,
             x=reorder(model, mean_time))) +
  geom_col() +
  geom_text(aes(label=time_label),nudge_y=15,size=3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~n_items) +
  labs(y="Average # of Minutes per Run",
       x="",
       caption=stringr::str_wrap("Plot shows average number of minutes for estimation time by model, including any post-estimation steps like bootstrapping to obtain uncertainty intervals.")) +
  ggthemes::theme_clean() + 
    theme(legend.position = "top") +
    theme(plot.caption = element_text(size=8))
  

saveRDS(p6a,"data/plot_time_elapsed_n_items.rds")

ggsave("plots/time_elapsed_n_items.png",plot=p6a)

# S Errors

s_err_data <- sim_draws %>% 
  mutate(missingness=factor(missingness,
                            labels=c("None","Non-ignorable")),
         time_process=factor(time_process, 
                             levels=c("AR","GP","random","splines"),
                         labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(sign_rotation, model, time_process, iter, sim,
           missingness,
           est_coef_pval, est_coef, true_coef, true_est_coef) %>% 
         mutate(s_errors=ifelse(sign_rotation,
                         est_coef_pval<0.05 & sign(est_coef) == sign(true_est_coef),
                         est_coef_pval<0.05 & sign(est_coef) != sign(true_est_coef)),
         s_errors_true=ifelse(sign_rotation,
                              est_coef_pval<0.05 & sign(est_coef) == sign(true_coef),
                              est_coef_pval<0.05 & sign(est_coef) != sign(true_coef))) %>% 
  group_by(model, time_process,missingness) %>% 
  summarize(list_var=list(Hmisc::smean.cl.boot(s_errors)),
            list_var2=list(Hmisc::smean.cl.boot(s_errors_true))) %>%
  ungroup %>%
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper']),
         mean_est2=sapply(list_var2, function(x) x['Mean']),
         low_ci2=sapply(list_var2, function(x) x['Lower']),
         high_ci2=sapply(list_var2, function(x) x['Upper']))
  


 p7 <- s_err_data %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model, mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci,
                      colour=missingness,
                      linetype=missingness),position = position_dodge(width=.5)) +
  #geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~time_process) +
  labs(y="Proportion of S Errors",
       x="",
       caption=stringr::str_wrap("Plot shows the proportion of S errors, or estimated coefficients of the wrong sign, from the regression of the estimated ideal point latent variable.")) +
  ggthemes::theme_clean() +
   scale_colour_viridis_d(name="Missingness") + 
   scale_linetype(name="Missingness") +
   theme(legend.position = "top") +
   theme(plot.caption = element_text(size=8))
 

saveRDS(p7,"data/s_errors.rds")

ggsave("plots/s_errors.png",plot=p7)

p7a <- s_err_data %>% 
  ggplot(aes(y=mean_est2,
             x=reorder(model, mean_est2))) +
  geom_pointrange(aes(ymin=low_ci2, ymax=high_ci2)) +
  #geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  #facet_wrap(~time_process) +
  labs(y="Proportion of S Errors",
       x="",
       caption=stringr::str_wrap("Plot shows the proportion of S errors, or estimated coefficients of the wrong sign, from the regression of the estimated ideal point latent variable.")) +
  ggthemes::theme_clean() +
  scale_colour_viridis_d(name="Missingness") + 
  scale_linetype(name="Missingness") +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(size=8))


saveRDS(p7a,"data/s_errors_true.rds")

ggsave("plots/s_errors_true.png",plot=p7)

# Power

power_data <- sim_draws %>% 
  mutate(missingness=factor(missingness,labels=c("None","Non-ignorable")),
         time_process=factor(time_process, levels=c("AR","GP","random","splines"),
                             labels=c("AR(1)","Gaussian Process","Random Walk","Spline"))) %>% 
  distinct(sign_rotation, model, time_process, iter, sim,
           est_coef_pval, est_coef, true_coef, true_est_coef) %>% 
  group_by(model, time_process) %>% 
  summarize(list_var=list(Hmisc::smean.cl.boot(est_coef_pval<0.05)),
            list_var2=list(Hmisc::smean.cl.boot(est_coef_pval<0.05))) %>%
  ungroup %>%
  mutate(mean_est=sapply(list_var, function(x) x['Mean']),
         low_ci=sapply(list_var, function(x) x['Lower']),
         high_ci=sapply(list_var, function(x) x['Upper']),
         mean_est2=sapply(list_var2, function(x) x['Mean']),
         low_ci2=sapply(list_var2, function(x) x['Lower']),
         high_ci2=sapply(list_var2, function(x) x['Upper']))

p8 <- power_data %>% 
  ggplot(aes(y=mean_est,
             x=reorder(model, mean_est))) +
  geom_pointrange(aes(ymin=low_ci, ymax=high_ci)) +
  #geom_text(aes(label=time_label),nudge_y=4) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_wrap(~time_process) +
  labs(y="Proportion of S Errors",
       x="") +
  ggthemes::theme_clean() +
  scale_colour_viridis_d(name="Missingness") + 
  scale_linetype(name="Missingness") +
  theme(legend.position = "top") +
  theme(plot.caption = element_text(size=8))

saveRDS(p8,"data/power.rds")

ggsave("plots/power.png",plot=p8)

# let's fit a model to predict S errors


