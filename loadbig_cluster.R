library(idealstan)
library(ggplot2)
library(patchwork)
library(tidyverse)

unemp1_all_fit <- readRDS('/work/rkubinec/all1_12_1_fit.rds')

ideal_pts <- summary(unemp1_all_fit, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_11.rds")

unemp2_all_fit <- readRDS('/work/rkubinec/unempall1_12_2_fit.rds')

ideal_pts <- summary(unemp2_all_fit, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_21.rds")

unemp3_all_fit <- readRDS('/work/rkubinec/unempall1_12_3_fit.rds')

ideal_pts <- summary(unemp3_all_fit, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_31.rds")

# do plots/rhats here

un1big <- id_plot_legis_dyn(unemp1_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 2° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1big, "/work/rkubinec/un1big.rds")

un2big <- id_plot_legis_dyn(unemp2_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 3° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2big, "/work/rkubinec/un2big.rds")

un3big <- id_plot_legis_dyn(unemp3_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 4° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3big, "/work/rkubinec/un3big.rds")

unemp1_all_fitm <- readRDS('/work/rkubinec/unempall2_12_1_fit.rds')


ideal_pts <- summary(unemp1_all_fitm, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_12.rds")

un1bigm <- id_plot_legis_dyn(unemp1_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 2° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1bigm, "/work/rkubinec/un1bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp2_all_fitm <- readRDS('/work/rkubinec/unempall2_12_2_fit.rds')

ideal_pts <- summary(unemp2_all_fitm, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_22.rds")

un2bigm <- id_plot_legis_dyn(unemp2_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 3° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2bigm, "/work/rkubinec/un2bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp3_all_fitm <- readRDS('/work/rkubinec/unempall2_12_3_fit.rds')

unemp2_all_fitm <- readRDS('/work/rkubinec/unempall2_12_2_fit.rds')

ideal_pts <- summary(unemp3_all_fitm, aggregated=FALSE) %>% 
  filter(!is.na(Group)) %>% 
  group_by(Group, Time_Point) %>% 
  summarize(mean_est=mean(Ideal_Points),
            high_est=quantile(Ideal_Points, .95),
            low_est=quantile(Ideal_Points, .05))

saveRDS(ideal_pts, "/work/rkubinec/idealpts_32.rds")

un3bigm <- id_plot_legis_dyn(unemp3_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 4° Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3bigm, "/work/rkubinec/un3bigm.rds")

# make big plot and save it

un1big + un2big + un3big + 
  un1bigm + un2bigm + un3bigm + 
  plot_layout(nrow=2,guides="collect") + 
  plot_annotation(caption=stringr::str_wrap("Posterior average values shown for ideal points for U.S. House legislators from 1990 to 2018. Item discrimination for Republican party-line votes was constrained to be positive. Observed data models only used recorded legislator votes while the missing data models incorporated a 2-stage adjustment for legislators who did not show up to vote."),
                  tag_levels="A") & theme(legend.position = "bottom",plot.title=element_text(size=9))

ggsave("full_dist.png",width=6,height=5,dpi = 600)
