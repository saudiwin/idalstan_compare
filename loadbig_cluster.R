library(idealstan)
library(ggplot2)

unemp1_all_fit <- readRDS('/lustre/scratch/rkubinec/all1_12_1_fit.rds')
unemp2_all_fit <- readRDS('/lustre/scratch/rkubinec/unempall1_12_2_fit.rds')
unemp3_all_fit <- readRDS('/lustre/scratch/rkubinec/unempall1_12_3_fit.rds')

# do plots/rhats here

un1big <- id_plot_legis_dyn(unemp1_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Spline of 2nd Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1big, "/lustre/scratch/rkubinec/un1big.rds")

un2big <- id_plot_legis_dyn(unemp2_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Spline of 3rd Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2big, "/lustre/scratch/rkubinec/un2big.rds")

un3big <- id_plot_legis_dyn(unemp3_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Spline of 4th Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3big, "/lustre/scratch/rkubinec/un3big.rds")

# need legis covariates

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp1_all_fitm <- readRDS('/lustre/scratch/rkubinec/unempall2_12_1_fit.rds')

un1bigm <- id_plot_legis_dyn(unemp1_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Spline of 2nd Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1bigm, "/lustre/scratch/rkubinec/un1bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp2_all_fitm <- readRDS('/lustre/scratch/rkubinec/unempall2_12_2_fit.rds')

un2bigm <- id_plot_legis_dyn(unemp2_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Spline of 3rd Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2bigm, "/lustre/scratch/rkubinec/un2bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp3_all_fitm <- readRDS('/lustre/scratch/rkubinec/unempall2_12_3_fit.rds')

un3bigm <- id_plot_legis_dyn(unemp3_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Spline of 4th Degree") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3bigm, "/lustre/scratch/rkubinec/un3bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])