library(idealstan)
library(ggplot2)
library(patchwork)

unemp1_all_fit <- readRDS('/work/rkubinec/all1_12_1_fit.rds')
unemp2_all_fit <- readRDS('/work/rkubinec/unempall1_12_2_fit.rds')
unemp3_all_fit <- readRDS('/work/rkubinec/unempall1_12_3_fit.rds')

# do plots/rhats here

un1big <- id_plot_legis_dyn(unemp1_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 2-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1big, "/work/rkubinec/un1big.rds")

un2big <- id_plot_legis_dyn(unemp2_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 3-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2big, "/work/rkubinec/un2big.rds")

un3big <- id_plot_legis_dyn(unemp3_all_fit,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                         D="blue"),na.translate=F,name="") + ggtitle("Observed: 4-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3big, "/work/rkubinec/un3big.rds")

unemp1_all_fitm <- readRDS('/work/rkubinec/unempall2_12_1_fit.rds')

un1bigm <- id_plot_legis_dyn(unemp1_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 2-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un1bigm, "/work/rkubinec/un1bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp2_all_fitm <- readRDS('/work/rkubinec/unempall2_12_2_fit.rds')

un2bigm <- id_plot_legis_dyn(unemp2_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 3-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un2bigm, "/work/rkubinec/un2bigm.rds")

rm(list=ls()[grepl(x=ls(),pattern = "unemp")])

unemp3_all_fitm <- readRDS('/work/rkubinec/unempall2_12_3_fit.rds')

un3bigm <- id_plot_legis_dyn(unemp3_all_fitm,use_ci=F,plot_text = F,person_line_alpha = 0.1) + scale_color_manual(values=c(R="red",
                                                                                                                           D="blue"),na.translate=F,name="") + ggtitle("Missing: 4-Degree Spline") + labs(y="") +
  scale_x_date(guide = guide_axis(n.dodge = 2))

saveRDS(un3bigm, "/work/rkubinec/un3bigm.rds")

# make big plot and save it

un1big + un2big + un3big + 
  un1bigm + un2bigm + un3bigm + 
  plot_layout(nrow=2,guides="collect") + 
  plot_annotation(caption="Posterior average values shown for 115th Congress legislator ideal points.\nItem discrimination for Republican party-line votes was constrained to be positive.") & theme(legend.position = "top",plot.title=element_text(size=9))

ggsave("full_dist.pdf",width=6,height=5)
