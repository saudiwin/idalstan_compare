library(idealstan)
library(ggplot2)
library(patchwork)

unemp1_all_fit <- readRDS('data/all1_12_1_fit.rds')
unemp1_all_fitm <- readRDS('data/all2_12_1_fit.rds')

# get time_varying scores then average them

legis_var_nomiss <- summary(unemp1_all_fit, aggregated=FALSE)
