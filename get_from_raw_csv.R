require(cmdstanr)
require(idealstan)
require(bayesplot)
require(posterior)
require(tidyverse)

unemp1_rw1 <- readRDS("data/unemp1_run1fit.rds")
unemp1_rw2 <- readRDS("data/unemp1_run2fit.rds")


# combine draws and re-calculate Rhats

unemp1_combine <- bind_draws(unemp1_rw1@stan_samples$draws("L_full"),
                             unemp1_rw2@stan_samples$draws("L_full"),along="chain")

unemp1_combine_discrim <- bind_draws(unemp1_rw1@stan_samples$draws("sigma_reg_free"),
                             unemp1_rw2@stan_samples$draws("sigma_reg_free"),along="chain")

c1 <- summarize_draws(unemp1_combine)
c2 <- summarize_draws(unemp1_combine_discrim)
c3 <- bind_draws(unemp1_rw1@stan_samples$draws("L_tp1_var"),
                 unemp1_rw2@stan_samples$draws("L_tp1_var"),along="chain") %>% 
  summarize_draws


mcmc_rhat(unemp1_combine)

mcmc_rhat(unemp1_fit@stan_samples$draws("L_full"))
