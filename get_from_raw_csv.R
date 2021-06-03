require(cmdstanr)
require(bayesplot)

get_file <- read_cmdstan_csv("data/irt_standard_map_threads-202105261618-1-5deee7.csv")

get_draws <- get_file$post_warmup_draws

mcmc_trace(get_draws,pars=c("L_full[1]","L_full[2]"))
