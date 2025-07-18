#!/usr/bin/env Rscript

# Load necessary libraries
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)

# ------------------------------------------------------------------------------
# Create a tibble where each row represents one simulation's parameters.
# ------------------------------------------------------------------------------
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

# filter out ones already completed

miss_sims <- readRDS("miss_sims.rds")

simulations <- filter(simulations, 
                      iter %in% miss_sims$iter)

# For debugging: print the tibble of simulation parameters
print(simulations)

# ------------------------------------------------------------------------------
# Define a function to submit one Slurm job given a row from the tibble.
#
# The function constructs an sbatch command that passes the parameters via
# the --export flag. Note that we convert logical values (missingness) to
# integers (0/1) for export.
# ------------------------------------------------------------------------------
submit_slurm_job <- function(sim_row) {
  # Build the command string using sprintf for clarity
  cmd <- sprintf(
    "sbatch -n 4 -t 168:00:00 -N 1 -p extended --job-name=%s --output=%s.out --error=%s.out --export=NSIMS=%s,NPERSON=%s,NITEM=%s,TIMEPOINTS=%s,TIMESD=%s,TRUECOEF=%s,TIMEPROC=%s,MISSING=%s,ITER=%s  compare_mods_slurm.sh",
    paste0("is_",sim_row$iter),
    paste0("is_",sim_row$iter,"_output"),
    paste0("is_",sim_row$iter,"_error"),
    sim_row$nsims,
    sim_row$n_persons,
    sim_row$n_items,
    sim_row$time_points,
    sim_row$time_sd,
    sim_row$true_coef,
    sim_row$time_process,
    as.integer(sim_row$missingness),
    sim_row$iter
  )
  
  cat("Submitting job with command:\n", cmd, "\n\n")
  # Execute the command. This call will block until the sbatch command returns.
  system(cmd)
}

# ------------------------------------------------------------------------------
# Iterate over each row of the tibble and submit a Slurm job for that simulation.
#
# We use purrr::pwalk to apply the function to each row.
# ------------------------------------------------------------------------------
pwalk(simulations, function(...) {
  # pwalk passes each row as individual arguments; we capture them in a list
  sim_row <- list(...)
  # Convert to a named list (if not already) to access by name in submit_slurm_job
  sim_row <- as.list(sim_row)
  submit_slurm_job(sim_row)
})
