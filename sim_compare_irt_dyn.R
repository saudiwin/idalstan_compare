# simulate time series idealstan model and run other models against it
# give them true parameters to constrain

# parallelization libraries

library(doParallel)
library(foreach)
library(parallel)

packages <- c(
  "idealstan",
  "MCMCpack",
  "tidyverse",
  "ggplot2",
  "lubridate",
  "stringr",
  "emIRT",
  "parallel",
  "dwnominate"
)


# simulation parameters ---------------------------------------------------

set.seed(20250310)  # For reproducibility

n_sims <- 500
time_points <- 10
n_persons <- 50
n_items <- 400
time_sd <- 1
true_coef <- .2 # size of coefficient in latent regression
time_process <- "random" # type of time process being simulated
missingness <- FALSE # whether to model missing data

# Define parallelization parameters
cores_per_task <- 4  # Number of cores per task

# Detect available cores
total_cores <- detectCores()
num_workers <- min(n_sims, total_cores / cores_per_task)  # Ensure we don't overload CPU

# Register parallel backend for tasks
cl <- makeCluster(num_workers,outfile="")
registerDoParallel(cl)

# Define simulation function (runs in parallel)
simulate_task <- function(task_id) {
  message(sprintf("Starting Task %d on %d cores...", task_id, cores_per_task))
  
  # simulate dataset with idealstan -----------------------------------------
  
  sim_data <- id_sim_gen(num_person=n_persons,
                         num_items=n_items,
                         time_sd=time_sd,
                         ideal_pts_sd=1,
                         time_process="random",
                         time_points=time_points)
  
  wide_df <- sim_data@score_matrix %>%
    mutate(outcome_disc=as.numeric(outcome_disc)-1) %>% 
    select(person_id, item_id, outcome_disc) %>%
    pivot_wider(names_from = item_id, values_from = outcome_disc, names_prefix = "item_")
  
  wide_df_mat <- as.matrix(wide_df[,-1])
  
  row.names(wide_df_mat) <- paste0("person_",wide_df$person_id)
  
  # Construct the item-time mapping
  item_time_map <- sim_data@score_matrix %>%
    dplyr::select(item_id, time_id) %>%
    distinct() %>%
    arrange(item_id) %>% 
    pull(time_id)
  
  true_ideal <- as_tibble(sim_data@simul_data$true_person) %>% 
    mutate(person_id=as.character(1:n())) %>% 
    gather(key="time_id",value="true_ideal_point",-person_id) %>% 
    mutate(time_id=as.numeric(str_extract(time_id,'[0-9]+')))
  
  # scale these as well
  
  true_ideal <- group_by(true_ideal,
                         time_id) %>% 
    mutate(true_ideal_point=scale(true_ideal_point)) %>% 
    ungroup
  
  
  # generate covariate model ------------------------------------------------
  
  outcome <- rnorm(n=nrow(true_ideal),
                   mean=-2 + true_coef * true_ideal$true_ideal_point)
  
  true_ideal$true_coef <- true_coef
  true_ideal$outcome <- outcome
  
  c1 <- lm(outcome ~ true_ideal_point, data=true_ideal)
  
  true_ideal$true_est_coef <- c1$coefficients[2]
  
  
  # mcmc pack ---------------------------------------------------------------
  
  
  constraint_ids <- c(sort(apply(sim_data@simul_data$true_person,1,mean),
                           decreasing=TRUE,
                           index=TRUE)$ix[1],
                      sort(apply(sim_data@simul_data$true_person,1,mean),
                           decreasing=F, 
                           index=T)$ix[1])
  
  constraint_vals <- c(sort(apply(sim_data@simul_data$true_person,1,mean),
                            decreasing=T)[1],
                       sort(apply(sim_data@simul_data$true_person,1,mean),
                            decreasing=F)[1])
  
  # use same technique for starting values
  
  theta.start <- rep(0, n_persons)
  theta.start[constraint_ids[1]] <- constraint_vals[1]
  theta.start[constraint_ids[2]] <- constraint_vals[2]
  
  const_list <- lapply(constraint_vals, function(x) {x})
  names(const_list) <- paste0("person_",constraint_ids)
  
  start_time <- Sys.time()
  mcmc_pack_fit <- MCMCdynamicIRT1d(wide_df_mat,
                                    item_time_map,
                                    burnin=20000,
                                    mcmc=40000,
                                    theta.start = theta.start,
                                    tau2.start = time_sd,
                                    thin = 20,
                                    A0 = 1,B0 = 1)
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  mcmc_elapsed_time <- end_time - start_time
  
  
  # idealstan ---------------------------------------------------------------
  
  start_time <- Sys.time()
  
  idealstan_fit <- sim_data %>% 
    id_estimate(model_type=1,vary_ideal_pts="random_walk",
                nchains=1,niter=1000,warmup=500,ncores=4,
                restrict_ind_high = as.character(sort(sim_data@simul_data$true_reg_discrim,
                                                      decreasing=T,
                                                      index=T)$ix[1]),
                restrict_ind_low = as.character(sort(sim_data@simul_data$true_reg_discrim,
                                                     decreasing=F, 
                                                     index=T)$ix[1]),
                fix_high = sort(sim_data@simul_data$true_reg_discrim,
                                decreasing=T)[1],fix_low = sort(sim_data@simul_data$true_reg_discrim,
                                                                decreasing=F)[1],
                
                fixtype='prefix',const_type="items")
  
  
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  idealstan_elapsed_time <- end_time - start_time
  
  
  # emIRT -------------------------------------------------------------------
  
  # Convert outcome_disc to dynIRT coding (1 for yea, -1 for nay, 0 for missing)
  df <- sim_data@score_matrix %>%
    mutate(outcome_disc = case_when(
      outcome_disc == 2 ~ 1,   # Convert 2 to 1 (yea)
      outcome_disc == 1 ~ -1,  # Convert 1 to -1 (nay)
      TRUE ~ 0                 # Treat anything else as missing (0)
    ))
  
  
  # Get unique identifiers
  persons <- sort(unique(df$person_id))
  items <- sort(unique(df$item_id))
  time_periods <- sort(unique(df$time_id))
  
  N <- length(persons)  # Number of persons
  J <- length(items)    # Number of items (bills)
  Tpts <- length(time_periods)  # Total number of time periods
  
  # Create mapping indices for persons, items, and time
  person_map <- setNames(seq_along(persons), persons)
  item_map <- setNames(seq_along(items), items)
  time_map <- setNames(seq(0, Tpts - 1), time_periods)  # Terms start at 0 in dynIRT
  
  # Initialize the response matrix (N x J) with 0 (missing data)
  rc_matrix <- matrix(0, nrow = N, ncol = J)
  
  # Fill the response matrix
  for (i in 1:nrow(df)) {
    p_idx <- person_map[[as.character(df$person_id[i])]]
    i_idx <- item_map[[as.character(df$item_id[i])]]
    rc_matrix[p_idx, i_idx] <- df$outcome_disc[i]
  }
  
  # Compute start and end term for each legislator
  startlegis <- sapply(persons, function(p) min(time_map[df$time_id[df$person_id == p]]))
  endlegis <- sapply(persons, function(p) max(time_map[df$time_id[df$person_id == p]]))
  
  # Convert startlegis and endlegis to matrix format
  startlegis <- matrix(startlegis, ncol = 1)
  endlegis <- matrix(endlegis, ncol = 1)
  
  # Compute bill session (J x 1) matrix
  bill_session <- sapply(items, function(i) min(time_map[df$time_id[df$item_id == i]]))
  bill_session <- matrix(bill_session, ncol = 1)
  
  # Create starting values for dynIRT
  alpha <- matrix(rnorm(J), ncol = 1)  # Item difficulty starting values
  beta <- matrix(rnorm(J), ncol = 1)   # Item discrimination starting values
  x <- matrix(0, nrow = N, ncol = Tpts)   # Ideal points (set to 0 for missing terms)
  
  # Define priors
  x_mu0 <- as.matrix(rep(0,n_persons),ncol=1)
  x_mu0[constraint_ids,] <- constraint_vals # Prior means for respondent ideal points
  x_sigma0 <- matrix(time_sd, nrow = N, ncol = 1)  # Prior variance for respondent ideal points
  beta_mu <- matrix(0, nrow = 2, ncol = 1)   # Prior means for item parameters
  beta_sigma <- diag(2)  # Prior covariance matrix for item parameters
  omega2 <- matrix(0.1, nrow = N, ncol = 1)  # Evolutionary variance
  
  # priors
  
  start_time <- Sys.time()
  
  
  em_irt_fit <- dynIRT(.data=list(rc=rc_matrix,
                                  startlegis=startlegis,
                                  endlegis=endlegis,
                                  bill.session=bill_session,
                                  `T`=Tpts),
                       .starts=list(alpha=alpha,
                                    beta=beta,
                                    x=x),
                       .priors=list(x.mu0=x_mu0,
                                    x.sigma0=x_sigma0,
                                    beta.mu=beta_mu,
                                    beta.sigma=beta_sigma,
                                    omega2=omega2),
                       .control = list(
                         threads = 4,
                         verbose = TRUE,
                         thresh = 1e-6,
                         maxit=500,
                         threads=4
                       ))
  
  # get uncertainty
  # need to re-implement parametric bootstrap to account for uncertainty
  
  # Extract estimated parameters
  beta_hat <- em_irt_fit$means$beta  # Bill discrimination parameters
  x_hat <- em_irt_fit$means$x        # Ideal points over time
  alpha_hat <- em_irt_fit$means$alpha      # difficulty
  
  # Number of bootstrap replications
  B <- 200  
  
  # Store bootstrap results
  bootstrap_results <- array(NA, dim = c(N, Tpts, B))
  
  print("Bootstrapping emIRT")
  
  # Parametric Bootstrap Loop
  for (b in 1:B) {
    # Simulate new vote data using the estimated parameters
    simulated_votes <- matrix(NA, nrow = N, ncol = J)
    
    binom_func <- function(prob_yes) {
      
      ans <- rbinom(N, 1, prob_yes)
      
      if(any(ans==0)) ans[ans==0] <- -1
      
      return(ans)
      
    }
    
    for (t in 1:Tpts) {
      for (j in 1:J) {
        prob_yes <- plogis(beta_hat[j] * x_hat[, t] - alpha_hat[j])  # Logistic probability
        simulated_votes[, j] <- binom_func(prob_yes)
      }
    }
    
    # Refit dynIRT model to simulated data
    fit_boot <- dynIRT(
      .data=list(rc=simulated_votes,
                 startlegis=startlegis,
                 endlegis=endlegis,
                 bill.session=bill_session,
                 `T`=Tpts),
      .starts=list(alpha=alpha,
                   beta=beta,
                   x=x),
      .priors=list(x.mu0=x_mu0,
                   x.sigma0=x_sigma0,
                   beta.mu=beta_mu,
                   beta.sigma=beta_sigma,
                   omega2=omega2),
      .control = list(
        threads = 4,
        verbose = FALSE,
        thresh = 1e-6,
        maxit=500,
        threads=4
      )
    )
    
    # Store bootstrapped ideal points
    bootstrap_results[, , b] <- fit_boot$means$x
  }
  
  # Reshape bootstrap results into a long data frame
  bootstrap_long <- as.data.frame.table(bootstrap_results, responseName = "Ideal_Point") %>%
    rename(Legislator = Var1, Time = Var2, Bootstrap = Var3) %>%
    mutate(Legislator = as.integer(Legislator),
           Time = as.integer(Time),
           Bootstrap = as.integer(Bootstrap))
  
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  emirt_elapsed_time <- end_time - start_time
  
  # Compute 95% confidence intervals using quantiles
  em_irt_results <- bootstrap_long %>%
    group_by(Time) %>% 
    mutate(Ideal_Point=as.numeric(scale(Ideal_Point))) %>% 
    group_by(Legislator, Time) %>%
    summarise(
      Mean_Ideal_Point = mean(Ideal_Point, na.rm = TRUE),
      Lower_95_CI = quantile(Ideal_Point, 0.025, na.rm = TRUE),
      Upper_95_CI = quantile(Ideal_Point, 0.975, na.rm = TRUE),
      .groups = "drop"
    )
  
  
  # dwnominate ---------------------------------------------------------------
  
  df <- sim_data@score_matrix %>%
    mutate(outcome_disc=as.numeric(outcome_disc),
           vote_code = outcome_disc - 1)
  
  # Split data by sessions
  sessions <- split(df, df$time_id)
  
  rc_list <- lapply(sessions, function(session_data) {
    # Pivot data to wide format: legislators (rows) x votes (columns)
    rc_matrix <- session_data %>%
      select(person_id, item_id, vote_code) %>%
      pivot_wider(names_from = item_id, values_from = vote_code, names_prefix = "vote_") %>%
      column_to_rownames("person_id") %>%
      as.matrix()
    
    # make 1 missing at random
    # Randomly select one index to set as NA
    random_index <- sample(length(rc_matrix), 1)  # Pick one random position
    rc_matrix[random_index] <- NA  # Set the selected position to NA
    
    row.names(rc_matrix) <- paste0("legis_",1:nrow(rc_matrix))
    
    # Create rollcall object
    c1 <- rollcall(
      data = rc_matrix,
      yea = 1,
      nay = 0,
      missing = NA,
      legis.names = row.names(rc_matrix),
      vote.names = colnames(rc_matrix),
      legis.data=data.frame(legis_id=row.names(rc_matrix),
                            party=as.character(as.numeric(runif(nrow(rc_matrix))>0.5))
      ))
    
    names(attr(c1$votes,"dimnames")) <- c("legis_id","vote.names")
    
    c1
    
  })
  
  start_time <- Sys.time()
  
  dw_nom_fit <- dwnominate::dwnominate(rc_list, 
                                       dims=1,minvotes=10,
                                       id="legis_id",polarity=sort(apply(sim_data@simul_data$true_person,1,mean),
                                                                   decreasing=F, 
                                                                   index=T)$ix[1],
                                       model=3)
  
  # Extract estimated ideal points from the original model
  original_theta <- dw_nom_fit$legislators
  
  # Define number of bootstrap replications to calculate dw-nominate uncertainty
  num_bootstraps <- 200
  
  # Step 2: Run the bootstrap procedure
  
  over_boot <- mclapply(1:num_bootstraps, function(b) {
    
    # Step 2a: Simulate a new roll-call matrix
    boot_rc_list <- lapply(rc_list, function(rc) {
      
      # Compute vote probabilities using original parameters
      vote_probs <- plogis(outer(original_theta[,1], rc$votes, "*"))
      
      # fill in missing values with predictive mean matching
      
      yeas <- dw_nom_fit$rollcalls$midpoint1D - dw_nom_fit$rollcalls$spread1D
      nays <- dw_nom_fit$rollcalls$midpoint1D + dw_nom_fit$rollcalls$spread1D
      
      impute_data <- mice::mice(data=tibble(yeas,nays), method = "pmm", m = 1, maxit = 5)
      
      vote_probs <- wnominate::nomprob(yea = as.matrix(impute_data$imp$yeas),
                                       nay=as.matrix(impute_data$imp$nays),
                                       ideal=as.matrix(dw_nom_fit$legislators$coord1D),
                                       Beta=dw_nom_fit$beta,
                                       dimweight=dw_nom_fit$weights)
      
      # Sample new votes from Bernoulli distribution
      new_votes <- matrix(as.numeric(rbinom(length(rc$votes), size = 1, prob = vote_probs)), 
                          nrow = nrow(rc$votes), ncol = ncol(rc$votes))
      
      random_index <- sample(length(new_votes), 1)  # Pick one random position
      new_votes[random_index] <- NA  # Set the selected position to NA
      
      # Convert back into a rollcall object
      attributes(new_votes) <- attributes(rc$votes)
      rc$votes <- new_votes
      return(rc)
    })
    
    # Step 2b: Refit DW-NOMINATE on the bootstrapped data
    try(boot_fit <- dwnominate(boot_rc_list, dims=1,minvotes=5,lop=0,
                           id="legis_id",polarity=sort(apply(sim_data@simul_data$true_person,1,mean),
                                                       decreasing=F, 
                                                       index=T)$ix[1],
                           start = dw_nom_fit$start,
                           model=3))
    
    if('try-error' %in% class(boot_fit)) {
      
      return(NULL)
      
    } else {
      
      # Step 2c: Store the bootstrapped ideal points
      boot_fit$legislators %>% 
        select(coord1D,ID,session) %>% 
        mutate(bootstrap=b) %>% 
        group_by(session) %>% 
        mutate(coord1D=as.numeric(scale(coord1D))) %>% 
        ungroup
      
    }
    
  },mc.cores=4) %>% bind_rows
  
  # calculate uncertainty intervals
  # if enough bootstraps came back ok
  
  if(nrow(over_boot)>50) {
    
    dw_nom_unc <- group_by(over_boot, ID,session) %>% 
      summarize(ideal_point=mean(coord1D),
                ideal_point_low=quantile(coord1D,.025),
                ideal_point_high=quantile(coord1D,.975))
    
  } else {
    
    dw_nom_unc <- dw_nom_fit$legislators %>% 
      select(coord1D,ID,session) %>% 
      mutate(ideal_point=coord1D,
                ideal_point_low=NA,
                ideal_point_high=NA)
    
  }
  
  
  
  end_time <- Sys.time()
  
  # Compute elapsed time
  dw_elapsed_time <- end_time - start_time
  
  # combine estimates -------------------------------------------------------
  
  mcmcpack_ideal_points <- as_tibble(mcmc_pack_fit) %>%
    mutate(iter_id = row_number()) %>%
    pivot_longer(-iter_id, names_to = "time_id", values_to = "ideal_point") %>%
    filter(grepl(x=time_id,pattern="theta\\.person")) %>% 
    mutate(model = "MCMCpack",
           person_id=str_extract(time_id,"(?<=_)\\d+"),
           time_id=as.numeric(str_extract(time_id,"(?<=t)\\d+")),
           time_elapsed=mcmc_elapsed_time) %>% 
    group_by(time_id) %>% 
    mutate(ideal_point2=as.numeric(scale(ideal_point))) %>% 
    group_by(model,person_id,time_id,time_elapsed) %>% 
    summarize(ideal_point=median(ideal_point2),
              ideal_point_high=quantile(ideal_point2,.975),
              ideal_point_low=quantile(ideal_point2,.025))
  
  dwnominate_ideal_points <- dw_nom_unc %>% 
    mutate(model = "DW-NOMINATE",
           person_id=str_extract(ID, "[0-9]+"),
           time_elapsed=dw_elapsed_time) %>% 
    ungroup %>% 
    select(time_id="session",-ID,model,person_id,
           matches("ideal"),time_elapsed)
  
  
  dynIRT_ideal_points <- ungroup(em_irt_results) %>% 
    mutate(model = "emIRT",
           Legislator=as.character(Legislator)) %>% 
    select(time_id="Time",
           person_id="Legislator",
           ideal_point="Mean_Ideal_Point",
           ideal_point_high="Upper_95_CI",
           ideal_point_low="Lower_95_CI",
           model) %>% 
    mutate(time_elapsed=emirt_elapsed_time)
  
  idealstan_id_pts <- summary(idealstan_fit,
                              aggregated=FALSE) %>%
    group_by(Time_Point) %>% 
    mutate(Ideal_Points=as.numeric(scale(Ideal_Points))) %>% 
    group_by(Person,Time_Point) %>% 
    summarize(`Posterior Median`=quantile(Ideal_Points,.5),
              `High Posterior Interval`=quantile(Ideal_Points,.975),
              `Low Posterior Interval`=quantile(Ideal_Points,.025)) %>% 
    select(person_id="Person",
           ideal_point="Posterior Median",
           ideal_point_high="High Posterior Interval",
           ideal_point_low="Low Posterior Interval",
           time_id="Time_Point") %>% 
    mutate(model="idealstan",
           time_elapsed=idealstan_elapsed_time) %>% 
    ungroup
  
  combined_ideal_points <- bind_rows(
    mcmcpack_ideal_points,
    dwnominate_ideal_points,
    dynIRT_ideal_points,
    idealstan_id_pts
  )
  
  # merge in true values
  
  combined_ideal_points <- left_join(combined_ideal_points,
                                     true_ideal,
                                     by=c("person_id","time_id"))
  
  combined_ideal_points <- combined_ideal_points %>% 
    mutate(in_interval=as.numeric((true_ideal_point > ideal_point_low) & (true_ideal_point < ideal_point_high)),
           rmse = sqrt((true_ideal_point - ideal_point)^2),
           sim=i)
  
  
  # latent regression -------------------------------------------------------
  
  #loop over models, run regression with scores
  
  over_regs <- split(combined_ideal_points, 
                     combined_ideal_points$model) %>%
    lapply(function(this_data) ({
      
      
      c2 <- lm(outcome ~ ideal_point, 
               data=this_data)
      c3 <- summary(c2)
      
      tibble(est_coef=c2$coefficients[2],
             est_coef_pval=c3$coefficients[2,4],
             model=this_data$model[1])
      
    })) %>% bind_rows
  
  combined_ideal_points <- left_join(combined_ideal_points,
                                     over_regs,
                                     by="model")
  
  return(combined_ideal_points)
  
}


# Run multiple tasks in parallel using foreach
results_list <- foreach(task_id = 1:n_sims, .packages = packages, .errorhandling="pass") %dopar% {
  
  print(paste0("Now on task: ",task_id))
  suppressMessages(simulate_task(task_id))
}

saveRDS(results_list,paste0("/lustre/scratch/rkubinec/sim_models_nsims_",n_sims,
                         "_timeproc_",time_process,
                         "_missingness_",as.numeric(missingness),
                         "_timevar_",time_sd,
                         "_numpers_",n_persons,
                         "_numitems_",n_items,
                         ".rds"))

# Combine results from all tasks
#over_sims <- bind_rows(results_list)

# Stop parallel backend
stopCluster(cl)


# Finish and save ---------------------------------------------------------

# calc_sims <- group_by(over_sims, model) %>% 
#   summarize(mean_rmse=mean(rmse),
#             cov=mean(in_interval),
#             rmse_coef=mean(sqrt((est_coef-true_est_coef)^2)),
#             S_err=mean(as.numeric(sign(true_est_coef)!=sign(est_coef))))
