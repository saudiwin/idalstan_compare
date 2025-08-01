# simulate time series idealstan model and run other models against it
# give them true parameters to constrain

# parallelization libraries
library(future)
library(future.apply)
library(parallel)
library(idealstan)
library(MCMCpack)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)
library(emIRT)
library(dwnominate)


# simulation parameters ---------------------------------------------------
this_seed <- 20250310
set.seed(this_seed)  # For reproducibility

n_sims <- as.numeric(Sys.getenv("NSIMS"))
time_points <- as.numeric(Sys.getenv("TIMEPOINTS"))
n_persons <- as.numeric(Sys.getenv("NPERSON"))
n_items <- as.numeric(Sys.getenv("NITEM"))
time_sd <- as.numeric(Sys.getenv("TIMESD"))
true_coef <- as.numeric(Sys.getenv("TRUECOEF")) # size of coefficient in latent regression
time_process <- Sys.getenv("TIMEPROC") # type of time process being simulated
missingness <- as.logical(as.numeric((Sys.getenv("MISSING")))) # whether to model missing data

sim_iter <- Sys.getenv("ITER")

# n_sims <- 5
# time_points <- 10
# n_persons <- 50
# n_items <- 200
# time_sd <- .4
# true_coef <- .1 # size of coefficient in latent regression
# time_process <- "random_walk" # type of time process being simulated
# missingness <- FALSE

print(paste0("NSIMS is: ", n_sims))
print(paste0("TIMEPROC is: ", time_process))
print(paste0("MISSING is: ", missingness))

# sim_iter <- 1

# Define parallelization parameters
cores_per_task <- 4  # Number of cores per task

# Detect available cores
total_cores <- detectCores() - 1
num_workers <- min(n_sims, total_cores / cores_per_task)  # Ensure we don't
#overload CPU
#num_workers <- parallel::detectCores()

print(paste0("Total cores is: ",detectCores()))

# Register parallel backend for tasks
# cl <- makeCluster(num_workers,outfile="")
# registerDoParallel(cl)

# Define simulation function (runs in parallel)
simulate_task <- function(task_id) {
  
  print(paste0("Starting task ", task_id))
  
  task_time <- Sys.time()
  
  # simulate dataset with idealstan -----------------------------------------
  
  spline_degree <- case_when(time_process=="splines" & time_sd==0.4 ~ 2,
                       time_process=="splines" & time_sd==1 ~ 3,
                       TRUE ~ 2)
  
  time_sd <- case_when(time_process=="splines"~0.4,
                       TRUE~time_sd)
  
  num_ids <- 1
  
  sim_data <- id_sim_gen(num_person=n_persons,
                         num_items=n_items,
                         time_sd=time_sd,
                         ideal_pts_sd=1,inflate = missingness,
                         time_process=time_process,
                         spline_basis_sd = 1,
                         #absence_discrim_sd = .5,
                         gp_rho=.5,
                         gp_alpha=case_when(time_process=="GP" & time_sd==1 ~ .2,
                                            TRUE ~ .5),
                         time_points=time_points,
                         spline_degree = spline_degree)
  
  if(missingness) {
    
    wide_df <- sim_data@score_matrix %>%
      mutate(outcome_disc=na_if(outcome_disc, "Missing"),
        outcome_disc=as.numeric(outcome_disc)-1) %>% 
      dplyr::select(person_id, item_id, outcome_disc) %>%
      pivot_wider(names_from = item_id, values_from = outcome_disc, names_prefix = "item_")
    
    
  } else {
    
    wide_df <- sim_data@score_matrix %>%
      mutate(outcome_disc=as.numeric(outcome_disc)-1) %>% 
      dplyr::select(person_id, item_id, outcome_disc) %>%
      pivot_wider(names_from = item_id, values_from = outcome_disc, names_prefix = "item_")
    
  }
  
  
  
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
  
  true_ideal <- true_ideal %>% 
    ungroup %>% 
    mutate(true_ideal_point_raw=true_ideal_point,
           true_ideal_point=scale(true_ideal_point)) %>% 
    ungroup
  
  
  # generate covariate model ------------------------------------------------
  
  covar <- rnorm(n=nrow(true_ideal),
                mean = 1 + 0.5 * true_ideal$true_ideal_point)
    
  outcome <- rnorm(n=nrow(true_ideal),
                   mean=-2 + -1*true_ideal$true_ideal_point + true_coef * true_ideal$true_ideal_point * covar + 3*covar)
  
  true_ideal$true_coef <- true_coef
  true_ideal$outcome <- outcome
  true_ideal$covar <- covar
  
  c1 <- lm(outcome ~ true_ideal_point*covar, data=true_ideal)
  
  true_ideal$true_est_coef <- c1$coefficients[4]
  
  
  # mcmc pack ---------------------------------------------------------------
  
  
  constraint_ids <- c(sort(apply(sim_data@simul_data$true_person,1,mean),
                           decreasing=TRUE,
                           index=TRUE)$ix[1:num_ids],
                      sort(apply(sim_data@simul_data$true_person,1,mean),
                           decreasing=F, 
                           index=T)$ix[1:num_ids])
  
  constraint_vals <- c(sort(apply(sim_data@simul_data$true_person,1,mean),
                            decreasing=T)[1:num_ids],
                       sort(apply(sim_data@simul_data$true_person,1,mean),
                            decreasing=F)[1:num_ids])
  
  # use same technique for starting values
  
  theta.start <- rep(0, n_persons)
  theta.start[constraint_ids[1:num_ids]] <- constraint_vals[1:num_ids]
  theta.start[constraint_ids[(num_ids+1):(2*num_ids)]] <- constraint_vals[(num_ids+1):(2*num_ids)]
  
  const_list <- lapply(constraint_vals, function(x) {x})
  names(const_list) <- paste0("person_",constraint_ids)
  
  start_time <- Sys.time()
  
  # if missing data, need to set starting values for items
  
  if(missingness) {
    
    beta.start <- 0
    
  } else {
    
    beta.start <- NA
    
  }
  
  # make constraints for MCMC pack
  
  constraint_list <- as.list(constraint_vals)
  names(constraint_list) <- constraint_ids
  
  mcmc_pack_fit <- MCMCdynamicIRT1d(wide_df_mat,
                                    item_time_map,
                                    burnin=20000,
                                    mcmc=40000,
                                    theta.constraints = constraint_list,
                                    theta.start = theta.start,
                                    tau2.start = time_sd,
                                    beta.start=beta.start,
                                    thin = 20,seed = this_seed,
                                    A0 = 1,B0 = 1)
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  mcmc_elapsed_time <- end_time - start_time
  
  
  # idealstan ---------------------------------------------------------------
  print("Running idealstan")
  start_time <- Sys.time()
  
  # identify items to constrain
  
  item_time_ids <- distinct(sim_data@score_matrix, item_id, time_id)
  
  true_item <- tibble(discrim=sim_data@simul_data$true_reg_discrim) %>% 
    mutate(item_id=factor(1:n()))
  
  item_time_ids <- left_join(item_time_ids, true_item)
  
  # need to cut
  
  item_time_ids <- mutate(item_time_ids,
                          time_id=as.numeric(time_id),
                          time_period=as.numeric(cut(time_id, 2, 
                                          labels=c(1,2)))) %>% 
    group_by(time_period) %>% 
    filter(discrim %in% c(max(discrim),min(discrim)))
  
  # need to convert to beta regression parameters
  
  get_gbeta_shapes <- function(y, a = -1, b = +1, phi = 100,
                               return="alpha") {
    x     <- (y - a) / (b - a)
    alpha <- phi * x
    beta  <- phi * (1 - x)
    if(return=="alpha") return(alpha)
    if(return=="beta") return(beta)
  }
  
  item_time_ids <- mutate(item_time_ids,
                          shape=get_gbeta_shapes(discrim,phi=750),
                          scale=get_gbeta_shapes(discrim, return="beta",phi=750))
  
  
  high_ids <- arrange(ungroup(item_time_ids), desc(discrim)) %>% slice(c(1:num_ids))
  low_ids <- arrange(ungroup(item_time_ids), discrim) %>% slice(c(1:num_ids))
  
  if(missingness) {
    
    model_type <- 2
    
  } else {
    
    model_type <- 1
    
  }
  
  time_process_ideal <- case_match(time_process,
                             "random"~"random_walk",
                             "AR" ~ "AR1",
                             .default=time_process)
  
  idealstan_fit <- sim_data %>% 
    id_estimate(model_type=model_type,time_var=1/time_sd,
                vary_ideal_pts=time_process_ideal,
                nchains=1,ar1_down = -1,
                niter=1000,warmup=500,ncores=cores_per_task,person_sd = 1,
                spline_degree =  spline_degree,
                gp_alpha=case_when(time_process=="GP" & time_sd==1 ~ .2,
                                   TRUE ~ .5),
                restrict_ind_high = high_ids$item_id,
                restrict_ind_low = low_ids$item_id,
                restrict_N_high = high_ids$shape,
                restrict_sd_low = low_ids$shape,
                restrict_sd_high = high_ids$scale,
                restrict_N_low = low_ids$scale,
                fixtype='prefix',const_type="items",
                seed=this_seed)
  
  
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  idealstan_elapsed_time <- end_time - start_time
  
  print(paste0("Done with idealstan: finished in ",idealstan_elapsed_time))
  
  # idealstan pathfinder ---------------------------------------------------------------
  print("Running idealstan -- pathfinder")
  start_time <- Sys.time()
  
  idealstan_pathfinder_fit <- sim_data %>% 
    id_estimate(model_type=model_type,time_var=1/time_sd,
                vary_ideal_pts=time_process_ideal,
                nchains=1,use_method = "pathfinder",
                niter=1000,warmup=500,ncores=cores_per_task,person_sd = 1,
                spline_degree =  spline_degree,ar1_down = -1,
                gp_alpha=case_when(time_process=="GP" & time_sd==1 ~ .2,
                                   TRUE ~ .5),
                restrict_ind_high = high_ids$item_id,
                restrict_ind_low = low_ids$item_id,
                restrict_N_high = high_ids$shape,
                restrict_sd_low = low_ids$shape,
                restrict_sd_high = high_ids$scale,
                restrict_N_low = low_ids$scale,
                fixtype='prefix',const_type="items",
                seed=this_seed)
  
  
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  idealstan_pathfinder_elapsed_time <- end_time - start_time
  
  # idealstan laplace ---------------------------------------------------------------
  print("Running idealstan -- laplace")
  start_time <- Sys.time()
  
  idealstan_laplace_fit <- sim_data %>% 
    id_estimate(model_type=model_type,time_var=1/time_sd,
                vary_ideal_pts=time_process_ideal,
                nchains=1,person_sd=1,
                use_method="laplace",
                niter=1000,ar1_down = -1,
                warmup=500,ncores=cores_per_task,
                spline_degree = spline_degree,
                gp_alpha=case_when(time_process=="GP" & time_sd==1 ~ .2,
                                   TRUE ~ .5),
                restrict_ind_high = high_ids$item_id,
                restrict_ind_low = low_ids$item_id,
                restrict_N_high = high_ids$shape,
                restrict_sd_low = low_ids$shape,
                restrict_sd_high = high_ids$scale,
                restrict_N_low = low_ids$scale,
                fixtype='prefix',const_type="items",
                seed=this_seed)
  
  
  # End time
  end_time <- Sys.time()
  
  # Compute elapsed time
  idealstan_laplace_elapsed_time <- end_time - start_time
  
  
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
  # set informative starting values as well
  x[constraint_ids,] <- constraint_vals
  
  # Define priors
  x_mu0 <- as.matrix(rep(0,n_persons),ncol=1)
  x_mu0[constraint_ids,] <- constraint_vals # Prior means for respondent ideal points
  x_sigma0 <- matrix(1, nrow = N, ncol = 1)  # Prior variance for respondent ideal points
  x_sigma0[constraint_ids, ] <- 0.1
  beta_mu <- matrix(0, nrow = 2, ncol = 1)   # Prior means for item parameters
  beta_sigma <- diag(x=c(3,1),2)  # Prior covariance matrix for item parameters
  omega2 <- matrix(time_sd^2, nrow = N, ncol = 1)  # Evolutionary variance
  
  # priors
  
  start_time <- Sys.time()
  
  sink("/dev/null")
  em_irt_fit <- suppressMessages(dynIRT(.data=list(rc=rc_matrix,
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
                       )))
  sink()
  
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
        prob_yes <- pnorm(beta_hat[j] * x_hat[, bill_session[j] + 1] + alpha_hat[j])  # Logistic probability
        simulated_votes[, j] <- binom_func(prob_yes)
      }
    }
    
    sink("/dev/null")
    
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
    
    sink()
    
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
    group_by(Bootstrap) %>% 
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
           vote_code = case_match(outcome_disc,
                                  1~0,
                                  2~1,
                                  3~NA_real_))
  
  # Split data by sessions
  sessions <- split(df, df$time_id)
  
  rc_list <- lapply(sessions, function(session_data) {
    # Pivot data to wide format: legislators (rows) x votes (columns)
    rc_matrix <- session_data %>%
      dplyr::select(person_id, item_id, vote_code) %>%
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
  
  sink("/dev/null") 
  dw_nom_fit <- try(suppressMessages(dwnominate::dwnominate(rc_list, 
                                       dims=1,minvotes=10,
                                       id="legis_id",
                                       polarity=sort(apply(sim_data@simul_data$true_person,1,mean),
                                                                   decreasing=T, 
                                                                   index=T)$ix[1],
                                       model=spline_degree)))
  sink()
  
  if(!('try-error' %in% class(dw_nom_fit))) {
    
    # Extract estimated ideal points from the original model
    original_theta <- dw_nom_fit$legislators
    
    # Define number of bootstrap replications to calculate dw-nominate uncertainty
    num_bootstraps <- 200
    
    # Step 2: Run the bootstrap procedure
    
    over_boot <- mclapply(1:num_bootstraps, function(b) {
      
      # Step 2a: Simulate a new roll-call matrix
      boot_rc_list <- lapply(rc_list, function(rc) {
        
        # Compute vote probabilities using original parameters
        #vote_probs <- plogis(outer(original_theta[,1], rc$votes, "*"))
        
        # fill in missing values with predictive mean matching
        
        yeas <- dw_nom_fit$rollcalls$midpoint1D - dw_nom_fit$rollcalls$spread1D
        nays <- dw_nom_fit$rollcalls$midpoint1D + dw_nom_fit$rollcalls$spread1D
        sink("/dev/null")
        
        # only impute if necessary
        
        if(any(is.na(yeas) | is.na(nays))) {
          
          impute_data <- mice::mice(data=tibble(yeas,nays), method = "pmm", m = 1, maxit = 5)
          
        } else {
          
          impute_data <- list(imp=list(yeas=yeas,
                                       nays=nays))
          
        }
        
        
        vote_probs <- wnominate::nomprob(yea = as.matrix(impute_data$imp$yeas),
                                         nay=as.matrix(impute_data$imp$nays),
                                         ideal=as.matrix(dw_nom_fit$legislators$coord1D),
                                         Beta=dw_nom_fit$beta,
                                         dimweight=dw_nom_fit$weights)
        
        sink()
        
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
      
      sink("/dev/null") 
      
      # Step 2b: Refit DW-NOMINATE on the bootstrapped data
      try(boot_fit <- suppressMessages(dwnominate(boot_rc_list, dims=1,minvotes=10,lop=0,
                                                  id="legis_id",polarity=sort(apply(sim_data@simul_data$true_person,1,mean),
                                                                              decreasing=F, 
                                                                              index=T)$ix[1],
                                                  start = dw_nom_fit$start,
                                                  model=spline_degree)))
      
      sink()
      
      if('try-error' %in% class(boot_fit)) {
        
        return(NULL)
        
      } else {
        
        # Step 2c: Store the bootstrapped ideal points
        boot_fit$legislators %>% 
          dplyr::select(coord1D,ID,session) %>% 
          mutate(bootstrap=b) %>% 
          #group_by(session) %>% 
          mutate(coord1D=as.numeric(scale(coord1D))) %>% 
          ungroup
        
      }
      
    },mc.cores=cores_per_task) %>% bind_rows
    
    # calculate uncertainty intervals
    # if enough bootstraps came back ok
    
    if(nrow(over_boot)>50) {
      
      dw_nom_unc <- group_by(over_boot, ID,session) %>% 
        summarize(ideal_point=mean(coord1D),
                  ideal_point_low=quantile(coord1D,.025),
                  ideal_point_high=quantile(coord1D,.975))
      
    } else {
      
      dw_nom_unc <- dw_nom_fit$legislators %>% 
        dplyr::select(coord1D,ID,session) %>% 
        mutate(ideal_point=coord1D,
               ideal_point_low=NA,
               ideal_point_high=NA)
      
    }
    
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
    group_by(iter_id) %>% 
    mutate(ideal_point2=as.numeric(scale(ideal_point))) %>% 
    group_by(model,person_id,time_id,time_elapsed) %>% 
    summarize(ideal_point=median(ideal_point2),
              ideal_point_high=quantile(ideal_point2,.975),
              ideal_point_low=quantile(ideal_point2,.025))
  
  if('try-error' %in% class(dw_nom_fit)) {
    
    # failed to estimate at all
    
    dwnominate_ideal_points <- sim_data@score_matrix %>% 
      dplyr::select(person_id, time_id) %>% 
      distinct %>%
      mutate(ideal_point=NA,
             ideal_point_low=NA,
             ideal_point_high=NA,
             model = "DW-NOMINATE",
             time_elapsed=NA)
    
  } else {
    
    # estimated at least point estimates
    
    dwnominate_ideal_points <- dw_nom_unc %>% 
      mutate(model = "DW-NOMINATE",
             person_id=str_extract(ID, "[0-9]+"),
             time_elapsed=dw_elapsed_time) %>% 
      ungroup %>% 
      dplyr::select(time_id="session",-ID,model,person_id,
                    matches("ideal"),time_elapsed)
    
  }
  
  
  
  
  dynIRT_ideal_points <- ungroup(em_irt_results) %>% 
    mutate(model = "emIRT",
           Legislator=as.character(Legislator)) %>% 
    dplyr::select(time_id="Time",
           person_id="Legislator",
           ideal_point="Mean_Ideal_Point",
           ideal_point_high="Upper_95_CI",
           ideal_point_low="Lower_95_CI",
           model) %>% 
    mutate(time_elapsed=emirt_elapsed_time)
  
  idealstan_id_pts <- summary(idealstan_fit,
                              aggregated=FALSE) %>%
    group_by(Iteration) %>% 
    mutate(Ideal_Points=as.numeric(scale(Ideal_Points))) %>% 
    group_by(Person,Time_Point) %>% 
    summarize(`Posterior Median`=quantile(Ideal_Points,.5),
              `High Posterior Interval`=quantile(Ideal_Points,.975),
              `Low Posterior Interval`=quantile(Ideal_Points,.025)) %>% 
    dplyr::select(person_id="Person",
           ideal_point="Posterior Median",
           ideal_point_high="High Posterior Interval",
           ideal_point_low="Low Posterior Interval",
           time_id="Time_Point") %>% 
    mutate(model="idealstan",
           time_elapsed=idealstan_elapsed_time) %>% 
    ungroup
  
  idealstan_pathfinder_id_pts <- summary(idealstan_pathfinder_fit,
                              aggregated=FALSE) %>%
    group_by(Iteration) %>% 
    mutate(Ideal_Points=as.numeric(scale(Ideal_Points))) %>% 
    group_by(Person,Time_Point) %>% 
    summarize(`Posterior Median`=quantile(Ideal_Points,.5),
              `High Posterior Interval`=quantile(Ideal_Points,.975),
              `Low Posterior Interval`=quantile(Ideal_Points,.025)) %>% 
    dplyr::select(person_id="Person",
                  ideal_point="Posterior Median",
                  ideal_point_high="High Posterior Interval",
                  ideal_point_low="Low Posterior Interval",
                  time_id="Time_Point") %>% 
    mutate(model="pathfinder",
           time_elapsed=idealstan_pathfinder_elapsed_time) %>% 
    ungroup
  
  idealstan_laplace_id_pts <- summary(idealstan_laplace_fit,
                              aggregated=FALSE) %>%
    group_by(Iteration) %>% 
    mutate(Ideal_Points=as.numeric(scale(Ideal_Points))) %>% 
    group_by(Person,Time_Point) %>% 
    summarize(`Posterior Median`=quantile(Ideal_Points,.5),
              `High Posterior Interval`=quantile(Ideal_Points,.975),
              `Low Posterior Interval`=quantile(Ideal_Points,.025)) %>% 
    dplyr::select(person_id="Person",
                  ideal_point="Posterior Median",
                  ideal_point_high="High Posterior Interval",
                  ideal_point_low="Low Posterior Interval",
                  time_id="Time_Point") %>% 
    mutate(model="laplace",
           time_elapsed=idealstan_laplace_elapsed_time) %>% 
    ungroup
  
  combined_ideal_points <- bind_rows(
    mcmcpack_ideal_points,
    dwnominate_ideal_points,
    dynIRT_ideal_points,
    idealstan_id_pts,
    idealstan_pathfinder_id_pts,
    idealstan_laplace_id_pts
  )
  
  # merge in true values
  
  combined_ideal_points <- left_join(combined_ideal_points,
                                     true_ideal,
                                     by=c("person_id","time_id"))
  
  combined_ideal_points <- combined_ideal_points %>% 
    mutate(in_interval=as.numeric((true_ideal_point > ideal_point_low) & (true_ideal_point < ideal_point_high)),
           rmse = sqrt((true_ideal_point - ideal_point)^2),rmse2=sqrt((true_ideal_point_raw - ideal_point)^2),
           sim=task_id)
  
  
  # latent regression -------------------------------------------------------
  
  #loop over models, run regression with scores
  # use sample of full data
  
  over_regs <- split(combined_ideal_points, 
                     combined_ideal_points$model) %>%
    lapply(function(this_data) ({
      
      print(paste0("Now on model: ",unique(this_data$model)))
      
      if(!all(is.na(this_data$ideal_point))) {
        
        # select 200 data points at random
        
        c2 <- lm(outcome ~ ideal_point*covar, 
                 data=this_data)
        c3 <- summary(c2)
        
        tibble(est_coef=c2$coefficients[4],
               est_coef_pval=c3$coefficients[4,4],
               model=this_data$model[1])
        
      } else {
        
        tibble(est_coef=NA_real_,
               est_coef_pval=NA_real_,
               model=this_data$model[1])
        
      }
      
      
      
    })) %>% bind_rows
  
  combined_ideal_points <- left_join(combined_ideal_points,
                                     over_regs,
                                     by="model")
  
  task_time <- Sys.time() - task_time
  
  combined_ideal_points$task_time <- task_time
  
  return(combined_ideal_points)
  
}

# Run multiple tasks in parallel using foreach
results_list <- mclapply(1:n_sims, simulate_task, mc.cores=num_workers)

#results_list <- bind_rows(results_list)

saveRDS(results_list,paste0("/lustre/scratch/rkubinec/sim_models_iter",sim_iter,
                         ".rds"))

# Combine results from all tasks
#

# Stop parallel backend
#stopCluster(cl)



