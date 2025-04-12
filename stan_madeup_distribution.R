# Load required packages
library(rstan)
library(ggplot2)

# Function to simulate data from the Bimodal Exponential-Beta Distribution
simulate_bimodal_data <- function(N, lambda, alpha, beta, w) {
  data <- numeric(N)
  for (i in 1:N) {
    if (runif(1) < w) {
      # Exponential component
      data[i] <- -log(1 - runif(1)) / lambda
    } else {
      # Beta component
      data[i] <- rbeta(1, alpha, beta)
    }
    # Bound the data to [0, 1]
    data[i] <- max(1e-6, min(1 - 1e-6, data[i]))  # Avoid exact 0 or 1
  }
  return(data)
}

# Parameters for the simulation
set.seed(123)
N <- 1000
lambda_true <- 5
alpha_true <- 2
beta_true <- 5
w_true <- 0.7

# Simulate data
sim_data <- simulate_bimodal_data(N, lambda_true, alpha_true, beta_true, w_true)

# Visualize the simulated data
ggplot(data.frame(x = sim_data), aes(x)) +
  geom_histogram(bins = 50, color = "black", fill = "lightblue") +
  labs(title = "Simulated Bimodal Exponential-Beta Data", x = "x", y = "Frequency")

# Stan model as a string
stan_model_code <- "
functions {
  real bimodal_exponential_beta_lpdf(real x, real lambda, real alpha, real beta, real w) {
    if (x <= 0 || x >= 1) return negative_infinity();  // Avoid log(0)
    if (w < 0 || w > 1) return negative_infinity();
    real log_exp = log(lambda) - lambda * x;
    real log_beta = lgamma(alpha + beta) - lgamma(alpha) - lgamma(beta) +
                    (alpha - 1) * log(x) + (beta - 1) * log1m(x);
    return log_sum_exp(log(w) + log_exp, log1m(w) + log_beta);
  }
}
data {
  int<lower=1> N;
  real<lower=0, upper=1> x[N];
}
parameters {
  real<lower=0> lambda;
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0, upper=1> w;
}
model {
  // Priors
  lambda ~ exponential(1);
  alpha ~ gamma(2, 2);
  beta ~ gamma(2, 2);
  w ~ beta(2, 2);

  // Likelihood
  for (n in 1:N) {
    target += bimodal_exponential_beta_lpdf(x[n] | lambda, alpha, beta, w);
  }
}
"

# Compile the Stan model
stan_model <- stan_model(model_code = stan_model_code)

# Prepare data for Stan
stan_data <- list(
  N = N,
  x = sim_data
)

# Define initial values function
init_fun <- function() {
  list(
    lambda = abs(rnorm(1, mean = lambda_true, sd = 1)),
    alpha = abs(rnorm(1, mean = alpha_true, sd = 0.5)),
    beta = abs(rnorm(1, mean = beta_true, sd = 0.5)),
    w = runif(1, min = 0.5, max = 0.9)
  )
}

# Fit the Stan model with initial values
fit <- sampling(
  stan_model,
  data = stan_data,
  init = init_fun,
  iter = 2000,
  chains = 4,
  seed = 123,
  control = list(adapt_delta = 0.9)
)

# Print results
print(fit)

# Extract posterior samples
posterior_samples <- extract(fit)

# Plot posterior distributions
posterior_df <- data.frame(
  lambda = posterior_samples$lambda,
  alpha = posterior_samples$alpha,
  beta = posterior_samples$beta,
  w = posterior_samples$w
)

ggplot(posterior_df, aes(x = lambda)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Posterior Distribution of Lambda", x = "Lambda", y = "Density")

ggplot(posterior_df, aes(x = alpha)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  labs(title = "Posterior Distribution of Alpha", x = "Alpha", y = "Density")

ggplot(posterior_df, aes(x = beta)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Posterior Distribution of Beta", x = "Beta", y = "Density")

ggplot(posterior_df, aes(x = w)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Posterior Distribution of Weight (w)", x = "w", y = "Density")
