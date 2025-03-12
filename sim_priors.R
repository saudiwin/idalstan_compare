# simulate priors for other IRT models

# Load required libraries

library(ggplot2)
library(dplyr)
library(tidyr)

# Set simulation parameters
set.seed(123)
J <- 500   # Number of subjects (legislators)
T <- 10   # Number of time periods
I <- 500   # Number of items (bills)

# Hyperparameters for priors
e0 <- 0      # Mean of initial ideal points
E0 <- 1      # Variance of initial ideal points
c0 <- -1      # Shape parameter for inverse gamma
d0 <- -1      # Scale parameter for inverse gamma
a0 <- 0      # Mean for alpha (discrimination)
A0 <- 1      # Precision for alpha (A0^{-1} is variance)
b0 <- 0      # Mean for beta (difficulty)
B0 <- 1      # Precision for beta (B0^{-1} is variance)

# Function to generate inverse-gamma samples
rinvgamma <- function(n, shape, scale) {
  1 / rgamma(n, shape = shape, rate = 1 / scale)
}

# Simulate evolution variance (tau^2_j) for each subject (Inverse-Gamma distribution)
tau2_j <- 1

# Simulate ideal points (theta_jt) over time
theta <- matrix(NA, nrow = J, ncol = T)

# Initialize theta at t=0
theta[, 1] <- rnorm(J, mean = e0, sd = sqrt(E0))

# Evolve theta over time using the transition model
for (t in 2:T) {
  theta[, t] <- rnorm(J, mean = theta[, t - 1], sd = sqrt(tau2_j))
}

# Simulate item parameters (alpha_i and beta_i)
alpha <- rnorm(I, mean = a0, sd = sqrt(1 / A0))  # Discrimination
beta <- rnorm(I, mean = b0, sd = sqrt(1 / B0))   # Difficulty

# Convert parameters to a long-format data frame
df_tau <- data.frame(Parameter = "tau2_j", Value = tau2_j)
df_theta <- data.frame(Parameter = "theta_j0", Value = theta[, 1])  # Only initial ideal points
df_alpha <- data.frame(Parameter = "alpha", Value = alpha)
df_beta <- data.frame(Parameter = "beta", Value = beta)

# Combine into one data frame
df_long <- bind_rows(df_tau, df_theta, df_alpha, df_beta)

# Plot faceted histograms
ggplot(df_long, aes(x = Value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Parameter, scales = "free") +
  labs(title = "Faceted Histograms of Simulated Priors",
       x = "Value", y = "Count") +
  theme_minimal()
