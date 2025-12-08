# Simulation for 4ST414 Semester Project
# Topic: Variance Estimation of Sample Quantiles using Woodruff's Method (Taylor Linearization)
# Author: František Pavlík (AI generated)

library(ggplot2)
library(dplyr)
library(tidyr)

# Set random seed for reproducibility
set.seed(42)

# --- Configuration ---
# Population size (for finite population correction - though often ignored in simple asymptotics, we keep it for strictness if needed)
N_pop <- 100000 
# Simulation parameters
R_replications <- 1000
sigmas <- c(0.5, 1.0, 1.5)  # Shape parameters for Log-Normal
sample_sizes <- c(200, 500, 1000, 2000)
quantiles_p <- c(0.5, 0.9)  # Median and 90th percentile

# Output directory for figures
if (!dir.exists("fig")) dir.create("fig")

# --- Helper Functions ---

# Theoretical True Values for Log-Normal(0, sigma)
# Quantile function: Q(p) = exp(mu + sigma * qnorm(p))
# Density at Q(p): f(Q(p)) = dlnorm(Q(p), mu, sigma)
get_true_quantile <- function(p, sigma) {
  exp(0 + sigma * qnorm(p))
}

get_true_density <- function(q, sigma) {
  dlnorm(q, meanlog = 0, sdlog = sigma)
}

# Asymptotic Variance (Infinite population approximation)
# Var(Q_hat) approx p(1-p) / (n * f(Q)^2)
get_asymptotic_variance <- function(n, p, sigma) {
  Q <- get_true_quantile(p, sigma)
  f <- get_true_density(Q, sigma)
  (p * (1 - p)) / (n * f^2)
}

# Woodruff's Method Implementation
woodruff_variance_est <- function(y, p, alpha = 0.05) {
  n <- length(y)
  
  # 1. Confidence interval for proportion p
  z <- qnorm(1 - alpha/2)
  margin_p <- z * sqrt(p * (1 - p) / n)
  
  p_L <- p - margin_p
  p_U <- p + margin_p
  
  # 2. Map to quantiles (indices)
  # Clamp indices to [1, n] to avoid errors
  idx_L <- max(1, floor(p_L * n))
  idx_U <- min(n, ceiling(p_U * n))
  
  y_sorted <- sort(y)
  
  Q_L <- y_sorted[idx_L]
  Q_U <- y_sorted[idx_U]
  
  # 3. Woodruff Interval
  # Interval is [Q_L, Q_U]. Len = 2 * z * SE
  # SE = Len / (2*z)
  # Var = SE^2
  
  implied_se <- (Q_U - Q_L) / (2 * z)
  return(implied_se^2)
}

# --- Main Simulation Loop ---

results <- data.frame()

cat("Starting Simulation...\n")

for (sigma in sigmas) {
  cat(sprintf("Processing Sigma = %.1f\n", sigma))
  
  # Pre-calculate true values
  true_Q_05 <- get_true_quantile(0.5, sigma)
  true_Q_09 <- get_true_quantile(0.9, sigma)
  
  # Theoretical Variances for n=1 (divide by n later)
  true_var_const_05 <- get_asymptotic_variance(1, 0.5, sigma)
  true_var_const_09 <- get_asymptotic_variance(1, 0.9, sigma)
  
  for (n in sample_sizes) {
    cat(sprintf("  Sample size n = %d\n", n))
    
    # Storage for this run
    ests_Q05 <- numeric(R_replications)
    ests_Q09 <- numeric(R_replications)
    vars_woodruff_05 <- numeric(R_replications)
    vars_woodruff_09 <- numeric(R_replications)
    
    # Coverage counters
    cov_05 <- 0
    cov_09 <- 0
    
    for (r in 1:R_replications) {
      # Generate data
      sample_data <- rlnorm(n, meanlog = 0, sdlog = sigma)
      
      # 1. Point Estimates (sample quantiles)
      # type=7 is standard in R, but for large n differences are negligible
      q_hats <- quantile(sample_data, probs = c(0.5, 0.9))
      ests_Q05[r] <- q_hats[1]
      ests_Q09[r] <- q_hats[2]
      
      # 2. Woodruff Variance Estimates
      v_05 <- woodruff_variance_est(sample_data, 0.5)
      v_09 <- woodruff_variance_est(sample_data, 0.9)
      
      vars_woodruff_05[r] <- v_05
      vars_woodruff_09[r] <- v_09
      
      # 3. Check Coverage (using the estimated variance to build normal CI)
      # CI = Q_hat +/- z * sqrt(V_woodruff)
      # NOTE: Woodruff method actually gives the CI directly [Q_L, Q_U].
      # But usually we check if the LINEARIZED variance works in the standard Wald formula,
      # OR we check if the Woodruff interval itself covers true Q.
      # Zadani implies: "Coverage Probability: How many constructed 95% CIs cover Q_p"
      # The Woodruff interval IS the CI. Let's recalculate it or just use the variance approx for Wald CI.
      # Let's use Wald CI with Woodruff SE for consistency with "Variance Estimation" topic.
      
      SE_05 <- sqrt(v_05)
      CI_05 <- c(ests_Q05[r] - 1.96*SE_05, ests_Q05[r] + 1.96*SE_05)
      if (true_Q_05 >= CI_05[1] && true_Q_05 <= CI_05[2]) cov_05 <- cov_05 + 1
      
      SE_09 <- sqrt(v_09)
      CI_09 <- c(ests_Q09[r] - 1.96*SE_09, ests_Q09[r] + 1.96*SE_09)
      if (true_Q_09 >= CI_09[1] && true_Q_09 <= CI_09[2]) cov_09 <- cov_09 + 1
    }
    
    # --- Aggregate Results ---
    
    # True empirical variance of the estimator (Monte Carlo Variance)
    MC_Var_05 <- var(ests_Q05)
    MC_Var_09 <- var(ests_Q09)
    
    # Average Estimated Variance (Woodruff)
    Mean_Woodruff_Var_05 <- mean(vars_woodruff_05, na.rm=TRUE)
    Mean_Woodruff_Var_09 <- mean(vars_woodruff_09, na.rm=TRUE)
    
    # Relative Bias of Variance Estimator
    RB_05 <- (Mean_Woodruff_Var_05 - MC_Var_05) / MC_Var_05
    RB_09 <- (Mean_Woodruff_Var_09 - MC_Var_09) / MC_Var_09
    
    # Coverage Probability
    CP_05 <- cov_05 / R_replications
    CP_09 <- cov_09 / R_replications
    
    # Store
    results <- rbind(results, data.frame(
      Sigma = sigma,
      n = n,
      Quantile = "Median (0.5)",
      True_Q = true_Q_05,
      MC_Var = MC_Var_05,
      Est_Var = Mean_Woodruff_Var_05,
      Rel_Bias_Pct = RB_05 * 100,
      Coverage = CP_05
    ))
    
    results <- rbind(results, data.frame(
      Sigma = sigma,
      n = n,
      Quantile = "P90 (0.9)",
      True_Q = true_Q_09,
      MC_Var = MC_Var_09,
      Est_Var = Mean_Woodruff_Var_09,
      Rel_Bias_Pct = RB_09 * 100,
      Coverage = CP_09
    ))
  }
}

# --- Saving Results ---

write.csv(results, "results_simulation.csv", row.names = FALSE)
print(results)

# --- Visualization ---

# 1. Coverage Plot
p1 <- ggplot(results, aes(x = factor(n), y = Coverage, fill = factor(Sigma))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") +
  facet_wrap(~Quantile) +
  labs(title = "Coverage Probability of Woodruff's 95% CI",
       subtitle = "Comparison across Sample Sizes and Skewness (Sigma)",
       x = "Sample Size (n)",
       y = "Coverage Probability",
       fill = "Sigma (Skewness)") +
  theme_minimal() +
  ylim(0, 1)

ggsave("fig/plot_coverage.png", p1, width = 8, height = 6)

# 2. Relative Bias Plot
p2 <- ggplot(results, aes(x = n, y = Rel_Bias_Pct, color = factor(Sigma))) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~Quantile) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Relative Bias of Woodruff's Variance Estimator",
       x = "Sample Size (n)",
       y = "Relative Bias (%)",
       color = "Sigma") +
  theme_minimal()

ggsave("fig/plot_bias.png", p2, width = 8, height = 6)

cat("Simulation completed successfully.\n")
