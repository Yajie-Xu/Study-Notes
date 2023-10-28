library(ggplot2)
library(patchwork)
library(VGAM)

set.seed(123) # For reproducibility
n <- 1000 # Number of samples

create_plots <- function(samples, distribution_name, dist_color) {  data <- data.frame(values = samples)

# Density plot against standard normal
p1 <- ggplot(data, aes(values)) + 
  geom_density(aes(y=..density.., fill=dist_color), alpha=0.5) +
  stat_function(fun=dnorm, args=list(mean=0, sd=1), aes(color="Standard Normal"), size=1) +
  labs(title=paste(distribution_name, "vs Standard Normal"), x="Value", y="Density") +
  theme_minimal() +
  scale_fill_identity(name="Distributions") +
  theme(legend.position="top")

# QQ plot
p2 <- ggplot(data, aes(sample = values)) +
  geom_qq() +
  geom_qq_line(aes(sample = values), color="red", size=1) +
  labs(title=paste("QQ-Plot for", distribution_name), x="Theoretical Quantiles", y="Sample Quantiles") +
  theme_minimal()

return(p1 + p2)
}

# Plot for Laplace Distribution
laplace_samples = rlaplace(n*10, location=0, scale=2)
plot_laplace <- create_plots(laplace_samples, "Laplace(0,2)", "blue")
print(plot_laplace)

# Generate random samples from the Cauchy distribution
cauchy_sample <- rcauchy(n*10,location = 0, scale = 1)
# Filter extreme values (e.g., those beyond a certain range)
threshold <- 15  # adjust as needed
filtered_cauchy_sample <- cauchy_sample[abs(cauchy_sample) < threshold]

# Use your function to create the plots
plot_cauchy <- create_plots(filtered_cauchy_sample, "Truncated Cauchy(0,1)", "blue")
print(plot_cauchy)

# For the Beta Distribution
beta_samples <- rbeta(n*100, shape1=2, shape2=5)
plot_beta <- create_plots(beta_samples, "Beta(2,5)", "blue")
print(plot_beta)

# Plot for Exponential Distribution
exp_samples = rexp(n, rate=1)
plot_exp <- create_plots(exp_samples, "Exponential(1)", "cyan")
print(plot_exp)

# For the Gamma Distribution
gamma_samples <- rgamma(n, shape=2, scale=1)
plot_gamma <- create_plots(gamma_samples, "Gamma(2,1)", "cyan")
print(plot_gamma)

# For the Log-Normal Distribution
lognorm_samples <- rlnorm(n, meanlog=0, sdlog=1)
plot_lognorm <- create_plots(lognorm_samples, "Log-Normal(0,0)", "cyan")
print(plot_lognorm)

# Plot for Uniform Distribution
uni_samples = runif(n*20, min=-2, max=2)
plot_uni <- create_plots(uni_samples, "Uniform(-2,2)", "yellow")
print(plot_uni)

# For the Binomial Distribution
binom_samples <- rbinom(n*10, size=5, prob=0.5)
plot_binom <- create_plots(binom_samples, "Binomial(0.5)", "yellow")
print(plot_binom)

# Distributions
standard_normal_samples <- rnorm(n*10)
bimodal_samples <- c(rnorm(n/2, mean=-3), rnorm(n/2, mean=3))
bimodal_samples_2 <- c(rnorm(n/2, mean=-2, .25), rnorm(n/2, mean=2, .25))
trimodal_samples <- c(rnorm(n/3, mean=-6), rnorm(n/3, mean=0), rnorm(n/3, mean=6))
quadmodal_samples <- c(rnorm(n/4, mean=-6), rnorm(n/4, mean=-2), rnorm(n/4, mean=2), rnorm(n/4, mean=6))

# Generate plots
plot_standard_normal <- create_plots(standard_normal_samples, "Standard Normal","green")
plot_bimodal <- create_plots(bimodal_samples, "Bimodal","green")
plot_bimodal_2 <- create_plots(bimodal_samples_2, "Bimodal","green")
plot_trimodal <- create_plots(trimodal_samples, "Trimodal","green")
plot_quadmodal <- create_plots(quadmodal_samples, "Quadmodal","green")

# Display plots
print(plot_standard_normal)
print(plot_bimodal)
print(plot_bimodal_2)
print(plot_trimodal)
print(plot_quadmodal)