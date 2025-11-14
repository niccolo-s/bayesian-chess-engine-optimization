# Set the parameters for the Beta distribution
alpha <- 2 # shape parameter
beta <- 8    # shape parameter (not rate, both are shape parameters for Beta)

# Generate random numbers from the Beta distribution
set.seed(27)  # for reproducibility
beta_samples <- rbeta(10000, alpha, beta)

# Plot the histogram of the generated samples
hist(beta_samples, 
     breaks = 50, 
     main = paste("Beta Distribution (alpha=", alpha, ", beta=", beta, ")", sep=""), 
     xlab = "Value", 
     col = "skyblue", 
     border = "white", 
     probability = TRUE) # Normalize the histogram to a density

# Add a vertical line for the mean
abline(v = mean(beta_samples), col = "red", lwd = 1)

# Plot the theoretical Beta distribution curve
curve(dbeta(x, alpha, beta), col = "darkred", lwd = 3, lty = 2, add = TRUE)

# Add a smooth density curve of the sampled data
lines(density(beta_samples), col = "darkgreen", lwd = 3)

# Display the mean and SD
cat("Mean of the generated samples:", mean(beta_samples), "\n")
cat("Standard deviation of the generated samples:", sd(beta_samples), "\n")
