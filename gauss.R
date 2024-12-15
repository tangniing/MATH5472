library(smashr)
num_points <- 2^10
x_vector <- seq(0, 1, length.out = num_points)

# spike function
signal_function <- function(x) {
  0.6 * exp(-800 * (x - 0.08)^2) +
    2.3 * exp(-1000 * (x - 0.25)^2) +
    1.8 * exp(-1800 * (x - 0.45)^2) +
    1.0 * exp(-2200 * (x - 0.65)^2)
}

true_signal <- signal_function(x_vector)
noise <- rnorm(n, mean = 0, sd = 0.2)
scaled_signal <- true_signal + noise

# Denoise the Gaussian noisy data using the 'smash' method
denoised_signal_gaussian <- smash(scaled_signal, "gaus")

par(mfrow = c(2, 1))

# Plot 1: True Signal with Gaussian Noise
plot(x_vector, true_signal, type = "l", col = "blue", main = "Original Signal", ylab = "Value", xlab = "x",cex.lab=0.8, cex.main = 0.8)

legend("topright", legend = c("True Signal"), col = c("blue"), lty=1,cex=0.7)

# Plot 2: Noisy Signal vs. Denoised Signal
plot(x_vector, scaled_signal, type = "l", col = "red", 
     main = "Noisy vs Denoised Signal", ylab = "value", 
     xlab = "x", cex.main = 0.8, cex.lab=0.8)
lines(x_vector, denoised_signal_gaussian, col = "black")
legend("topright", legend = c("Noisy Signal", "Denoised Signal"), col = c("red", "black"), lty = c(1,1),cex=0.7)


