library(smashr)
num_points <- 2^10
x_vector <- seq(0, 1, length.out = num_points)

# spike function
signal_function <- function(x) {
  0.3 * exp(-800 * (x - 0.12)^2) +
    1.6 * exp(-1000 * (x - 0.34)^2) +
    0.78 * exp(-1800 * (x - 0.98)^2) +
    2.9 * exp(-2200 * (x - 0.56)^2) +
    1.23 * exp(-200 * (x - 0.77)^2)
}

signal_s <- signal_function(x_vector)
signal_t <- 0.02 + signal_s
scaled_signal <- rpois(num_points, signal_t)

# Denoise the Gaussian noisy data using the 'smash' method
denoised_signal_gaussian <- smash(scaled_signal, "poiss")
par(mfrow = c(2, 1))

# Plot 1: True Signal with possion
plot(x_vector, true_signal, type = "l", col = "blue", main = "Original Signal", ylab = "Value", xlab = "x",cex.lab=0.8, cex.main = 0.8)

legend("topright", legend = c("True Signal"), col = c("blue"), lty=1,cex=0.7)

# Plot 2: Noisy Signal vs. Denoised Signal
plot(x_vector, scaled_signal, type = "l", col = "red", 
     main = "Noisy vs Denoised Signal", ylab = "value", 
     xlab = "x", cex.main = 0.8, cex.lab=0.8)
lines(x_vector, denoised_signal_gaussian, col = "black")
legend("topright", legend = c("Noisy Signal", "Denoised Signal"), col = c("red", "black"), lty = c(1,1),cex=0.7)


