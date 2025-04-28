
#######3.5 Tecnica Bootstrap
set.seed(12345)
reclam <- rlnorm(20, meanlog=2.5, sdlog= 2.5)
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}
bootstrap_desv <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(sd(mse_values) / sqrt(length(mse_values)))
}
#Estimacion del MSE usando bootstrap
n_bootstrap <- 100000
mse_estimate <-bootstrap_mse(reclam, n_bootstrap)
mse_desvz<-bootstrap_desv(reclam, n_bootstrap)
cat("El MSE es", format(mse_estimate, big.mark = ","), "\n")
#IC del MSE
alpha <- 0.05
z <- qnorm(1-alpha/2)
ci_lower_mse <- mse_estimate - z*mse_desvz
ci_upper_mse <- mse_estimate + z*mse_desvz
#Graficar densidad del MSE y los IC al 95%
plot(density(mse_estimate,mse_desvz),
     main = "Distribución Bootstrap del MSE",
     xlab = "MSE", ylab = "Densidad",
     col = "blue", lwd = 2)
abline(v = mse_estimate, col = "orange", lwd = 2, lty = 2)
abline(v = ci_lower_mse, col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_upper_mse, col = "darkgreen", lwd = 2, lty = 3)
legend("topright",
       legend = c("Densidad Bootstrap",
                  paste("Media =", format(mse_estimate, digits = 5)),
                  "IC 95%"),
       col = c("blue", "orange", "darkgreen"),
       lty = c(1, 2, 3), lwd = 2)
#3.5 con archivo reclam.csv
reclam <- c(144, 134, 185, 141, 205, 126, 123, 152, 123, 215, 170, 165, 180, 175, 160, 185, 168, 172, 178, 169)
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}
bootstrap_desv <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(sd(mse_values) / sqrt(length(mse_values)))
}
#Estimacion del MSE usando bootstrap
n_bootstrap <- 100000
mse_estimate <-bootstrap_mse(reclam, n_bootstrap)
mse_desvz<-bootstrap_desv(reclam, n_bootstrap)
cat("El MSE es", format(mse_estimate, big.mark = ","), "\n")
#IC del MSE
alpha <- 0.05
z <- qnorm(1-alpha/2)
ci_lower_mse <- mse_estimate - z*mse_desvz
ci_upper_mse <- mse_estimate + z*mse_desvz
#Graficar densidad del MSE y los IC al 95%
plot(density(mse_estimate,mse_desvz),
     main = "Distribución Bootstrap del MSE archivo reclam.csv",
     xlab = "MSE", ylab = "Densidad",
     col = "blue", lwd = 2)
abline(v = mse_estimate, col = "pink", lwd = 2, lty = 2)
abline(v = ci_lower_mse, col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_upper_mse, col = "darkgreen", lwd = 2, lty = 3)
legend("topright",
       legend = c("Densidad Bootstrap",
                  paste("Media =", format(mse_estimate, digits = 5)),
                  "IC 95%"),
       col = c("blue", "orange", "darkgreen"),
       lty = c(1, 2, 3), lwd = 2)

