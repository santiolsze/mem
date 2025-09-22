library(MASS)
NRep <- 5000
n <- 1000
mu <- c(0,0)
Sigma <- matrix(c(10,0,0,100), ncol = 2, byrow = T)
Sigma <- matrix(c(10,5*sqrt(10),5*sqrt(10),100), ncol = 2, byrow = T)
true_lambdas <- eigen(Sigma)$values
samples <- lapply(1:NRep, function(i) mvrnorm(n = n, mu, Sigma, tol = 1e-06, empirical = FALSE))
lambdas <- lapply(samples, function(i) eigen(cov(i), symmetric = T)$values)
lambda1 <- lapply(lambdas, function(i) i[1])
lambda2 <- lapply(lambdas, function(i) i[2])

par(mfrow = c(2,1))
hist(unlist(lambda1), main = paste("Autovalor #1"), probability = T)
lines(density(unlist(lambda1)))
abline(v = true_lambdas[1], col = "red", lwd = 3, lty = 1)
abline(v = mean(unlist(lambda1)), col = "black", lwd = 2, lty = 2)

hist(unlist(lambda2), main = "Autovalor #2", probability = T)
lines(density(unlist(lambda2)))
abline(v = true_lambdas[2], col = "darkblue", lwd = 5, lty = 1)
abline(v = mean(unlist(lambda2)), col = "black", lwd = 2, lty = 2)



################################################################
