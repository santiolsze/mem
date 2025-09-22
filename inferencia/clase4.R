Nrep = 1000
n = 5
sigma = 1
nivel = 0.05
u0 = 0
us <- seq(from=-1, to = 1, by = 0.1)

test_mayor <- test_menor <- test_bilateral <- numeric(Nrep)

# NIVEL EMPIRICO
for (rep in 1:Nrep){
  muestra <- rnorm(n = n, mean = 0, sd = sigma)
  test_mayor[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma > qnorm(1-nivel)
  test_menor[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma < qnorm(nivel)
  test_bilateral[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma < qnorm(nivel/2) | sqrt(n)*(mean(muestra) - u0)/sigma > qnorm(1-nivel/2)
} 

mean(test_mayor)
mean(test_menor)
mean(test_bilateral)

# POTENCIA EMPÍRICA
pot_mayor <- pot_menor <- pot_bilateral <- numeric(u)

for (u_index in 1:length(us)){
  test_mayor <- test_menor <- test_bilateral <- numeric(Nrep)
for (rep in 1:Nrep){
  muestra <- rnorm(n = n, mean = us[u_index], sd = sigma)
  test_mayor[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma > qnorm(1-nivel)
  test_menor[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma < qnorm(nivel)
  test_bilateral[rep] <- sqrt(n)*(mean(muestra) - u0)/sigma < qnorm(nivel/2) | sqrt(n)*(mean(muestra) - u0)/sigma > qnorm(1-nivel/2)
} 
  pot_mayor[u_index] <- mean(test_mayor)
  pot_menor[u_index] <- mean(test_menor)
  pot_bilateral[u_index] <- mean(test_bilateral)
  
  
}


plot(us, pot_menor, type = "l", col = "blue", lwd = 2,
     xlab = "medias", ylab = "Potencia",
     main = "Potencia",
     ylim = range(c(pot_menor, pot_mayor, pot_bilateral)))


lines(us, pot_mayor, col = "red", lwd = 2)
lines(us, pot_bilateral, col = "darkgreen", lwd = 2)


abline(h = nivel, col = "gray", lty = 2, lwd = 2)


legend("top", cex = 0.5, legend = c("Unilateral: Menor", "Unilateral: Mayor", "Bilateral"),
       col = c("blue", "red", "darkgreen"), lwd = 2, lty = 1)

