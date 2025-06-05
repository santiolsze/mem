library(MASS)

#####################
### EJERCICIO 2 #####
#####################
datos <- scan()

dens <- fitdistr(datos, densfun = "exponential")

a <- 1/dens$estimate

b<-pexp(19, rate = dens$estimate)

c<-qexp(0.27, rate = dens$estimate)


# d)
m <- 19
lambda0 <- 1/21

# Calculo cosas
p <- 1-exp(-lambda0*m)  
lambda_est <- lambda0  
n <- length(datos)

V_asint_P_mv <- (m**2 * exp(-2*lambda_est*m) * lambda_est**2)/n
V_asint_P_np <- p*(1-p)/n
eff <- V_asint_P_mv/V_asint_P_np
round( eff, 4)

#####################
### EJERCICIO 3 #####
#####################

K <- 3:20
n <- 10
B <-2000

e1 <- e2 <- e3 <- matrix(NA, nrow = B,
              ncol = length(K))


for (b in 1:B){
  for (k in 1:length(K)){
  sample <- rt(n, K[k])
  e1[b,k] <- mean(sample)
  e2[b,k] <- median(sample)
  e3[b,k] <- mean(sample, trim =.1)}
  
}
e1_vars <- apply(e1,2,var)
e2_vars <- apply(e2,2,var)
e3_vars <- apply(e3,2,var)

# Create plot with e3_vars/e1_vars
plot(K, e1_vars / e3_vars,
     type = "o", pch = 16, col = "blue", lty = 1, lwd = 1,
     xlab = "Grados de libertad de rt", ylab = "Eficiencia",
     main = "Eficiencia relativa", xlim = c(0, max(K)), ylim = c(0,2), cex = 0.6)

# Add minor ticks every 1 (unlabeled)
axis(side = 1, at = seq(0, max(K), by = 1), labels = FALSE, tcl = -0.2)


# Add second line: e2_vars/e1_vars
lines(K, e1_vars / e2_vars,
      type = "o", pch = 17, col = "red", lty = 2, lwd = 1, cex = 0.6)

# Add a reference line at 1 (baseline)
abline(h = 1, col = "gray", lty = 3)

# Add legend
legend("bottomright",
       legend = c("Media truncada vs media", "Mediana vs media"),
       col = c("blue", "red"), pch = c(16, 17), lty = c(1, 2), lwd = 1,
       bty = "n", cex = .7)

