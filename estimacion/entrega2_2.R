datos <- scan()
n <- length(datos)
x_item_d <- 120
varianza <- sum((datos - mean(datos))**2) / n


alpha_momentos <- mean(datos)^2 / varianza
alpha_momentos
lambda_momentos <- mean(datos) / varianza
lambda_momentos
dens <- fitdistr(datos, densfun = "gamma")

alpha_mv <- dens$estimate["shape"]
lambda_mv <- dens$estimate["rate"]

1 - round(pgamma(x_item_d, shape = round(alpha_mv,4), rate= round(lambda_mv,4)),4)


hist(datos, freq = FALSE, main = "")

curve(dgamma(x, shape = alpha_mv, rate = lambda_mv),
      col = "blue", lwd = 2, add = TRUE)

curve(dgamma(x, shape = alpha_momentos, rate = lambda_momentos),
      col = "lightblue", lwd = 2, add = TRUE, lty = 2)

lines(density(datos, kernel = "gaussian"), col = "red", lwd = 2)

legend("topright", 
       legend = c("Gamma (MV)", "Gamma  (Momentos)", "KDE"),
       col = c("blue", "lightblue", "red"),
       lwd = 2, 
       lty = c(1, 2, 1),
       bty = "n")


