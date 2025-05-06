#1
seed <- 014
set.seed(seed)
n <- 1000
datos <- sort(rexp(n))
datos
plot(ecdf(datos), main = paste0("seed=", seed, " n=", n)  )
#lines(c(0,1), c(0,1), col = "red")
lines(datos, pexp(datos, rate = 1), col = "red", lwd = 2, lty = 2)

# 2
par(mfrow = c(2,2) ) 
dev.off()

# 6
# Crear secuencia de valores
xs <- seq(from = 0.01, to = 0.99, by = 0.01)

# Calcular densidades
udensity <- dunif(xs, min = 0, max = 1)     # Distribución uniforme entre 0 y 1
expdensity <- dexp(xs, rate = 1)            # Distribución exponencial con tasa 1

# Graficar
plot(xs, udensity,
     type = "l",
     col = "black",
     lwd = 2,
     xlim = c(0,1),
     ylim = c(0, max(c(udensity, expdensity))),
     xlab = "x",
     ylab = "f(x)",
     main = "")

lines(xs, expdensity, col = "red", lwd = 2)

# Agregar leyenda
legend("bottomright",
       legend = c("U(0,1)", "Exp(lambda=1)"),
       col = c("black", "red"),
       lwd = 2)


################## EJERCICIO 2 #################################################
datos <- scan("../../Downloads/buffalo.txt")

# 1.
f <- density(datos, kernel = "gaussian", bw = 5 / sqrt(3))
f.aprox <- approxfun(f) 
plot(f)
bw.nrd(datos)
bw.ucv(datos)
