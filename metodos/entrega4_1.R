# ENTREGA 1
# IC media con SD conocido
datos <- scan()
nivel <- 0.04
sigma <- sqrt(0.5)
n <- length(datos)

z <- -qnorm(nivel/2)

lower <- mean(datos) - z*sigma/sqrt(n)
upper <- mean(datos) + z*sigma/sqrt(n)
c(lower, upper)


sqrt(n) <= 2*-qnorm(nivel/2)*sigma / 0.11

(2*-qnorm(nivel/2)*sigma / 0.11)**2

# ENTREGA 2
#datos <- scan()
n <- length(datos)

nivel_mu <- 0.04

z <- -qt(nivel_mu/2,df = n-1)
s <- sqrt(var(datos))
lower <- mean(datos) - z*s/sqrt(n)
upper <- mean(datos) + z*s/sqrt(n)
c(lower, upper)


# Sigma cuadrado con u desconocido
(n-1)*s**2/qchisq(nivel_mu/2, n-1)
(n-1)*s**2/qchisq(1-nivel_mu/2, n-1)

# Sigma 
# SQRT de los anteriores


# ENTREGA 3 #

datos <- scan()
n <- length(datos)
nivel <- 0.91
# Parametro
q_left <- qchisq((1-nivel)/2, df = 2*n)
q_right <- qchisq((1-nivel)/2, df = 2*n, lower.tail = F)
est <- 1/mean(datos) # MLE de theta

LI <- q_left/(2*sum(datos))
LS <- q_right/(2*sum(datos))
1/LI
1/LS



