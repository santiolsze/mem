d <- read.csv(file = "../../Downloads/datos_sim_ninos.csv")

######### EJERCICIOS 1-17 [VER ACT CLASE 4 RESUELTAS] ##########################

######### EJERCICIO 18 #########################################################

n <- length(d$x)

silverman <- 1.06 * min(sd(d$x),  IQR(d$x) / 1.34) * n**(-1/5)
silverman <- bw.nrd(d$x) #nrd0 usa ventana de 0.9
cv.h <- bw.bcv(d$x)
silverman0 <- bw.nrd0(d$x)
hist(d$x, freq = F)
lines(density(d$x, kernel = "gaussian", bw = silverman), col = "darkblue", lwd = 2)
lines(density(d$x, kernel = "gaussian", bw = silverman), col = "darkgreen", lwd = 2)
lines(density(d$x, kernel = "gaussian", bw = cv.h), col = "red", lwd = 2)




