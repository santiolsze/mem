#1. Implementar funciones est1 y est2 que tengan por argumento un conjunto de datos
# (x1, . . . , xn) y devuelvan el valor de la estimaci´on θbn(x1, . . . , xn) y θen(x1, . . . , xn), para los
# estimadores definidos en (1), respectivamente.
# Calcular el valor de dichas estimaciones en cada uno de los siguientes conjuntos de datos:
#  a) 1.17 1.75 0.28 2.56 2.36 0.36 1.82 0.24 1.17 1.86
# b) 0.66 0.07 0.62 0.65 1.33 0.40 1.17 1.11 2.01 2.98
set.seed(123)

est1 <- function(datos){
  2*mean(datos)
}

est2 <- function(datos){
  max(datos) 
}

a <- c(1.17, 1.75, 0.28, 2.56, 2.36, 0.36, 1.82, 0.24, 1.17, 1.86)
b <- c(0.66, 0.07, 0.62, 0.65, 1.33, 0.40, 1.17, 1.11, 2.01, 2.98)


est1(a)
est1(b)

est2(a)
est2(b)
# 2. Generar n = 5 datos correspondientes a una muestra aleatoria con distribuci´on U(0, 3).
xs <- runif(n = 5, max = 3)
est1(xs)
est2(xs)
# 3. Calcular el valor de la estimaci´on correspondiente a θb en base a los datos generados.
# 4. Repetir los dos pasos anteriores R = 1000 veces, obteniendo as´ı m realizaciones de θb a las
#que llamaremos θb1 , . . . , θbR. Realizar un histograma y un boxplot con estos valores.
R <- 1000
for (n in c(10, 30,50,100, 5)){
est1s <- c(); est2s <- c()
for (i in 1:R){
  est1s <- c(est1s, est1(runif(n = n, max = 3)))
  est2s <- c(est2s, est2(runif(n = n, max = 3)))
}
par(mfrow=c(2,2))
hist(est1s, freq = F, main = paste0("[2(mean)] n=", n, " s=",round(sd(est1s),3)), xlim = c(0,6))
hist(est2s, freq = F, main = paste0("[Max] n=", n, " s=",round(sd(est2s),3)), xlim = c(0,6))
boxplot(est1s, ylim = c(0,6), main = paste0("sesgo.emp:", round(mean(est1s) - 3,3)))
boxplot(est2s, ylim = c(0,6), main = paste0("sesgo.emp:", round(mean(est2s) - 3,3)))
}
# 8. Computar el Sesgo emp´ırico de θb mediante la expresi´on
#Utilizaremos las R = 1000 realizaciones θb1
#, . . . , θbR de θb y θe1
#, . . . , θeR de θe, obtenidas en base
#a las muestras de tama˜no n = 5.
s.emp.1 <- mean(est1s) - 3
s.emp.2 <- mean(est2s) - 3

# 11. Computar la Varianza emp´ırica de θb mediante la expresi´on
v.emp.1 <- sum((est1s-mean(est1s))**2) / R
v.emp.2 <- sum((est2s-mean(est1s))**2) / R

# 14. Computar el EMSE

emse.1 <- sum((est1s-3)**2) / R
emse.2 <- sum((est2s-3)**2) / R

