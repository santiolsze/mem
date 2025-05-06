# Ejercicio 2, Práctica 6
rej2 <- function(n){
    U <- runif(n, min = 0, max = 1) # 
    X <- sqrt(U) # La inversa de Fx evaluada en la uniforme
    Y <- rexp(n, 1/X) # Y | X=x ~ Exp(1/X) 
  cbind(X,Y)
}
datos <- rej2(n = 1000)

EX_hat <- mean(datos[,1]) 
EY_hat <- mean(datos[,2])
COV_est <- mean(datos[,1] * datos[,2]) -  mean(datos[,1]) * mean(datos[,2])
