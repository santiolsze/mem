######## Ejercicio 2 ##########
set.seed(123)

est1 <- function(datos){
  2*mean(datos)
} # sombrerito

est2 <- function(datos){
  max(datos) 
} # moño

sesgo.emp <- function(datos, tita, estimador, m){
  sum(estimador(datos)) / m  - tita
}
est <- est2

M <- 1000
n <- 50
tita <- 3
estimaciones <- c()

for (m in 1:M){
  estimaciones <- c(estimaciones ,est(datos = runif(n, max = 3)))
}

# Sesgo empírico
sum(estimaciones)/M - tita

# Varianza empírica
sum((estimaciones- mean(estimaciones))**2)/M 



  