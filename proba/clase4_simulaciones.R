#1. Implementar una función llamada promedios(N_infty,distribucion, tita) que tenga por parámetros N_infty, 
# representando la cantidad máxima de datos que se van a generar, una distribución
#con la que generar los datos y un vector tita con los parámetros asociados a la distribución indicada.
# Debe devolver un vector con promedios utilizando 1,2,. . ., N_infty datos generados bajo
#la distribución indicada, de forma tal que cada promedio se obtiene agregando un dato nuevo a
#los ya generados.

promedios <- function(N_infty, distribucion, tita){
  promedios <- c()
  parametros <- as.list(c(N_infty, tita))
  datos <- do.call(distribucion, parametros)
  
  for (n in 1:N_infty){
    promedios <-c(promedios, mean(datos[1:n]))
  }
  promedios
}

muchos_promedios <- function(N_gente, N_infty, distribucion, tita){
  muchos <- array(dim = c(N_gente, N_infty))
  
  for (i in 1:N_gente){
  muchos[i,1:N_infty] <- promedios(N_infty, distribucion, tita)
  }
  muchos
}

n_max <- 1000
dev.off()
par(mfrow=c(2,3))
plot(x = 1:n_max, y = promedios(n_max, rbinom, c(1,0.2)), main = "Ber(0.2)")
plot(x = 1:n_max, y = promedios(n_max, runif, c(67,73)), main = "U(67,73)")
plot(x = 1:n_max, y = promedios(n_max, rexp, 1/70), main = "exp(1/70)")
plot(x = 1:n_max, y = promedios(n_max, rnorm, c(70,3)), main = "norm(70,3)")
plot(x = 1:n_max, y = promedios(n_max, rlnorm, c(0,1)), main = "lognorm(0,1)")



hist_muchos_promedios<- function( filas, N_gente, N_infty, distribucion, tita, main = ""){
  a <- muchos_promedios( N_gente, N_infty, distribucion, tita)
  par(mfrow=c(length(filas),1))
  
  for (fila in filas){
    hist(a[fila,], xlim = c(min(a[filas[1],]),max(a[filas[1],])), main = main)
  }
}



a <- muchos_promedios(100, 1000, rbinom, c(1,0.2))

# Histograma de la columna 10: representando 100 replicaciones del promedio de n = creciendo. 
# Es decir, vemos la distribución de X_raya 
par(mfrow = c(3,1))
hist(a[,1])
hist(a[,10])
hist(a[,50])

# Scatterplot de la fila 10: vemos UNA realización de X_raya para cada n. 
# Es decir, vemos la distribución de X_raya 
plot(a[10,])


