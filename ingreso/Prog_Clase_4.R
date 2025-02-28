################################################################################
############################# PRIMER MODELO ####################################
################################################################################

########################## EJERCICIO 1 #########################################
n<- 6
sample(1:n, 1)

########################## EJERCICIO 2 ########################################
album_size = 6
x <- sample(1:album_size, album_size, T)
compradas <- album_size
while (length(x[!duplicated(x)]) != 6){
  x <- c(x, sample(1:album_size, 1))
  compradas <- compradas + 1
}
compradas

########################## EJERCICIO 3 ########################################
cuantas_figus <- function(album_size){
  x <- sample(1:album_size, album_size, T)
  compradas <- album_size
  while (length(x[!duplicated(x)]) != album_size){
    x <- c(x, sample(1:album_size, 1))
    compradas <- compradas + 1
  }
  return(compradas)
}

cuantas_figus(6)

########################## EJERCICIO 4 #########################################
n_rep = 1000
i <- 0
res <- c()
while(i < n_rep){
  res <- c(res, cuantas_figus(6))
  i <- i + 1
}

#a
mean(res) # 15

#b
sum(res <= 16) / length(res) # P("completar con 16 figus") = 0.673

#c
quantile(res, probs = 0.90) # 24

########################## EJERCICIO 5 #########################################
simulate_cuantas_figus<- function(n_rep, n_figus = 6){
  i <- 0
  res <- c()
  while(i < n_rep){
    res <- c(res, cuantas_figus(n_figus))
    i <- i + 1
  }
  return(res)
}

n_reps = c(2,10, 20, 50, 100,200,500,1000, 2000,5000,10000)
sims <- lapply(n_reps, simulate_cuantas_figus)
means <- lapply(sims, mean)
plot(n_reps, means)



########################## EJERCICIO 6 #########################################
sims <- simulate_cuantas_figus(n_rep = 100, n_figus = 670)
mean(sims) # 4668 en promedio

################################################################################
############################# SEGUNDO MODELO ###################################
################################################################################

########################## EJERCICIO 1 #########################################
figus_total = 670
figus_paquete = 5
paquete <- sample(1:figus_total, figus_paquete, T)

########################## EJERCICIO 2 #########################################
generar_paquete <- function(figus_total, figus_paquete){
  return(sample(1:figus_total, figus_paquete, replace = T)
)
}
generar_paquete(figus_total = 670, figus_paquete = 5)

########################## EJERCICIO 3 #########################################
cuantos_paquetes <- function(figus_total, figus_paquete){
  album <- c()
  paquetes <- 0
  while (length(album[!duplicated(album)]) != figus_total){
    album <- c(album, generar_paquete(figus_total = figus_total, figus_paquete = figus_paquete))
    paquetes <- paquetes + 1
  }
  return(paquetes)
}
cuantos_paquetes(670, 5)

########################## EJERCICIO 4 #########################################
res <- c()
n_rep = 100
i <- 0
while (i < n_rep){
  res <- c(res, cuantos_paquetes(670, 5))
  i <- i +1
}

mean(res) # 940 en promedio

# Estimación de ejercicios 5 y 6 con este número de reps
# 5
sum(res <= 850) / length(res) # P("completar con 850 paquetes") = 0.32

# 6 
quantile(res, probs = 0.90) # 1141
