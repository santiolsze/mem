classknn <- function(x,y, k, x_nuevo){
  indexes <- order(abs(x - x_nuevo))[1:k]
  as.numeric(mean(y[indexes]) > 0.5)
}

clas_prop_loc <- function(x, y, h, x_nuevo){
  values <- y[abs(x - x_nuevo) <= h]
  as.numeric(mean(values) >= 0.5)
}

clas_gen <- function(x, y, x_nuevo, h0, h1){
  dens0 <- density(x[y == 0], kernel = "gaussian", bw = h0, from = x_nuevo, to = x_nuevo, n = 1)$y
  dens1 <- density(x[y == 1], kernel = "gaussian", bw = h1, from = x_nuevo, to = x_nuevo, n = 1)$y
  propm0 <- mean(y == 0)
  propm1 <- mean(y == 1)
  aux0 <- dens0*propm0
  aux1 <- dens1*propm1
  if(aux0 >= aux1)
  {return(0)}else{return(1)}
}


# Completar con el valor de la cantidad óptima de vecinos más cercanos hallada 
# en el ítem 8 de la Sección 3 de la Práctica 5 (recordar que consideraremos el
#mayor entre todos los que minimizan a la función CV(k)

alturas <- read.csv("../../Downloads/alturas_n_490.csv")
alturas$mujer <- (alturas$genero=="F")*1
ks <- 5:100

loocv.kn <- function(x,  y,  ks){
  cvs <- c()
  for (k in ks){
    cv <- 0
    for (i in 1:length(x)){
      pred <- classknn(x = x[-i],
               y = y[-i],
               k = k,
               x_nuevo = x[i])
      cv <- cv + (y[i] != pred)
    }
    cvs <- c(cvs, cv/length(x))
    
  }
  cvs
} 
ks <- 5:100

loocv.cvs <- loocv.kn(x = alturas$altura,  y = alturas$mujer,  ks = ks)
cbind(ks, loocv.cvs)[loocv.cvs == min(loocv.cvs),]


loocv.prop.loc <- function(x,  y,  hs){
  cvs <- c()
  for (h in hs){
    cv <- 0
    for (i in 1:length(x)){
      
      #pred <- clas_prop_loc(x = x[-i], y = y[-i],h = h, x_nuevo = x[i])
      pred <- ksmooth(x = x[-i], y = y[-i], bandwidth = 2*h, x.points = x[i])$y > 0.5
      cv <- cv + (y[i] != pred)
    }
    cvs <- c(cvs, cv/length(x))
    
  }
  cvs
} 

hs <- seq(1.5,12, 0.1)
loocv.cvs <- loocv.prop.loc(x = alturas$altura,  y = alturas$mujer,  hs = hs)
cbind(hs, loocv.cvs)[loocv.cvs == min(loocv.cvs),]

