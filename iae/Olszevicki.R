classknn <- function(x,y, k, x_nuevo){
  indexes <- order(abs(x - x_nuevo))[1:k]
  as.numeric(mean(y[indexes]) > 0.5)
}

clas_prop_loc <- function(x, y, h, x_nuevo){
  values <- y[abs(x - x_nuevo) <= h]
  as.numeric(mean(values) > 0.5)
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
