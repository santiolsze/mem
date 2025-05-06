hongos <- read.table("../../Downloads/hongos_clasificados.txt", header = T)
head(hongos)

# Ejercicio 1
boxplot(hongos$Height ~ hongos$Variety)


# Ejercicio 2
hongos$Indi <- (hongos$Variety - 1)*1

# Ejercicio 3-5 (METODOS DISCRIMINATIVOS)
classknn <- function(x,y, k, x_nuevo){
  indexes <- order(abs(x - x_nuevo))[1:k]
  as.numeric(mean(y[indexes]) > 0.5)
}

clas_prop_loc <- function(x, y, h, x_nuevo){
  values <- y[abs(x - x_nuevo) <= h]
  as.numeric(mean(values) > 0.5)
}


classknn(hongos$Height, hongos$Indi, k = 8, x_nuevo = 5.2)
classknn(hongos$Height, hongos$Indi, k = 8, x_nuevo = 6)

attributes(FNN::knn(train = hongos$Height, test = matrix(c(5.2,6), ncol = 1), cl = as.factor(hongos$Indi), k = 8, prob = T))$prob



clas_prop_loc(hongos$Height, hongos$Indi, h = 0.1, x_nuevo = 5.2)
clas_prop_loc(hongos$Height, hongos$Indi, h = 0.1, x_nuevo = 6)

ksmooth(hongos$Height, hongos$Indi, kernel = "box", bandwidth = 0.2, x.points = c(5.2,6))


# EJERCICIO 6: METODO GENERATIVO

f0 <- density(hongos$Height[hongos$Indi == 0], kernel = "gaussian", bw = bw.bcv(hongos$Height[hongos$Indi == 0]))
f1 <- density(hongos$Height[hongos$Indi == 1], kernel = "gaussian", bw = bw.bcv(hongos$Height[hongos$Indi == 1]))

hist(hongos$Height[hongos$Indi == 0], freq = F, col = "lightgreen")
lines(f0, col = "darkgreen")
hist(hongos$Height[hongos$Indi == 1], freq = F, col = "pink", add = T)

lines(f1, col = "red")


p1 <- mean(hongos$Indi)
p0 <- 1-p1

f0.aprox <- approxfun(f0) # Tiene que haber una manera de ver la densidad exacta
f1.aprox <- approxfun(f1)

x_nuevo <- 6
f1.aprox(x_nuevo)*p1 > f0.aprox(x_nuevo)*p1


bayes_cutoff_p0.5 <- uniroot(function(x){f0.aprox(x) - f1.aprox(x)}, lower = 5.2, upper = 6)$root
bayes_cutoff_p.real <- uniroot(function(x){p0*f0.aprox(x) - p1*f1.aprox(x)}, lower = 5.2, upper = 6)$root
abline(v = bayes_cutoff_p.real, col = "darkgreen", lty = 2, lwd = 2)
abline(v = bayes_cutoff_p0.5, col = "black", lty = 1, lwd = 0.5)


f0 <- density(hongos$Height[hongos$Indi == 0], kernel = "gaussian", bw = bw.bcv(hongos$Height[hongos$Indi == 0]))
f1 <- density(hongos$Height[hongos$Indi == 1], kernel = "gaussian", bw = bw.bcv(hongos$Height[hongos$Indi == 1]))


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


loocv_ec <- function(x, y, h0, h1){
  sumatoria <- 0
  for (i in 1:length(x)){
  y_pred <- clas_gen(x = x[-i], y = y[-i], x_nuevo = x[i], h0 = h0, h1 = h1)
  sumatoria <- y_pred
  }
  y_pred / length(x)
  }

loocv_ec(hongos$Height,
         hongos$Indi, 
         h0 = bw.bcv(hongos$Height[hongos$Indi == 0]),
         h1 = bw.bcv(hongos$Height[hongos$Indi == 1])
         )
