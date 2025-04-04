hongos <- read.table("../../Downloads/hongos_clasificados.txt", header = T)
head(hongos)

# Ejercicio 1
boxplot(hongos$Height ~ hongos$Variety)


# Ejercicio 2
hongos$Indi <- (hongos$Variety - 1)*1

# Ejercicio 3-5 (METODOS DISCRIMINATIVOS)
classknn <- function(x,y, k, x_nuevo){
  indexes <- order(abs(x - x_nuevo))[1:k]
  print(paste0("Prop:", mean(y[indexes])))
  
  as.numeric(mean(y[indexes]) > 0.5)
}

clas_prop_loc <- function(x, y, h, x_nuevo){
  values <- y[abs(x - x_nuevo) <= h]
  print(paste0("Prop:", mean(values)))
  as.numeric(mean(values) > 0.5)
}


classknn(hongos$Height, hongos$Indi, k = 8, x_nuevo = 5.2)
classknn(hongos$Height, hongos$Indi, k = 8, x_nuevo = 6)

attributes(FNN::knn(train = hongos$Height, test = matrix(c(5.2,6), ncol = 1), cl = as.factor(hongos$Indi), k = 8, prob = T))$prob



clas_prop_loc(hongos$Height, hongos$Indi, h = 0.1, x_nuevo = 5.2)
clas_prop_loc(hongos$Height, hongos$Indi, h = 0.1, x_nuevo = 6)

ksmooth(hongos$Height, hongos$Indi, kernel = "box", bandwidth = 0.2, x.points = c(5.2,6))

