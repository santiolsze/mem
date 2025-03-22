lidar <- read.table("../../Downloads/lidar.txt", header = T)
source("iae/practica2.R") # Me traigo las funciones
colnames(lidar)
dev.off()

# EJ 2
plot(x = lidar$range, y = lidar$int.conc, type = "p")
points(602, pred_prom_loc(x = lidar$range, y = lidar$int.conc, x_nueva = 602, h = 5), col = "red")

# EJ 3
x_nuevas <- min(lidar$range):max(lidar$range)
h5 <- iterate_pred_prom_loc(x = lidar$range,
                      y = lidar$int.conc,
                      x_nuevas = x_nuevas,
                      h = 5)

h10 <- iterate_pred_prom_loc(x = lidar$range,
                             y = lidar$int.conc,
                             x_nuevas = x_nuevas,
                             h = 10)

h30 <- iterate_pred_prom_loc(x = lidar$range,
                             y = lidar$int.conc,
                             x_nuevas = x_nuevas,
                             h = 30)

lines(x_nuevas, h5, col = "darkred", lwd = 2)
lines(x_nuevas, h10, col = "red", lwd = 2)
lines(x_nuevas, h30, col = "pink", lwd = 2)

# EJ 4

lines(x_nuevas,
      ksmooth(x = lidar$range,
        y = lidar$int.conc,
        kernel = "normal",
        bandwidth = 5,
        x.points = x_nuevas)$y,
      col = "gold", lwd = 3)

# EJ 5 

lines(x_nuevas,
      ksmooth(x = lidar$range,
              y = lidar$int.conc,
              kernel = "normal",
              bandwidth = 10,
              x.points = x_nuevas)$y,
      col = "gray", lwd = 3)

lines(x_nuevas,
      ksmooth(x = lidar$range,
              y = lidar$int.conc,
              kernel = "normal",
              bandwidth = 30,
              x.points = x_nuevas)$y,
      col = "darkblue", lwd = 3)

lines(x_nuevas,
      ksmooth(x = lidar$range,
              y = lidar$int.conc,
              kernel = "normal",
              bandwidth = 50,
              x.points = x_nuevas)$y,
      col = "black", lwd = 3)

# EJ 6 
# Con la funci´on ksmooth, predecir el logratio para los valores observados del rango
# usando cada una de las 4 ventanas del ´ıtem anterior y luego computar el Error
# Cuadr´atico Medio de entrenamiento (MSEtrain(h)) para cada h.

for (h in c(5,10,30,50)){
mse.train <- sum((lidar$int.conc  - ksmooth(x = lidar$range,
        y = lidar$int.conc,
        bandwidth = h)$y)**2) / length(lidar$int.conc)

print(paste(h, mse.train))
}

# EJ 7
loocv <- function(x, y, h){
indexes <- 1:length(x)
partial_cv <- 0
for(i in indexes){
pred <- ksmooth(x = x[-i],
        y = y[-i],
        kernel = "normal",
        bandwidth = h,
        x.points = x[i])$y
partial_cv <- partial_cv + (y[i] - pred)**2}
partial_cv / length(x)
}

hs <- 3:165
cvhgrilla <- c()
for (i in 1:length(hs)){
  
  cvhgrilla[i] <- loocv(x = lidar$range,
                        y = lidar$int.conc,
                        h = hs[i])
}
hs[order(cvhgrilla)[1]]
plot(hs, cvhgrilla)

# EJ 8
cvhgrilla[hs[order(cvhgrilla)[1]]] #CV(hopt)

#mse train
sum((lidar$int.conc  - ksmooth(x = lidar$range,
                                            y = lidar$int.conc,
                                            kernel = "normal",
                                            bandwidth = 41)$y)**2) / length(lidar$int.conc)


# 11
pred_knn <- function(x, y, x_nuevo, k){
  indexes <- order(abs(x - x_nuevo))[1:k]
  mean(y[indexes])
}
# 12
#Utilizar la funci´on definida en el´ıtem anterior para obtener predicciones para logratio
#por el m´etodo de vecinos m´as cercanos para un valor de rango igual a 570 utilizando
#k = 20 y 40 y comparar los resultados obtenidos aqu´ı junto con el que utiliza k =
#  5.
for (k in c(5,20,40)){
  p<- pred_knn(x = lidar$range,
           y = lidar$int.conc,
           x_nuevo = 570,
           k = k)
  print(paste(k,p))
}
