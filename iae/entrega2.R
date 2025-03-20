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

lines(x_nuevas, h5, col = "darkred")
lines(x_nuevas, h10, col = "red")
lines(x_nuevas, h30, col = "pink")

