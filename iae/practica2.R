################################################################################
##########################   EJERCICIO 1   #####################################
################################################################################
# 1. Descargar de la p´agina dos conjuntos de datos con n = 50 y n = 500 observaciones,
# respectivamente, ingresando como nro de identificaci´on los 5 ´ultimos n´umeros del DNI de
# alg´un integrante del grupo y con todas las variables.
alt50 <- read.csv("../../Downloads/alturas_n_50.csv")
alt500 <- read.csv("../../Downloads/alturas_n_500.csv")

# 2. Identificar el nombre de las columnas del data frame.
colnames(alt50)

# 3. Predecir la altura de un individuo, de la que no se tiene ninguna informaci´on. Ingresar los
# valores predichos en las columnas “n = 50” y “n = 500” de la siguiente planilla. Importante:
# utilice la coma (“,”) como separador decimal.
alt50$altura %>% mean() # 167.572
alt500$altura %>% mean() # 166.572
# 4. Realizar un histograma de las alturas de los individuos. Establecer 15 clases para la
# construcci´on de los mismos. ¿Cu´antas modas se observan? ¿A qu´e se puede atribuir?
par(mfrow=c(2,1))
hist(alt50$altura, nclass = 15) 
hist(alt500$altura, nclass = 15)

#5. Predecir la altura de un individuo de g´enero masculino (hijo) y comparar con la predicci´on
# anterior.
alt50[alt50$genero == "M",]$altura %>% mean() # 172.3968
alt500[alt500$genero == "M",]$altura %>% mean() # 172.1549

#6. Predecir la altura de un hijo cuya madre es de contextura peque˜na y comparar con el valor del
# ´ıtem anterior.

table(alt50$genero, alt50$contextura_madre) # no tengo como para hacer el simple promedio
alt500[alt500$genero == "M" & alt500$contextura_madre == "bajita",]$altura %>% mean() # 169.2


################################################################################
##########################   EJERCICIO 2   #####################################
################################################################################
library(ggplot2)

# 7. Graficar la altura de la mam´a (en el eje x) vs. la altura del individuo (eje y), utilizando un
# color distinto por cada g´enero. ¿Qu´e se puede observar?
ggplot(data = alt500, mapping = aes(x = altura_madre, y = altura, colour = genero)) + geom_point()

#En adelante, trabajaremos s´olo con los datos de los varones. Para ello, crear un data frame
#llamado alturasdat500m que contenga los datos correspondientes a los varones.
alturasdat500m <- alt500[alt500$genero == 'M',]

# 8. Indicar si hay alguna madre de un var´on cuya altura sea 156 cm.
# Depende lo que pida la consigna, hay o no
alturasdat500m[alturasdat500m$altura == 156,]
alturasdat500m[alturasdat500m$altura_madre == 156,]

#9. Predicir la altura de un var´on cuya madre mide x = 156 (cm) calculando el promedio local
#centrado en 156 con ventana de ancho h = 1 (cm). Para ello:

#a) Indicar cu´antos casos hay donde la madre registra una altura entre 155 y 157 cm inclusive.
nrow(alturasdat500m[alturasdat500m$altura_madre >= 155 & alturasdat500m$altura_madre <= 157,]) #43

#b) Calcular el promedio de la altura de los varones cuyas madres registran una altura entre
     #155 y 157 cm.

mean(alturasdat500m[alturasdat500m$altura_madre >= 155 &
                      alturasdat500m$altura_madre <= 157,
                    "altura"]) # 170.6023

#c) repetir con h = 2.
mean(alturasdat500m[alturasdat500m$altura_madre >= 154 &
                      alturasdat500m$altura_madre <= 158,
                    "altura"]) # 170.7321

################################################################################
##########################   EJERCICIO 3   #####################################
################################################################################
#11. Implementar una funci´on que en base a los datos de la altura de las madres (x) y de la altura
#de sus hijos (y), permita predecir la altura de un hijo cuya madre tiene altura igual a x nueva,
#usando una ventana de tama˜no h, mediante el c´alculo del promedio local. Es decir, defina la
#funci´on
# 
pred_prom_loc <- function(x, y, x_nueva, h){
  mean(y[x >= (x_nueva - h) & x <= (x_nueva + h)])
}

pred_prom_loc(x = alturasdat500m$altura_madre,
              y = alturasdat500m$altura,
              x_nueva = 156,
              h = 2
              )

# 12. Graficar la funci´on predictora de la altura por promedios locales, pred prom loc, para h=1 en
# base a los datos de los varones que guard´o en alturasdat500m. Para ello, generar una grilla
# de 100 valores equidistantes entre 151 y 168 para x nueva y evaluar la funci´on en cada uno de
# esos puntos.
iterate_pred_prom_loc <- function(x, y, x_nuevas, h){
  res <- c()
  for (x_nueva in x_nuevas){
  res <- c(res, pred_prom_loc(x = x,
                              y = y,
                              x_nueva = x_nueva,
                              h = h))
  }
  res
}
  
x_nuevas <- seq(151,168, length.out = 100)
h1 <- iterate_pred_prom_loc(x = alturasdat500m$altura_madre,
                            y = alturasdat500m$altura,
                            x_nuevas = x_nuevas,
                            h = 1)

h2 <- iterate_pred_prom_loc(x = alturasdat500m$altura_madre,
                           y = alturasdat500m$altura,
                           x_nuevas = x_nuevas,
                           h = 2)

h3 <- iterate_pred_prom_loc(x = alturasdat500m$altura_madre,
                            y = alturasdat500m$altura,
                            x_nuevas = x_nuevas,
                            h = 3)

h5 <- iterate_pred_prom_loc(x = alturasdat500m$altura_madre,
                            y = alturasdat500m$altura,
                            x_nuevas = x_nuevas,
                            h = 5)

h50 <- iterate_pred_prom_loc(x = alturasdat500m$altura_madre,
                            y = alturasdat500m$altura,
                            x_nuevas = x_nuevas,
                            h = 50)


plot(x_nuevas, h1, col = "darkred", type = "lines", lwd =1)
lines(x_nuevas, h2, col = "gray", lwd =1.5)
lines(x_nuevas, h3, col = "darkblue", lwd =2)
lines(x_nuevas, h5, col = "darkgreen", lwd =2.5)
lines(x_nuevas, h50, col = "black", lwd =1.5, lty = "dashed")

################################################################################
##########################   EJERCICIO 4   #####################################
################################################################################
