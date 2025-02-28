# 1. Inspeccionar el conjunto de datos “airquality” (Calidad del aire), contenido dentro de R.
airquality
nrow(airquality)
# 2. Considerar la variable “Ozone”. ¿Qu´e tipo de variable es?
oz <- na.exclude(airquality$Ozone)
class(oz)
# a) ¿Cuantos datos hay registrados?
length (oz)
# b) Representar los datos observados sobre la recta real.
plot(oz, rep(0, length(oz)), axes = F)
axis(1)
abline(h=0, col="black")  # Línea de base

# c) Obtener el m´ınimo valor, el primer cuartil, la media, la mediana, el tercer
# cuartil y el m´aximo valor. Como as´ı tambi´en el desv´ıo est´andar y la distancia
# intercuartil.
summary(oz)
sd(oz, na.rm = T)
IQR(oz, na.rm = T)
# d) ¿C´omo se obtiene el primer cuartil? (Presentar los datos ordenados de menor a mayor.)
sort(oz)[116 * .25]  # Justo da un entero, así que en la px 29 hay un 18 y en la 30 tambien; así que en promedio va a ser 18.
# e) Graficar las medidas de posici´on obtenidas en la misma recta en la que figuran los datos.

# f ) Definir Boxplot.
# g) Representar un boxplot de la variable Ozone por cada mes registrado (Boxplots paralelos)
boxplot(airquality$Ozone ~ airquality$Month)
# h) ¿Existe alguna relaci´on entre la media y la mediana muestrales?

summary(oz)  # La media es más grande que la mediana


# i) Graficar el histograma de frecuencias.
hist(oz, breaks = 10)

# j) ¿C´omo se obtienen las alturas de las barras? 
...
# k) Graficar el histograma de densidad. ¿En qu´e se diferencia del histograma de frecuencias?
hist(oz, breaks = 10, freq =  F)
# l) ¿C´omo se obtienen las alturas de las barras en este caso?
...
# m) En base al histograma de densidad, obtener la proporci´on de observaciones que se encuentran entre 0 y 40.
sum(oz < 40 & oz >= 0) / length(oz)
# n) ¿Qu´e sucede al refinar el histograma?
hist(oz, 10000)
# ñ) Realizar un histograma de Ozone para cada mes considerado.
library(ggplot2)

ggplot(airquality, aes(x=Ozone)) +
  geom_histogram(fill="gray", color="black") +
  facet_wrap(~ Month, nrow = 1) +
  labs(title="Histograma de ozono por mes") + theme_minimal()

# 3. Considerar la variable “Month”. ¿Qu´e tipo de variable es?
class(airquality$Month) # not great
# a) Definirla como factor.
mes <- as.factor(airquality$Month)
class(mes)
# b) ¿Cuáles son sus posibles valores (niveles)?
levels(mes)
# c) Obtener la cantidad de d´ıas regitrados para cada mes y representarlos en un gráfico de barras.
barplot(table(mes))
# d) ¿En qu´e se diferencian un gr´afico de barras de un histograma?
...
# e) Crear una variable factor que indique con 1 si la concentraci´on de Ozone es inferior a 30 y 0 en caso contrario y obtener, por mes, la proporci´on de d´ıas
# cuya concentraci´on de Ozono es inferior a 30.