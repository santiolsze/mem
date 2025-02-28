########## EJERCICIO 1 #########################################################
library(dplyr)
# A
nrow(iris)
ncol(iris)
colnames(iris)

# B
iris %>% filter(Species == 'versicolor') %>% nrow() # 50 individuos versicolor

#C
mean(iris$Petal.Length)

#D 
iris %>% mutate(Petal.Area = Petal.Length * Petal.Width * pi) %>%
  group_by(Species) %>% 
  summarise(Petal.Area.Media = mean(Petal.Area)) # 1.15 de promedio para setosa

#E 
iris %>% filter(Species == 'virginica') %>% 
  mutate(num = Sepal.Width > 3) %>% summarise(freq = sum(num) / n()) #  34%

#F
iris %>% filter(Species == 'setosa') %>% 
  mutate(num = Sepal.Length > 5 & Petal.Width < 0.3) %>% summarise(freq = sum(num) / n()) #  24%

#G
iris <- iris %>% mutate(suma = Petal.Length + Petal.Width + Sepal.Length + Sepal.Width)

#H
atributo = "Petal.Width"
especie_max_media <- function(atributo){
  sp <- iris %>% group_by(Species) %>%
    summarise(mean_attr = mean(!!sym(atributo))) %>%
    arrange(desc(mean_attr)) %>% 
    filter(mean_attr == max(mean_attr)) %>%
    pull(Species) %>% as.character()
  
  return(sp) 
}

especie_max_media("Sepal.Width")

# I
especie_menor_varianza <- function(atributo){
  sp <- iris %>% group_by(Species) %>%
    summarise(var_attr = var(!!sym(atributo))) %>%
    arrange(desc(var_attr)) %>% 
    filter(var_attr == min(var_attr)) %>%
    pull(Species) %>% as.character()
  
  return(sp) 
}
especie_menor_varianza("Petal.Width")

########## EJERCICIO 2 #########################################################
escarab <- read.csv("escarab.csv")

escarab %>%  summarise_all(mean)

########## Análisis exploratorio a partir del manejo de salidas gráficas #######
plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species,
     main = "Relación de medidas de sépalo por especie",
     xlab = "Longitud de sepalo",
     ylab = "Ancho de sepalo")

pairs(iris[,2:5], pch=as.numeric(iris$Species), col = iris$Species,
      main = "Distribución uni y bivariada por especie")


hist(iris$Sepal.Length, ylab="Frecuencia", xlab="Longitud del Sépalo")

plot(iris$Sepal.Length~iris$Species, main="Longitud del sepalo por especie")

barplot(tapply(iris$Sepal.Length,iris$Species,mean), main="Longitud del sepalo por especie")

####### Gráficos básicos: Probando modificar parámetros ########################
# 1

hist(iris$Sepal.Length, ylab="Frecuencia", xlab="Longitud del Sépalo", breaks = 100)
plot(density(iris$Sepal.Length), main="Densidad de LongSepalo")

# 2 

x11() 
par(mfrow = c(1, 4))
for (variable in colnames(iris[1:4])) {
  hist(
    iris[[variable]],
    main = paste("Histograma de", variable),
    ylab = "Frecuencia",
    xlab = variable,
    col = "lightblue",
    border = "black"
  )
}

# 3
dev.off()
colors <- c("lightblue")
species_names <- levels(iris$Species)
par(mfrow = c(1, 4))
for (variable in colnames(iris[1:4])) {
  boxplot(
    iris[[variable]],
    main = paste("Boxplot de", variable),
    ylab = variable,
    col = colors,
    border = "black")
}

# Parece haber datos atípicos en sepal.Width (a ambos extremos), 
# con un valor < q25 - 1.5*IQR y tres > Q75 + 1.5*IQR.
# Sin embargo, mejor analizar como sigue, la distribución por especie,
# ya que se evidencia (en particular con el Petalo), un rango IQR grande.

# 4, 5 y 6, 7
dev.off()
colors <- c("lightblue", "darkred", "darkgreen")
species_names <- levels(iris$Species)
par(mfrow = c(1, 4))
for (variable in colnames(iris[1:4])) {
  plot(
    iris[[variable]]~iris$Species,
    main = paste("Boxplot de", variable),
    ylab = variable,
    xlab = "Especie",
    col = colors,
    border = "black")
  legend("topleft", legend = species_names, fill = colors, title = "Especie")
  # Cambiando legend a topleft se mueve
}

