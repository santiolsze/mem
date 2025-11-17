library(ggplot2)
library(dplyr)
library(MASS)
data <- read.csv("Downloads/wbcd.csv")
data$diagnosis <- as.factor(data$diagnosis)
data.x <- data[,2:ncol(data)] # Saco la etiqueta del conjunto para aplicar PCA
data.x.c <- scale(data.x, center = T, scale = T) # Centro las X

################################################################################
#1)  Obtener PC. ¿Hay evidencia para suponer que una clasificación podría separar?
################################################################################

pca.fit <- prcomp(data.x.c)

screeplot(pca.fit)

ggplot(data = pca.fit$x, aes(x = PC1, y = PC2)) + geom_point(aes(colour = data$diagnosis)) +
  geom_abline(slope = 2, intercept = 0)

# Se evidencia una clara separación entre clases, con una frontera que aparenta ser bastante lineal. 
# Agrego, de hecho, la linea de pendiente 2 e intercepto 0, para evidenciar lo comentado anteriormente.

################################################################################
# 2) Ajustar modelos dx ~ PC1 + PC2 y calcular errores empíricos de clasificacion.
################################################################################

data.clasif <- data.frame(pca.fit$x) %>% dplyr::select(PC1, PC2) %>% mutate(diagnosis = data$diagnosis)

ggplot(data =data.clasif, aes(x = PC1, y = PC2, colour = diagnosis)) + geom_point()

# Ajuste de modelos
lda.fit <- lda(diagnosis ~ PC1 + PC2, data = data.clasif)
qda.fit <- qda(diagnosis ~ PC1 + PC2, data = data.clasif)
lr.fit <- glm(diagnosis ~ PC1 + PC2, data = data.clasif, family = binomial)

# Obtengo p_sombrero para cada observación
lda.predict <- predict(lda.fit)$posterior[,2]
qda.predict <- predict(qda.fit)$posterior[,2]
lr.predict <- predict(lr.fit, type = "response")

# Obtengo y_sombrero para cada observación (como factor)
lda.predict.bin <- as.factor(as.numeric(lda.predict >= 0.5))
qda.predict.bin <- as.factor(as.numeric(qda.predict >= 0.5))
lr.predict.bin <- as.factor(as.numeric(lr.predict >= 0.5))


EEC.lda <- sum(lda.predict.bin != data.clasif$diagnosis) / nrow(data.clasif)
EEC.qda <- sum(qda.predict.bin != data.clasif$diagnosis) / nrow(data.clasif)
EEC.lr <- sum(lr.predict.bin != data.clasif$diagnosis) / nrow(data.clasif)

# Presento errores empíricos de clasificacion de entrenamiento
data.frame("LDA" = EEC.lda, "QDA" = EEC.qda, "LR" = EEC.lr) %>% round(4)

################################################################################
# 3) Repito con las primeras 5 componentes
################################################################################

data.clasif.2 <- data.frame(pca.fit$x) %>% dplyr::select(PC1, PC2, PC3, PC4, PC5) %>% mutate(diagnosis = data$diagnosis)

# Ajuste de modelos
lda.fit.2 <- lda(diagnosis ~ PC1 + PC2 + PC3 + PC4 + PC5, data = data.clasif.2)
qda.fit.2 <- qda(diagnosis ~  PC1 + PC2 + PC3 + PC4 + PC5, data = data.clasif.2)
lr.fit.2 <- glm(diagnosis ~  PC1 + PC2 + PC3 + PC4 + PC5, data = data.clasif.2, family = binomial)

# Obtengo p_sombrero para cada observación
lda.predict.2 <- predict(lda.fit.2)$posterior[,2]
qda.predict.2 <- predict(qda.fit.2)$posterior[,2]
lr.predict.2 <- predict(lr.fit.2, type = "response")

# Obtengo y_sombrero para cada observación (como factor)
lda.predict.bin.2 <- as.factor(as.numeric(lda.predict.2 >= 0.5))
qda.predict.bin.2 <- as.factor(as.numeric(qda.predict.2 >= 0.5))
lr.predict.bin.2 <- as.factor(as.numeric(lr.predict.2 >= 0.5))


# Calculo EEC como proporcion de errores (p_hat)
EEC.lda.2 <- sum(lda.predict.bin.2 != data.clasif.2$diagnosis) / nrow(data.clasif.2)
EEC.qda.2 <- sum(qda.predict.bin.2 != data.clasif.2$diagnosis) / nrow(data.clasif.2)
EEC.lr.2 <- sum(lr.predict.bin.2 != data.clasif.2$diagnosis) / nrow(data.clasif.2)

# Presento errores empíricos de clasificacion de entrenamiento
resultados_finales <- data.frame("PC_n" = c(2,5),
          "LDA" = c(EEC.lda,EEC.lda.2),
          "QDA" = c(EEC.qda, EEC.qda.2),
          "LR" = c(EEC.lr, EEC.lr.2)) %>% round(4)
resultados_finales
################################################################################
# 4) JUSTIFICACIÓN
################################################################################
# En el planteo general de los problemas de clasificación, se busca minimizar el
# error cuadrático medio de clasificación de nuevos datos. Sin embargo, dado que 
# la consigna indica no separar el dataset, me basaré en la interpretación del 
# EEC en los datos de entrenamiento como proxy. Cabe destacar, que los EEC de 
# entrenamiento, cualquiera sea el modelo, mejora al aumentar la complejidad 
# (en este caso, al agregar más componentes). Esto nos restringe al afirmar  
# en forma directa decir que la estrategia de 5 componentes es mejor, ya que en 
# cualquier caso más complejidad implica mejor ajuste sobre los datos de 
# entrenamiento. A pesar de lo mencionado anteriormente,son modelos suficientemente
# sencillos como para pensar que el efecto observado puede deberse solamente a un sobreajuste.

# En la búsqueda de reducir dimensiones, el criterio del codo basado en el screeplot
# permitiría hacer el corte en dos componentes. Pero las componentes 3 a 5 acumulan el
# 21.5% de la varianza del dataset, con lo cual su aporte con fines predictivos puede
# ser considerable. Con todo lo dicho, elegiría 5 componentes para obtener una probable
# mejor performance predictiva en datos nuevos.

# En ambos escenarios, la regresión logística obtiene menor error empírico de 
# clasificación y por lo tanto es el modelo a elegir en este escenario. En cualquier caso,
# utilizaría train-test split, leave-on-out cross-valdation o k-fold corss validation de manera
# tal de poder estimar el EEC sobre datos no vistos y en base a ello comparar los modelos.
