
# Script recopilatorio de funciones y métodos de las fuentes

# =============================================================================
# --- Sección: Configuración y Carga de Datos (Basado en Fuentes 1, 2, 4, 5, 7) ---
# =============================================================================

# Limpiar el entorno de trabajo
rm(list=ls()) #

# Establecer directorio de trabajo (ejemplo, adaptar ruta)

# Cargar paquetes necesarios
# library(nombre_paquete)
library(tree) # Para modelos de árboles
library(ISLR2) # Para el conjunto de datos Boston
library(FNN) # Para kNN (regresión o clasificación)

# Establecer semilla para reproducibilidad (ejemplo)
set.seed(123) #

# Cargar conjuntos de datos desde diferentes formatos
# Cargar desde CSV con encabezado
datos_csv <- read.csv("nombre_archivo.csv", header=TRUE) #
# Cargar desde archivo de texto plano con encabezado, saltando líneas iniciales
datos_txt_skip <- read.table("nombre_archivo.txt", header=TRUE, skip=1) #
# Cargar desde archivo de texto plano con valores separados por espacio
datos_scan <- scan("nombre_archivo.txt") #

# Ejemplo de carga de datos de las fuentes (ejecutar solo si los archivos existen)
# alturadat50 <- read.csv("alturas_n_50.csv", header=T) #
# alturadat500 <- read.csv("alturas_n_500.csv", header=T) #
# hongos <- read.table("hongos_clasificados.txt", header=T) #
# grb <- read.table("GRB_afterglow.dat", header=T, skip=1) #
# buff <- scan("buffalo.txt") #
# aux_ninos <- read.csv("datos_sim_ninos.csv", header = T) #
# ninos <- aux_ninos$x #
# El conjunto Boston está en el paquete ISLR2

# =============================================================================
# --- Sección: Exploración y Resumen de Datos (Basado en Fuentes 1, 2, 7) ---
# =============================================================================

# Visualizar las primeras filas de un conjunto de datos
head(nombre_datos) #

# Abrir un visor de datos (útil para inspección interactiva)
View(nombre_datos) #

# Obtener los nombres de las columnas de un dataframe
names(nombre_dataframe) #

# Ver la estructura de un objeto (dataframe, lista, etc.)
str(nombre_objeto) #

# Obtener estadísticas resumen de las variables
summary(nombre_datos) #

# Obtener la clase (tipo) de una variable o columna
class(variable) #

# Calcular estadísticas básicas para una variable
mean(variable) #
sd(variable) #
IQR(variable) #
min(variable) # Implícito en summary
max(variable) # Implícito en summary
length(variable) #

# Calcular proporciones o contar elementos que cumplen una condición
sum(variable <= valor) #
mean(variable == valor) # Equivalente a proporción

# Ordenar un vector
sort(variable) #

# Excluir valores faltantes (NA) de un vector o dataframe
variable_completa <- na.exclude(variable_con_na) #
mean(variable, na.rm=TRUE) # Calcular mean ignorando NAs

# =============================================================================
# --- Sección: Visualización (Basado en Fuentes 1, 2, 4, 5, 7) ---
# =============================================================================

# Configurar layout de gráficos (filas, columnas)
par(mfrow=c(num_filas, num_columnas)) #
# Volver a layout por defecto
par(mfrow=c(1,1)) #

# Crear un histograma
hist(variable) # Histograma de frecuencias
hist(variable, freq=FALSE) # Histograma de densidad
hist(variable, breaks = numero_clases) # Especificar número de clases
hist(variable, breaks = vector_cortes) # Especificar los límites de las barras
hist(variable, main = "Título del Histograma") # Añadir título
hist(variable, xlim = c(limite_inf, limite_sup)) # Establecer límites en el eje x
hist(variable, ylim = c(limite_inf, limite_sup)) # Establecer límites en el eje y

# Crear un gráfico de dispersión (scatter plot)
plot(variable_x, variable_y) #
plot(variable_x, variable_y, type = "n") # Crear un gráfico vacío
plot(variable, yvariable, type="l") # Crear un gráfico de líneas
plot(hgrilla, cvhgrilla, type = "b") # Crear un gráfico con puntos y líneas
plot(funcion_ecdf) # Graficar la función ECDF
plot(objeto_density) # Graficar la función de densidad estimada
plot(objeto_ksmooth) # Graficar el resultado de ksmooth

# Añadir elementos a un gráfico existente
points(variable_x, variable_y, col = "color", pch = simbolo) # Añadir puntos
lines(variable_x, variable_y, col = "color") # Añadir líneas

# Crear un Boxplot
boxplot(variable) # Boxplot simple (vertical por defecto)
boxplot(variable, horizontal = TRUE) # Boxplot horizontal
boxplot(variable_numerica ~ variable_categorica) # Boxplots paralelos

# Añadir leyenda a un gráfico
legend("posición", legend = c("Etiqueta1", "Etiqueta2"), col = c("color1", "color2")) #

# =============================================================================
# --- Sección: Manipulación de Datos (Basado en Fuentes 2, 5) ---
# =============================================================================

# Seleccionar filas y columnas de un dataframe
df[indices_filas, indices_columnas]
df[df$columna == "valor", ] # Seleccionar filas por condición lógica
df[, c(col1, col2)] # Seleccionar columnas específicas
df[-indices_a_excluir, ] # Excluir filas
df[, -indices_a_excluir] # Excluir columnas

# Combinar columnas en un dataframe o matriz
cbind(columna1, columna2) #

# Crear una variable indicadora (dummy)
df$nueva_variable <- (df$variable_categorica == "valor") * 1 #

# Crear un dataframe manualmente
nuevo_dato <- data.frame(col1 = valor1, col2 = valor2, ...) #

# =============================================================================
# --- Sección: Funciones Personalizadas (Basado en Fuentes 2, 4, 5, 6) ---
# =============================================================================

# Definición de funciones personalizadas
#pred_prom_loc: Predicción por promedio local (vecindad simétrica)
pred_prom_loc <- function(x, y, x_nueva, h)
{
  mean(y[x>=(x_nueva-h) & x<=(x_nueva+h)], na.rm = TRUE) # na.rm=TRUE añadido para robustez
} #

# prop_vec: Estima proporciones de clases entre k vecinos más cercanos
prop_vec <- function(x, y, x_nuevo, k)
{
  vecinos <- order(abs(x - x_nuevo))[1:k] # Encontrar indices de los k vecinos más cercanos
  var1 <- mean(y[vecinos] == 0) # Proporción de clase 0
  var2 <- mean(y[vecinos] == 1) # Proporción de clase 1
  c(var1,var2) # Devuelve vector con las proporciones
} #

# prop_loc: Estima proporciones de clases en una ventana local
prop_loc <- function(x, y, x_nuevo, h)
{
  ind <- (x >= (x_nuevo - h) & x <= (x_nuevo + h)) * 1 # Identificar puntos dentro de la ventana
  entorno <- y[ind == 1] # Seleccionar los valores y correspondientes a la ventana
  var1 <- mean(entorno == 0, na.rm = TRUE) # Proporción de clase 0 (añadido na.rm)
  var2 <- mean(entorno == 1, na.rm = TRUE) # Proporción de clase 1 (añadido na.rm)
  c(var1, var2, sum(ind)) # Devuelve proporciones y tamaño del entorno
} #

# densidad_est_parzen: Estima densidad con ventana rectangular (Parzen)
# No es una implementación completa de kernel density, es una versión simplificada
# basada en contar puntos en una ventana. R tiene la función density() que es mejor.
# densidad_est_parzen <- function(x, x_0, h) { ... } #

# clas_gen: Clasificación basada en densidades estimadas para cada clase y priors
# Requiere funciones de densidad (o estimaciones) y proporciones previas (priors)
# Note: Esta función asume que tiene acceso a las densidades 'dens0' y 'dens1'
# y a las proporciones 'propm0' y 'propm1' calculadas previamente.
clas_gen <- function(x, y, x_nuevo, h0, h1)
{
  # Supongamos que x es la variable (e.g., Height), y es la indicadora (0 o 1)
  # x_nuevo es el valor a clasificar
  # h0 y h1 son anchos de banda para la estimación de densidad de cada clase
  
  # Estimación de densidad para clase 0 en x_nuevo
  # density() con from=to=n=1 es una forma de evaluar la densidad en un punto específico
  dens_clase0 <- density(x[y == 0], kernel = "gaussian", bw = h0,
                         from = x_nuevo, to = x_nuevo, n = 1)$y #
  
  # Estimación de densidad para clase 1 en x_nuevo
  dens_clase1 <- density(x[y == 1], kernel = "gaussian", bw = h1,
                         from = x_nuevo, to = x_nuevo, n = 1)$y #
  
  # Estimación de proporciones previas (priors) de las clases
  proporcion_clase0 <- mean(y == 0, na.rm = TRUE) #
  proporcion_clase1 <- mean(y == 1, na.rm = TRUE) #
  
  # Aplicar la regla de clasificación: comparar (densidad * prior) para cada clase
  if ((dens_clase0 * proporcion_clase0) > (dens_clase1 * proporcion_clase1)) { #
    return(0) # Clasificar como clase 0 (Variedad 1 en el ejemplo de hongos)
  } else {
    return(1) # Clasificar como clase 1 (Variedad 2 en el ejemplo de hongos)
  }
} #

# loocv: Calcula el error Leave-One-Out Cross-Validation
# Versión genérica para cualquier función de predicción 'pred_func'
# y métrica de pérdida 'loss_func'. En las fuentes se usa para promedio local y clasificación.
# loocv_generico <- function(x, y, pred_func, loss_func, ...) { ... } # Concepto basado en

# Versión específica para el predictor pred_prom_loc (Error Cuadrático Medio)
loocv_pred_prom_loc <- function(x,y,h)
{
  perd <- c()
  n <- length(y)
  for(i in 1:n)
  {
    # Calcula la predicción para el punto i usando todos los demás datos (x[-i], y[-i])
    pred_i <- pred_prom_loc(x[-i], y[-i], x[i], h) # Usar el predictor local sin el punto i
    # Calcula la pérdida (error cuadrático) para el punto i
    perd[i] <- (y[i] - pred_i)^2 #
  }
  return(mean(perd,na.rm=TRUE)) # Devuelve el promedio de las pérdidas, ignorando NAs
} #

# Versión específica para la clasificación clas_gen (Error de Clasificación)
loocv_clas_gen <- function(h0, h1) # h0 y h1 son los parámetros de la función clas_gen
{
  # Asume que 'altura' e 'indicadora' son variables globales o pasadas como argumentos
  # En la fuente, 'altura' e 'indicadora' son globales para esta función.
  # Si se quisiera hacer más genérica, se deberían pasar x e y como argumentos.
  
  n <- length(indicadora) #
  cvaux <- c() # Vector para almacenar las predicciones LOOCV
  
  for(i in 1:n)
  {
    # Clasifica el punto i usando todos los demás datos (altura[-i], indicadora[-i])
    # y los parámetros h0 y h1
    cvaux[i] <- clas_gen(altura[-i], indicadora[-i], altura[i], h0, h1) #
  }
  
  # Calcula el Error Cuadrático Medio entre las clases reales (0/1) y las predicciones (0/1)
  # Esto es equivalente a la tasa de error de clasificación en este caso binario (0/1)
  return(mean((indicadora - cvaux)^2)) #
} #

# =============================================================================
# --- Sección: Estimación No Paramétrica (Basado en Fuentes 2, 3, 4, 5) ---
# =============================================================================

# Estimación de densidad por Kernel
# density() es la función de R para Kernel Density Estimation (KDE)
# kernel: Tipo de kernel ("gaussian" por defecto, "rectangular", etc.)
# bw: Ancho de banda (bandwidth). Su selección es crucial.
# from, to, n: Rangos y número de puntos para la estimación (útil para evaluar en puntos específicos)

# Ejemplo de uso básico
estimacion_densidad <- density(datos, kernel="gaussian") #
plot(estimacion_densidad) # Graficar la densidad estimada

# Evaluar la densidad estimada en un punto específico (ejemplo)
aproxfun(estimacion_densidad)(punto_evaluacion) #
# O alternativamente, usando from/to/n en density
density(datos, kernel = "rectangular", bw = 10/sqrt(3), from = 80, to = 80, n = 1)$y #

# Estimación automática del ancho de banda
# bw.ucv(): Ancho de banda usando Unbiased Cross-Validation
# Otros métodos existen, algunos son usados por density() por defecto
ancho_banda_optimo <- bw.ucv(datos) #

# Kernel Smoothing (Regresión no paramétrica)
# ksmooth() calcula una regresión no paramétrica usando kernels (rectangular o normal)
# x, y: Datos de las variables predictora y respuesta
# kernel: Tipo de kernel ("box" o "normal")
# bandwidth: Ancho de banda
# x.points: Puntos donde evaluar la estimación (por defecto, se evalúa en los puntos x)

# Ejemplo de uso
ksmooth_result <- ksmooth(variable_x, variable_y, kernel="normal", bandwidth = 2) #
lines(ksmooth_result, col="blue") # Añadir la curva suavizada al gráfico

# =============================================================================
# --- Sección: k-Nearest Neighbors (kNN) Regresión (Basado en Fuente 3) ---
# =============================================================================

# knn.reg(): Implementación de kNN para regresión (del paquete FNN)
# train: Matriz o dataframe con las variables predictoras del conjunto de entrenamiento
# test: Matriz o dataframe con las variables predictoras del conjunto de test (puede ser un solo punto)
# y: Vector con la variable respuesta del conjunto de entrenamiento
# k: Número de vecinos a considerar
# algorithm: Algoritmo para encontrar vecinos (ej: "brute")

# Ejemplo de uso
librar(FNN) # Cargar paquete
pred_knn <- knn.reg(train = datos_entrenamiento_x, test = nuevo_punto_x,
                     y = datos_entrenamiento_y, k = 7, algorithm = "brute") #
pred_knn$pred # Obtener la predicción

# =============================================================================
# --- Sección: Evaluación de Modelos y Selección de Hiperparámetros (Basado en Fuentes 3, 6) ---
# =============================================================================

# Calcular Mean Squared Error (MSE)
# MSE = mean((valores_reales - valores_predichos)^2) #

# Calcular Sum of Squared Errors (SSE) o Residual Sum of Squares (RSS)
# SSE/RSS = sum((valores_reales - valores_predichos)^2) #

# Leave-One-Out Cross-Validation (LOOCV)
# Implementación manual de la función loocv_pred_prom_loc se encuentra arriba.
# Su uso implica iterar sobre cada punto, predecir el punto i excluyéndolo del entrenamiento,
# y calcular la pérdida para ese punto. El error LOOCV es el promedio de estas pérdidas.
# error_loocv <- loocv_pred_prom_loc(variable_x, variable_y, ancho_banda_h) #

# Búsqueda en grilla (Grid Search) para seleccionar hiperparámetros
# Definir una grilla de valores para el/los hiperparámetro(s)
# grilla_h <- seq(inicio, fin, by = paso) #
# Crear una estructura (vector o matriz) para almacenar los resultados de la evaluación
# resultados_cv <- c() # Vector
# resultados_cv_matriz <- matrix(nrow = n_h0, ncol = n_h1) # Matriz para 2 parámetros
# Iterar sobre los valores de la grilla, entrenar/evaluar el modelo y guardar el resultado
# for(h in grilla_h) {
#   resultados_cv[i] <- funcion_evaluacion(..., h)
# } #

# Encontrar el/los hiperparámetro(s) que minimizan el error de evaluación
# min(resultados_cv) # Valor mínimo del error
# which(resultados_cv == min(resultados_cv), arr.ind = TRUE) # Indices del mínimo en matriz
# h_optimo <- grilla_h[order(resultados_cv)] # Valor del hiperparámetro óptimo

# =============================================================================
# --- Sección: Modelos de Árboles (Basado en Fuente 7) ---
# =============================================================================

# Cargar paquete 'tree'
# library(tree) #
# El paquete ISLR2 es útil por sus conjuntos de datos, incluyendo Boston.
# library(ISLR2) #

# Dividir datos en conjuntos de entrenamiento y test (ejemplo 50/50)
# n_total <- nrow(dataframe) #
# indices_entrenamiento <- sample(1:n_total, n_total / 2) #
# datos_entrenamiento <- dataframe[indices_entrenamiento, ]
# datos_test <- dataframe[-indices_entrenamiento, ] #

# Entrenar un modelo de árbol (regresión o clasificación)
# tree(): Función principal para ajustar árboles
# formula: Especifica la relación entre variables (ej: respuesta ~ predictor1 + predictor2)
#          respuesta ~ . usa todas las demás variables como predictoras
# data: El dataframe que contiene los datos
# subset: Usar solo un subconjunto de los datos (e.g., conjunto de entrenamiento)
# mincut: Mínimo número de observaciones en cualquier nodo antes de intentar dividir (defecto=5)
# mindev: Mínima reducción de la desviación (error) requerida para hacer una división (defecto=0.01)

# Ejemplo de ajuste de un árbol de regresión
# arbol_regresion <- tree(medv ~ ., data = Boston, subset = indices_entrenamiento) #

# Ver un resumen del árbol ajustado
# summary(arbol_regresion) # Muestra variables usadas, nodos terminales, error en entrenamiento

# Obtener el RSS (Residual Sum of Squares) del árbol en entrenamiento
# rss <- summary(arbol_regresion)$dev #

# Visualizar el árbol
# plot(arbol_regresion) # Dibuja la estructura del árbol
# text(arbol_regresion, cex = 0.7) # Añade etiquetas a los nodos (reglas de división, predicciones)

# Hacer predicciones con un árbol ajustado
# predict(arbol_ajustado, newdata = nuevos_datos) #
# Para predecir en los datos de entrenamiento: predict(arbol_ajustado, newdata = datos_entrenamiento) #
# Para predecir en los datos de test: predict(arbol_ajustado, newdata = datos_test) #

# Evaluar el rendimiento del árbol
# MSE en test: mean((datos_test$respuesta - predicciones_test)^2) #
# MSE en entrenamiento: mean((datos_entrenamiento$respuesta - predicciones_entrenamiento)^2) #

# Podar el árbol usando Cross-Validation
# cv.tree(): Realiza cross-validation para determinar el mejor tamaño de árbol
# arbol_ajustado: El árbol a podar
# K: Número de folds para CV (ej: K=10)
# cv_resultado <- cv.tree(arbol_regresion, K = 10) #

# Ver resultados de CV (tamaño del árbol, desviación/error, parámetro de complejidad k)
# cv_resultado #
# plot(cv_resultado$size, cv_resultado$dev, type = "b") # Graficar error vs tamaño del árbol
# El tamaño con menor error es el "mejor"

# Podar el árbol al tamaño óptimo
# prune.tree(): Poda un árbol
# arbol_ajustado: El árbol a podar
# best: Número de nodos terminales deseado para el árbol podado
# arbol_podado <- prune.tree(arbol_regresion, best = tamaño_optimo) #
# plot(arbol_podado) # Visualizar árbol podado
# text(arbol_podado, cex=0.7)

# Fin del script recopilatorio