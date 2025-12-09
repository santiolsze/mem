################################################################################
################################################################################
################################# CLASE 1 ######################################
################################################################################
################################################################################
library(FNN)

set.seed(1)
n_train = 50 
n_test = 500
sigma = 1

f = function(x){10*x*(x-1)*(x+1)}

x_train = data.frame( x = runif(n_train, -1, 1) )
y_train = f(x_train$x) +rnorm(n_train, sd = sigma) 

x_test = data.frame( x = runif(n_test, -1, 1) )
y_test = f(x_test$x) + rnorm(n_test, sd = sigma)



xx = seq(-1, 1, 0.05)



plot(x_train$x, y_train, xlab = 'x', ylab = 'y', main = 'Ajuste por vecinos cercanos')
lines(xx, f(xx), lwd = 1.5, lty = 2)





K = 5
y_hat_knn5 = knn.reg( x_train , test =data.frame(x = xx)  , y = y_train, k = K)$pred
K = 10
y_hat_knn10 = knn.reg(x_train, test =data.frame(x = xx)  , y = y_train, k = K)$pred

K = 2
y_hat_knn2 = knn.reg(x_train, test =data.frame(x = xx)  , y = y_train, k = K)$pred


lines(xx, y_hat_knn5, col ='orange', lwd = 2,)
lines(xx, y_hat_knn10, col ='tomato', lwd = 2)
lines(xx, y_hat_knn2, col ='blue', lwd = 2)

legend("topright", 
       legend = c("K = 2", "K = 5", "K = 10",  "E (Y | X = x )" ),
       col = c("blue", "orange", "tomato", 'black'),
       lty = c(1, 1, 1, 2)
)  # sin




Ks = 1:20
k_test_mse = rep(0, length(Ks))
k_train_mse = rep( 0, length(Ks))




for (j in 1:20) {
  k_test_mse[j] = mean( (y_test -  knn.reg(x_train, test =x_test, 
                                           y = y_train, k = Ks[j])$pred)^2 ) 
  k_train_mse[j] = mean((y_train -  knn.reg(x_train, test =x_train, 
                                            y = y_train, k = Ks[j])$pred)^2 )
  
}

plot( Ks, k_test_mse, type = 'l', ylim =c(0, max(k_test_mse)), col = 'darkviolet', ylab = 'MSE', 
      main = 'Curvas de MSE - Ajuste de vecinos cercanos', xlab = 'K \n Cantidad de vecinos')
lines(Ks, k_train_mse, col = 'tomato')

k_min = Ks[ which.min(k_test_mse)]
val_min = k_test_mse[which.min(k_test_mse)]

points( k_min, val_min, col ='darkviolet')
lines( c(k_min, k_min), c(0,val_min) , lty = 2, lw = 0.5, col ='darkviolet')
lines( c(0, k_min), c(val_min, val_min) , lty = 2, lw = 0.5, col ='darkviolet')

legend("bottomright", 
       legend = c("Test MSE", "Train MSE", "Mínimo Test MSE"),
       col = c("darkviolet", "tomato", "darkviolet"),
       lty = c(1, 1, NA),
       pch = c(NA, NA, 1),
)  # sin
## Que sucede con la curva del MSE si aumenta la varianza de los errores? Piense primero y despues verifique.

###############################################################
# Comentario teórico (y script de verificación) sobre el
# ejemplo de KNN-regression con distintos K.
#
# Requisitos: instalado paquete FNN (ya usado en el ejemplo).
#
# El bloque combina:
#  - interpretación teórica de las curvas de MSE (train / test)
#  - una pequeña rutina para verificar qué pasa cuando
#    aumentamos la varianza del ruido (sigma).
###############################################################

# ---------------------------
# 1) Conclusiones teóricas
# ---------------------------
# • Descomposición del error esperado (en test):
#     E[(Y - \hat{f}(X))^2] = Bias^2( \hat{f}(x) ) + Var( \hat{f}(x) ) + Var(ε)
#   donde Var(ε) es la varianza irreducible (sigma^2).
#
# • ¿Qué hace K en KNN?
#     - K pequeño -> baja bias (mejor adaptación local) pero alta varianza
#       (predicciones muy sensibles a ruidos en puntos vecinos).
#     - K grande -> mayor bias (suaviza la función) y menor varianza.
#   Por eso la curva test-MSE en K suele mostrar una U: hay un K óptimo
#   que equilibra bias y varianza.
#
# • ¿Qué sucede cuando aumentamos la varianza del error (sigma^2)?
#   Predicción teórica:
#     1. El término irreducible Var(ε)=sigma^2 aumenta, por lo que
#        tanto el test-MSE promedio como (en menor medida) el train-MSE
#        se desplazan hacia arriba aproximadamente en sigma^2.
#     2. El componente de Var( \hat{f} ) relativo al muestreo permanece
#        aproximadamente igual (depende de n y K, no de sigma).
#     3. Dado que la proporción del error debida al ruido aumenta,
#        la diferencia entre modelos (distintos K) se vuelve menos
#        relevante en términos absolutos: la curva de test-MSE se "aplasta"
#        verticalmente respecto a la distancia entre valores de K.
#     4. Importante: al aumentar sigma, suele ser conveniente usar
#        K mayores (promediar más vecinos) porque eso reduce la
#        varianza de la predicción y mitiga parte del efecto del ruido.
#        En la práctica, el K óptimo tiende a aumentar con sigma.
#
# Resumen intuitivo:
#   - Test-MSE y train-MSE suben con mayor sigma^2.
#   - El mínimo de la curva test-MSE puede moverse hacia la derecha
#     (mayor K óptimo) cuando sigma aumenta.
#   - Las diferencias relativas entre K se hacen menos pronunciadas
#     si el ruido domina la señal.


# ---------------------------
# 3) Interpretación práctica de los resultados esperados
# ---------------------------
# • Verá que todas las curvas están desplazadas hacia arriba a medida que
#   sigma crece (MSE mayor porque Var(ε) mayor).
# • El valor mínimo del Test-MSE (en escala absoluta) aumenta con sigma.
# • Es probable que k_opt_per_sigma (los K óptimos) tienda a crecer para
#   sigmas grandes, confirmando la recomendación de promediar más vecinos
#   cuando el ruido es dominante.
#
# Nota: los detalles numéricos (qué tanto aumenta el K óptimo) dependen
# de n_train, de la forma de f, y de la semilla (muestreo de x). Con n_train
# pequeño la variabilidad en la estimación del K óptimo será mayor.
###############################################################

################################################################################
################################################################################
################################# CLASE 2 ######################################
################################################################################
################################################################################

library(FNN)
library(ISLR2)
library(tidyverse)
library(patchwork)

head(Default)

summary(Default)

ggplot(Default, aes(x = balance, y = income, color = default, pch =student)) +
  geom_point() + ggtitle('Default data set')

ggplot(Default, aes(x = default , y = balance)) +
  geom_boxplot() +ggtitle('Distribucion del balance por default')

ggplot(Default, aes(x = default , y = balance)) +
  geom_boxplot() +ggtitle('Distribucion del balance por default y estudiante') + facet_wrap( ~ student)


#### Ajuste de modelos
## Armamos un dataframe con los valores donde queremos evaluar el clasificador
vals  = crossing(balance = seq(min(Default$balance), max(Default$balance), length.out = 51), 
                 income = seq(min(Default$income), max(Default$income), length.out = 51))



vals['knn10'] = knn(train = Default %>% select(balance, income), 
                    test = vals %>% select(balance, income) , 
                    cl = Default %>% pull(default), k = 10, prob = TRUE)

vals['knn10_prob'] =1 - attributes( knn(train = Default %>% select(balance, income), 
                                        test = vals %>% select(balance, income) , 
                                        cl = Default %>% pull(default), k = 10, prob = TRUE) )$prob

vals['knn3'] = knn(train = Default %>% select(balance, income), 
                   test = vals %>% select(balance, income) , 
                   cl = Default %>% pull(default), k = 3, prob = TRUE)

vals['knn3_prob'] =1- attributes( knn(train = Default %>% select(balance, income), 
                                      test = vals %>% select(balance, income) , 
                                      cl = Default %>% pull(default), k = 3, prob = TRUE))$prob

fit1 = glm(default ~ balance + income , data = Default, family = binomial)

vals['proba'] = predict( fit1 , newdata =  vals %>% select(balance, income), type = 'response')
vals['logistic'] = ifelse( predict( fit1 , newdata =  vals %>% select(balance, income), type = 'response') > 0.5, 'Yes', 'No')


p1 = ggplot(vals, aes( x = balance, y = income , color = knn3)) + geom_point(shape = 3) +
  ggtitle('Regiones de clasificacion K = 3')

p2 = ggplot(vals, aes( x = balance, y = income , color = knn10)) + geom_point(shape = 3) +
  ggtitle('Regiones de clasificacion K = 10')



p1 + p2

p1 = ggplot(vals, aes( x = balance, y = income , color = knn3)) + geom_point(shape = 3) + 
  geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Regiones de clasificacion K = 3')

p2 = ggplot(vals, aes( x = balance, y = income , color = knn10)) + geom_point(shape = 3) +
  geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Regiones de clasificacion K = 10')



p1 + p2

p3 = ggplot(vals, aes( x = balance, y = income , color = logistic)) + geom_point(shape = 3) +
  #geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Probabilida de default')

p4 = ggplot(vals, aes( x = balance, y = income , color = logistic)) + geom_point(shape = 3) +
  geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Region de clasificacion, threshold: 0.5')

p3+p4




ggplot(vals, aes( x = balance, y = income , color = proba)) + geom_point(shape = 3) +
  #geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Logistica') +  scale_color_gradient(limits = c(0, 1))+   theme(legend.position = "none")+
  
  ggplot(vals, aes( x = balance, y = income , color = knn10_prob)) + geom_point(shape = 3) +
  #geom_point(data = Default, aes(x = balance, y = income, color = default) ) +
  ggtitle('Knn 10') +  scale_color_gradient(limits = c(0, 1)) +   theme(legend.position = "none")+
  
  ggplot(vals, aes( x = balance, y = income , color = knn3_prob)) + geom_point(shape = 3) +
  ggtitle('Knn 3')  +  scale_color_gradient(limits = c(0, 1)) +labs(color ="Proba\ndefault")










fit1 = glm(default ~ balance + income , data = Default, family = binomial)
fit2 = glm(default ~ balance + income + student, data = Default, family = binomial)

fit3 = glm(default ~ balance + income*student, data = Default, family = binomial)

summary(fit1)
summary(fit2)
summary(fit3)







vals['probs'] = predict(fit1, newdata = vals, type = 'response')

vals = vals %>% mutate( class05 = ifelse( probs > 0.5, 'Yes', 'No' ), class09 = ifelse( probs > 0.9, 'Yes', 'No' ), class01 =ifelse( probs > 0.1, 'Yes', 'No' ) )


### Ejercicio en clase
library(ISLR2)
library(tidyverse)
library(patchwork)

head(Bikeshare)

## No hay observaciones para todos los dias y todas las horas... pero casi
Bikeshare  %>% summarize( n_distinct(mnth, day, hr) )/ (365*24)

## Faltan de manera aleatoria?

Bikeshare %>% count(day) %>% ggplot(aes( x = n)) +geom_bar()  +  ggtitle('Graficos de barra por cantidad de dias con n observaciones')

##

data_transformada = Bikeshare %>% group_by(mnth) %>% 
  summarise( avg_bikers = mean( bikers),
             hour_sup_sd = mean( bikers) + sd(bikers),
             hour_inf_sd = mean( bikers) - sd(bikers)) 


p1 = ggplot(data_transformada, aes(x = mnth, y = avg_bikers, group = 1 )) + geom_line() + geom_point() + 
  geom_line(aes(x = mnth, y =hour_sup_sd), lty = 3) + 
  geom_line(aes(x = mnth, y =hour_inf_sd), lty = 3) + 
  ggtitle('Promedio de bikers +- 1 desvio')


data_transformada_2 = Bikeshare %>% group_by(hr) %>% 
  summarise( avg_bikers = mean( bikers),
             hour_sup_sd = mean( bikers) + sd(bikers),
             hour_inf_sd = mean( bikers) - sd(bikers),
  ) 

p2 = ggplot(data_transformada_2, aes(x = hr, y = avg_bikers, group = 1 )) + geom_line() + geom_point() + 
  geom_line(aes(x = hr, y =hour_sup_sd), lty = 3) + geom_line(aes(x = hr, y =hour_inf_sd), lty = 3) + ggtitle('Promedio de bikers +- 1 desvio') 


p1 + p2

### graficos alternativos

ggplot(Bikeshare, aes(y = bikers, x = mnth)) + geom_boxplot() + ggtitle('Boxplot de bikers por mes') +  
  ggplot(Bikeshare, aes(y = bikers, x = hr)) + geom_boxplot() + ggtitle('Boxplot de bikers por hora')


ggplot(Bikeshare %>% filter(hr == 17), aes(y = bikers, x = as.factor(workingday) )) + geom_boxplot() 



#### Solo para mostrar por que ggplot es tan lindo

data_transformada_3 = Bikeshare %>% mutate(season = as.factor(season), workingday = ifelse(workingday, 'Dias laborables', 'Dias no laborables')) %>% 
  group_by(hr, mnth, workingday) %>% 
  summarise( avg_bikers = mean( bikers),
             sd_bikers = sd(bikers),
  ) 

p4 = ggplot(data_transformada_3, aes(x = hr, y = avg_bikers, group = mnth, color = mnth )) + 
  geom_line() + ggtitle('Promedio de bikers por hora en distintos meses') + facet_wrap( ~ workingday)


p4


p5 = ggplot(Bikeshare, aes(x = weathersit, y = bikers )) + geom_boxplot() + ggtitle('Bikers por clima')
p5


#### Ajustar una regresion de poisson en el proble de los ciclistas

mod.pois <- glm( bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare, family = poisson)
mod.lm = lm( bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare)

par(mfrow = c(1,1))

plot(mod.lm)


summary(mod.pois)


coef.mnth = c(0, coef(mod.pois)[2:12])


par(mfrow = c(1,2))
plot(1:12, coef.mnth, xlab = 'Mes', ylab = 'Coeficiente', xaxt = 'n', col = 'blue', pch = 19, type = 'o', main = 'Coeficiente estimado por mes')
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

coef.hours <- c(0, coef(mod.pois)[13:35])
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19, type = "o", main ='Coeficientes estimado por hora del dia')

par(mfrow = c(1,1))

with(mod.pois, plot(fitted.values, Bikeshare$bikers - fitted.values, main = 'Fitted vs residuals' ) ) 


################################################################################
#                     COMENTARIOS TEÓRICOS — CLASIFICACIÓN
#                     Ejemplo: Default (ISLR2)
################################################################################

#-------------------------------------------------------------------------------
# 1) Espacio (balance, income) y fronteras de clasificación
#-------------------------------------------------------------------------------
# El dataset Default tiene dos predictores continuos (balance, income).
# Cuando generamos una grilla ‘vals’ y evaluamos un clasificador sobre ella,
# lo que estamos haciendo es visualizar la *región de decisión* en R^2.
#
# Un clasificador binario define una partición del plano:
#   {x : \hat{P}(default="Yes"|x) > 0.5}   vs   {x : ≤ 0.5}
#
# Según el modelo, esta frontera puede ser:
#   - Lineal (regresión logística)
#   - No lineal y muy irregular (KNN con K pequeño)
#   - Más suave (KNN con K grande)
#
# Estas figuras grafican exactamente eso: cada punto en la grilla se pinta
# según la clase predicha, mostrando la geometría del modelo.

#-------------------------------------------------------------------------------
# 2) Diferencias teóricas entre modelos
#-------------------------------------------------------------------------------
# *Regresión Logística*
#   - Modela logit(P(Y=1|X)) como función lineal de los predictores.
#   - Frontera de decisión = línea (+ posible curva en transformaciones).
#   - Suaviza mucho: alta interpretabilidad, baja varianza, posible alto bias.
#   - Produce probabilidades coherentes y calibradas en (0,1).
#
# *KNN Clasificación*
#   - No impone forma funcional. Aproxima P(Y=1|x) por mayoría de K vecinos.
#   - K pequeño → frontera muy fragmentada, curva, posiblemente irregular.
#                  baja bias, alta varianza
#   - K grande  → superficie más suave, más parecida a logística.
#                  mayor bias, menor varianza
#   - Las “probabilidades” son proporciones locales (no siempre bien calibradas).

#-------------------------------------------------------------------------------
# 3) Sobre el prob = TRUE en KNN
#-------------------------------------------------------------------------------
# Cuando usamos prob = TRUE, FNN::knn devuelve:
#   - La clase predicha
#   - attr(..., "prob") = proporción de votos del ganador.
#
# OJO: no es "P(default=Yes|x)" estricta. Es "confianza" del vecino mayoritario.
#
# En los gráficos de proba (gradient):
#   - La logística da un gradiente suave continuo.
#   - KNN produce bloques (celdas) 2D más abruptos, especialmente con K chico.

#-------------------------------------------------------------------------------
# 4) Comparación de modelos logísticos con efectos adicionales
#-------------------------------------------------------------------------------
# fit1: default ~ balance + income
# fit2: agrega ‘student’
# fit3: agrega interacción income*student
#
# Teoría:
#   - Si ‘student’ mejora el ajuste, su coeficiente será significativo.
#   - La interacción permite que el efecto de income cambie según student.
#   - Si la línea de decisión es distinta para estudiantes y no-estudiantes,
#     recién fit3 lo captura.
#
# Este es un ejemplo clásico de:
#   - Modelo lineal: frontera una recta o familia de rectas.
#   - Capacidad del modelo depende de incluir transformaciones / interacciones.

################################################################################
#                COMENTARIOS TEÓRICOS — ANÁLISIS DE BIKESHARE
################################################################################

#-------------------------------------------------------------------------------
# 5) Exploración de datos horarios y mensuales
#-------------------------------------------------------------------------------
# Los gráficos muestran:
#   - Fuerte patrón horario: picos en commuting (mañana / tarde).
#   - Estacionalidad mensual marcada.
#   - Diferencias entre días laborables y no laborables.
#   - Efecto del clima sobre la cantidad de ciclistas.
#
# Boxplots por hr y por mnth:  
#   → La dispersión varía fuertemente según la hora.  
#   → Justifica modelos con estructura flexible.

#-------------------------------------------------------------------------------
# 6) Regresión de Poisson vs regresión lineal
#-------------------------------------------------------------------------------
# Modelo Poisson: bikers ~ predictores
#   - Adecuado para conteos.
#   - Varianza ≈ media (salvo sobredispersión).
#   - Link log crea efectos multiplicativos:
#        log(E[bikers]) = β0 + β1*mnth + ...
#
# Interpretación de coeficientes:
#   - exp(βj) = factor multiplicativo en el número esperado de ciclistas
#     por unidad de cambio en la covariable.
#
# Gráficos de coeficientes por mes y por hora:
#   - Muestran el perfil diurno y la estacionalidad.
#
# Fitted vs residuals:
#   - En Poisson rara vez se ven residuos homocedásticos, pero buscamos patrones
#     sistemáticos que indiquen mala especificación.

################################################################################
#              RESUMEN DE IDEAS PRINCIPALES (para clase / práctico)
################################################################################
# 1) KNN:
#    - K pequeño = frontera irregular, alta varianza.
#    - K grande  = frontera suave, alta bias.
#    - Aproxima P(Y=1|x) localmente; no calibrado.
#
# 2) Logística:
#    - Modelo paramétrico lineal en log-odds.
#    - Frontera suave y simple, muy interpretable.
#    - Probabilidades calibradas.
#
# 3) Interacciones:
#    - Permiten que distintos grupos tengan pendientes distintas.
#
# 4) Poisson (Bikeshare):
#    - Adecuado para conteos con link log.
#    - Coeficientes = multiplicadores.
#    - Los gráficos muestran ciclos diarios y estacionales muy fuertes.
################################################################################


################################################################################
################################################################################
################################# CLASE 3 ######################################
################################################################################
################################################################################

library(ISLR2)

# Mostrar los primeros datos
head(Auto)

# Gráfico básico: Millas por galón vs Caballos de fuerza
plot(Auto$horsepower, Auto$mpg, 
     xlab = "Caballos de fuerza", 
     ylab = "Millas por galón",
     main = "Millas por galón vs Caballos de fuerza",
     pch = 16, col = "blue")

#### Partir la muestra en 2 grupos, uno para ajustar y otro para validar
set.seed(123)
train_split = sample(1:nrow(Auto)) %% 2

# Gráfico mostrando la separación en entrenamiento y validación
plot(Auto$horsepower, Auto$mpg, 
     col = ifelse(train_split == 0, "red", "blue"),
     pch = 16,
     xlab = "Caballos de fuerza", 
     ylab = "Millas por galón",
     main = "Separación en Validación y Entrenamiento")
legend("topright", 
       legend = c("Entrenamiento", "Validación"),
       col = c("red", "blue"), pch = 16)

# Separar los datos
train = Auto[train_split == 0, ]
validation = Auto[train_split == 1, ]

# Calcular errores para diferentes grados de polinomio
train_error = rep(NA, 10)
test_error = rep(NA, 10)

for (p in 1:10) {
  fit = lm(mpg ~ poly(horsepower, degree = p), data = train)
  test_error[p] = mean((validation$mpg - predict(fit, newdata = validation))^2)
  train_error[p] = mean(fit$residuals^2)
}

# Gráfico de errores
plot(1:10, test_error, type = "l", col = "blue",
     xlab = "Grado del polinomio mpg ~ 1 + hp + ... + hp^p",
     ylab = "Error Cuadrático Medio",
     main = "Error de testeo",
     ylim = range(c(train_error, test_error)))
lines(1:10, train_error, col = "orange", lty = 2)
legend("topright", 
       legend = c("Error Validación", "Error Entrenamiento"),
       col = c("blue", "orange"), lty = 1:2)

#### Distintas particiones de entrenamiento y validación
par(mfrow = c(2, 2))  # 2x2 grid de gráficos

for (i in 1:4) {
  random_split = sample(1:nrow(Auto)) %% 2
  plot(Auto$horsepower, Auto$mpg, 
       col = ifelse(random_split == 0, "red", "blue"),
       pch = 16,
       xlab = "Caballos de fuerza", 
       ylab = ifelse(i %in% c(1, 2), "Millas por galón", ""),
       main = paste("Partición", i))
}

# Restaurar configuración de gráficos
par(mfrow = c(1, 1))
title("Distintas particiones en validación y entrenamiento", outer = TRUE, line = -1)

#### Validación cruzada K = 4
K = 4
data = Auto
data$CC = sample(1:nrow(Auto)) %% K + 1

# Gráfico 1: Partición en grupos
plot(data$horsepower, data$mpg, 
     col = data$CC, pch = 16,
     xlab = "Caballos de fuerza", ylab = "Millas por galón",
     main = "Validación cruzada K = 4 - Partición en grupos")
legend("topright", legend = paste("Grupo", 1:K), 
       col = 1:K, pch = 16)

# Gráficos individuales para cada fold
par(mfrow = c(2, 2))

for (k in 1:K) {
  plot(data$horsepower, data$mpg, 
       col = ifelse(data$CC == k, "red", "blue"),
       pch = 16,
       xlab = "Caballos de fuerza", 
       ylab = "Millas por galón",
       main = paste("Fold k =", k))
  legend("topright", 
         legend = c("Validación", "Entrenamiento"),
         col = c("red", "blue"), pch = 16)
}

par(mfrow = c(1, 1))

#### Ejercicio alta correlación (igual que antes)
X = matrix(rnorm(1000*50), nrow = 50)
p = 1/(1+exp(-rowSums(2*X[, 1:7]))) #logit fun
Y = rbinom(50, 1, p)

cors_val = abs(cor(X, Y))
hist(cors_val, main = "Histograma de correlaciones", xlab = "Correlación")
k = 5

(max_cor_indx = which(cors_val > sort(cors_val, decreasing = TRUE)[k]))
sel_X = X[, max_cor_indx]

#### Stepwise (igual que antes)
library(MASS)
data(Boston)

M0 = lm(crim ~ 1, data = Boston)

Mfull = lm(crim ~ ., data = Boston)
Ffull = formula(Mfull)

# Forward selection
step(M0, data = Boston, direction = 'forward')

# Backward selection
backward_model = step(Mfull, scope = Ffull, direction = 'backward')

# Both directions
step(M0, scope = Mfull, direction = 'both')














#######################################################################
# Comentario teórico sobre todo el bloque de código:
#   – Validación simple entrenamiento/validación
#   – Selección del grado de polinomio
#   – Variabilidad de las particiones
#   – Validación cruzada K-fold
#   – Problema de correlaciones espurias
#   – Stepwise selection (forward, backward, both)
#
# Este chunk NO describe el funcionamiento del código, sino los 
# fundamentos teóricos del análisis que se está mostrando.
#######################################################################

# ─────────────────────────────────────────────────────────────
# 1. Relación no lineal: mpg vs horsepower
# ─────────────────────────────────────────────────────────────
# • La relación entre las millas por galón (mpg) y la potencia del motor 
#   (horsepower) es claramente no lineal y decreciente.
# • Ajustar modelos polinomiales de distintos grados permite representar 
#   distintos niveles de complejidad de la curva.
# • El desafío principal es elegir un grado adecuado: 
#   demasiado bajo → subajuste (underfitting),
#   demasiado alto → sobreajuste (overfitting).

# ─────────────────────────────────────────────────────────────
# 2. División en entrenamiento y validación
# ─────────────────────────────────────────────────────────────
# • Separar los datos permite evaluar el poder predictivo real del modelo 
#   sobre observaciones NO utilizadas para entrenar.
# • El error de entrenamiento SIEMPRE disminuye al aumentar la complejidad, 
#   pero el error de validación suele tener una U invertida: 
#       menor error en un punto intermedio de complejidad.
# • Este es el principio básico de selección de modelo:
#       elegir el modelo que minimiza el error de generalización,
#       no el de entrenamiento.

# ─────────────────────────────────────────────────────────────
# 3. Curva de error para polinomios 1–10
# ─────────────────────────────────────────────────────────────
# • Se grafica tanto el error de entrenamiento como el de test.
# • El error de entrenamiento decrece monótonamente con el grado p.
# • El error de test primero baja, luego sube:
#   evidencia de sobreajuste en modelos demasiado flexibles.
# • El punto óptimo depende del split usado: una sola partición 
#   puede inducir alta variabilidad en la selección.

# Concepto clave:
#   La selección de modelo basada en una única partición es inestable,
#   y diferentes splits pueden sugerir diferentes grados óptimos.

# ─────────────────────────────────────────────────────────────
# 4. Variabilidad de las particiones
# ─────────────────────────────────────────────────────────────
# • Graficar varias particiones aleatorias ilustra que la asignación 
#   de puntos a entrenamiento/validación depende del azar.
# • Esto afecta el error de test estimado.
# • La lección teórica: el proceso de validación simple puede tener 
#   alta varianza en muestras pequeñas → CV es preferible.

# ─────────────────────────────────────────────────────────────
# 5. Validación cruzada K-fold
# ─────────────────────────────────────────────────────────────
# • Se divide el dataset en K subconjuntos similares ("folds").
# • Cada fold rota como conjunto de validación, y los restantes K−1 
#   como entrenamiento.
# • Se promedian los errores de los K modelos → estimación estable del 
#   error de generalización.
#
# Ventajas teóricas:
#   – Menor varianza en la estimación de error que un único split.
#   – Usa todo el dataset para entrenar y validar.
#   – Permite evaluar modelos de distinta complejidad con mayor precisión.

# Elección de K:
#   – K pequeño → mayor sesgo, menor varianza.
#   – K grande (como LOOCV) → menor sesgo, mayor varianza.
#   – K=5 o K=10 suelen ser buenos compromisos.

# ─────────────────────────────────────────────────────────────
# 6. Ejemplo de alta correlación espuria
# ─────────────────────────────────────────────────────────────
# • Se genera un dataset donde la variable objetivo depende SOLO 
#   de 7 predictores, pero se simulan 1000 predictores totales.
# • Al calcular las correlaciones con Y, algunos predictores irrelevantes 
#   aparecen fuertemente correlacionados simplemente por azar.
#
# Lección clave:
#   En situaciones de alta dimensionalidad (p >> n),
#   correlaciones marginales NO son un criterio fiable de selección,
#   porque generan múltiples falsos positivos.
#
# Conclusión:
#   Métodos regulares (ridge, lasso, elastic net), CV, o modelos jerárquicos
#   son esenciales cuando hay muchísimos predictores.

# ─────────────────────────────────────────────────────────────
# 7. Stepwise selection (forward, backward, both)
# ─────────────────────────────────────────────────────────────
# • Forward: parte del modelo vacío y agrega variables una a una según 
#   el criterio AIC (o similar).
# • Backward: parte del modelo completo y elimina predictores irrelevantes.
# • Both: combina ambos procedimientos.
#
# Problemas teóricos:
#   – La búsqueda greedy NO garantiza el modelo óptimo global.
#   – La inferencia posterior (p-values, CIs) está sesgada, porque las 
#     variables fueron seleccionadas basándose en los mismos datos.
#   – Stepwise tiende a sobreajustar, especialmente con muchas variables.
#
# Aun así, es útil como método heurístico cuando:
#   – No se requiere inferencia estricta,
#   – Se desea un modelo interpretable de pocas variables,
#   – El espacio de modelos completos sería demasiado grande.

# ─────────────────────────────────────────────────────────────
# 8. Conclusión general del bloque
# ─────────────────────────────────────────────────────────────
# • La elección de modelo requiere equilibrar complejidad y generalización.
# • La validación cruzada es una herramienta robusta para estimar 
#   desempeño fuera de muestra.
# • Modelos polinomiales muestran claramente el trade-off sesgo–varianza.
# • Correlaciones espurias y stepwise ilustran por qué la selección de 
#   variables requiere métodos más sólidos cuando la dimensionalidad es alta.
# • El principio unificador es que la evaluación de modelos debe hacerse 
#   siempre con datos NO usados para entrenar.

#######################################################################























################################################################################
################################################################################
################################# CLASE 4 ######################################
################################################################################
################################################################################

library(glmnet)
library(ISLR2)
#La sintaxis de glmnet es distinta a los ejemplos que veniamos viendo antes

summary(Hitters)
#Hay valores NA en salary que es la variable que nos interesa predecir

Hitters = na.omit(Hitters) # Remueve las filas con NA en algun lugar.
summary(Hitters)



x = model.matrix(Salary ~ .,  Hitters)[,-1] #Le sacamos el intercept. 
y = Hitters$Salary



grid <- 10^ seq (10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

head(coef(ridge.mod))

ridge.mod$lambda [50]
coef(ridge.mod)[, 50]

#Notar que los coeficientes de lambda mas chico son mas grandes 
ridge.mod$lambda [60]
coef(ridge.mod)[, 60]

### Separemos el set de datos en entrenamiento y validacion

set.seed (1)
train = sample (1: nrow(x), nrow(x) / 2)
y.test = y[-train]

ridge.mod <- glmnet(x[train , ], y[train], alpha = 0,
                    lambda = grid) # alpha = 0 indica que queremos ridge, 


ridge.pred <- predict(ridge.mod , s = 4, newx = x[test , ]) # Con parametro 4 de regularizacion

## Calculamos el error de test.
mean (( ridge.pred - y.test)^2)

## glmnet hace validacion cruzada con 10 cruces por defecto 

set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
mejor_lambda <- cv.out$lambda.min
lambda_1_desvio = cv.out$lambda.1se

mejor_lambda


abline(v = -log(mejor_lambda), col = 'darkblue')


abline(v = -log(lambda_1_desvio), col = 'purple') # regla de un desvio


plot(ridge.mod)
abline(v = -log(mejor_lambda), col = 'darkblue', lty = 2)



ridge.pred <- predict(ridge.mod , s = mejor_lambda ,
                      newx = x[test , ])

MSE =  mean (( ridge.pred - y.test)^2)


points( c(-log(mejor_lambda)), c(MSE) )



out <- glmnet(x, y, alpha = 0, lambda = mejor_lambda)

predict(out , type = "coefficients", s = mejor_lambda)[1:20 , ]
#Ninguno de los coeficientes es cero pero algunos estan cerca!


####### Ajustamos lasso

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1,
                    lambda = grid )


plot(lasso.mod) #Ahora ploteamos el modelo entrenado con train.

set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1, lambda = grid)
plot(cv.out)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1) #podemos dejar que glmnet elija la grilla
plot(cv.out)

lasso_lam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = lasso_lam ,
                      newx = x[test , ])
mean (( lasso.pred - y.test)^2)

points(c(-log(lasso_lam)), c( mean (( lasso.pred - y.test)^2) ))



out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients",
                      s = lasso_lam)[1:20 , ]
lasso.coef

lasso.coef[lasso.coef != 0]

###############################################################
# Comentario teórico sobre el bloque de código que ajusta:
#   • Modelos Ridge (α = 0)
#   • Modelos Lasso (α = 1)
# usando glmnet sobre el dataset "Hitters".
#
# Este chunk resume los conceptos estadísticos que ilustran 
# los pasos del código (NO explica su funcionamiento técnico).
###############################################################

# ─────────────────────────────────────────────────────────────
# 1. Contexto general: ¿por qué usar penalización?
# ─────────────────────────────────────────────────────────────
# • Cuando hay muchas variables (o colinealidad), los estimadores 
#   OLS son inestables y tienen alta varianza.
# • Los métodos de regularización agregan una penalidad a la 
#   magnitud de los coeficientes, reduciendo varianza a cambio de 
#   introducir un pequeño sesgo (sesgo-beneficio de varianza).
# • glmnet implementa ambos métodos clásicos de penalización: 
#       – Ridge: penaliza la suma de cuadrados de coeficientes.
#       – Lasso: penaliza la suma de los valores absolutos.

# En resumen:
#   Ridge estabiliza modelos y Lasso además puede seleccionar
#   variables eliminando coeficientes.

# ─────────────────────────────────────────────────────────────
# 2. Ridge regression (alpha = 0)
# ─────────────────────────────────────────────────────────────
# • La penalización L2 (||β||^2) empuja coeficientes hacia cero, 
#   pero nunca los hace exactamente cero.
# • A medida que lambda ↓, la restricción se relaja y los 
#   coeficientes crecen en magnitud.
# • Ridge es especialmente útil cuando:
#       – hay multicolinealidad,
#       – se desea mantener todas las variables en el modelo,
#       – el objetivo es mejorar la predicción reduciendo varianza.
# • La trayectoria de coeficientes (plot(ridge.mod)) muestra cómo 
#   los parámetros se acercan a cero al aumentar lambda.

# Idea central:
#   Ridge controla la magnitud de β pero NO realiza selección
#   explícita de variables.

# ─────────────────────────────────────────────────────────────
# 3. Validación cruzada para elegir lambda
# ─────────────────────────────────────────────────────────────
# • cv.glmnet realiza validación cruzada k-fold (por defecto, k = 10).
# • Produce dos valores útiles:
#       – lambda.min: el lambda con menor error medio de CV.
#       – lambda.1se: el valor más parsimonioso dentro de un
#         desvío estándar del mínimo (regla 1-SE).
# • La regla del 1-SE favorece modelos más simples y estables.
#
# Concepto clave:
#   CV selecciona automáticamente el nivel apropiado de penalización, 
#   equilibrando ajuste y complejidad para minimizar error de test.

# ─────────────────────────────────────────────────────────────
# 4. Lasso regression (alpha = 1)
# ─────────────────────────────────────────────────────────────
# • La penalización L1 (||β||_1) no solo reduce magnitudes: 
#   puede llevar coeficientes exactamente a cero.
# • Lasso realiza selección automática de variables, permitiendo
#   construir modelos más interpretables.
# • Es especialmente útil cuando:
#       – hay muchas predictores irrelevantes,
#       – se desea un modelo esparso,
#       – importa la interpretabilidad tanto como la predicción.
#
# Rasgo distintivo:
#   A diferencia de Ridge, Lasso puede eliminar completamente 
#   predictores → coeficientes exactamente cero.

# ─────────────────────────────────────────────────────────────
# 5. Comparación conceptual Ridge vs Lasso
# ─────────────────────────────────────────────────────────────
# • Ridge:
#       – no hace selección de variables,
#       – distribuye el “peso” entre predictores correlacionados,
#       – tiende a ser más estable cuando hay mucha colinealidad.
# • Lasso:
#       – selecciona predictores,
#       – cuando variables están muy correlacionadas, puede elegir
#         arbitrariamente una y descartar las demás,
#       – produce modelos más esparsos y fáciles de interpretar.
#
# • La comparación empírica del MSE entre ambos modelos en test
#   ilustra cómo la penalización correcta depende de la estructura
#   real del problema y del tipo de objetivo (predicción vs. parsimonia).

# ─────────────────────────────────────────────────────────────
# 6. Interpretación de los coeficientes finales
# ─────────────────────────────────────────────────────────────
# • En Ridge, todos los coeficientes son distintos de cero, 
#   aunque algunos quedan muy atenuados.
# • En Lasso, solo un subconjunto queda distinto de cero: el modelo
#   resultante incluye solo unas pocas variables relevantes según CV.
# • Examinar lasso.coef[lasso.coef != 0] permite ver la selección
#   automática realizada.

# En conclusión:
#   Ridge mejora estabilidad y predicción.
#   Lasso crea modelos más simples y selecciona variables.
#   Ambos son herramientas de shrinkage reguladas por λ, 
#   cuyo valor óptimo se elige mediante validación cruzada.

###############################################################

















################################################################################
################################################################################
################################# CLASE 5 ######################################
################################################################################
################################################################################
library(ISLR2)
library(tidyverse)
library(tree)

arbol = tree(Salary ~ Hits + Years, data = Hitters)


plot(arbol)
text(arbol, pretty = 0)

summary(arbol)

newdata = crossing( Hits = seq(0, 200, 10 ), Years = 0:10 )


predict(arbol, newdata = data.frame(Hits = c(20, 100) , Years =c(5, 2)))

evaluated_grid = newdata %>% mutate( Salary = predict(arbol, newdata = newdata))

ggplot(evaluated_grid, aes(x = Years, y = Hits, color = Salary))


set.seed(12345)
arbol = tree(Salary ~ . , data = Hitters)

arbol_cv = cv.tree(arbol, K = 5) # Validación K = 5 cruces


par(mfrow = c(1, 2))
plot(arbol_cv$size, arbol_cv$dev, type = "b", xlab= 'Nodos del arbol', ylab = 'Deviance') 

plot(arbol_cv$k, arbol_cv$dev, type = "b", xlab = 'alfa', ylab = 'Deviance') # k corresponde al alpha de validacion cruzada


arbol_pruneado = prune.tree(arbol, best = arbol_cv$size[which.min(arbol_cv$dev)] )
par(mfrow = c(1,1))

plot(arbol_pruneado)
text(arbol_pruneado, pretty = 0)


#### Clasificacion



data = Carseats
data['High'] = factor(ifelse(data$Sales <= 8, "No", "Yes"))

arbol = tree(High ~ . -Sales , data = data , split = 'gini')  # Sacamos sales si no es muy facil!
summary(arbol)

plot(arbol)
text(arbol, pretty = 0)


set.seed (2)
train = sample (1: nrow(Carseats ), 200)
Carseats.test = data[-train , ]
High.test <- data$High[-train]
tree.carseats <- tree(High ~ . - Sales , data , subset = train, split = 'gini')

tree.pred <- predict(tree.carseats , Carseats.test , type = "class")

confusion = table(tree.pred , High.test) # Matriz de confusión.

(accuracy = (confusion[1,1] + confusion[2,2])/ sum(confusion))

(precision = confusion[2,2]/(sum(confusion[2, ])))


#### Bagging y RF

library( randomForest )
set.seed (1)

train <- sample (1: nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston , subset = train)

summary(tree.boston)

plot(tree.boston)
text(tree.boston , pretty = 0)

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size , cv.boston$dev , type = "b")

yhat <- predict(tree.boston , newdata = Boston[-train , ])
boston.test <- Boston[-train , "medv"]
plot(yhat , boston.test)
abline (0, 1)
mean (( yhat - boston.test)^2)

bag.boston <- randomForest( medv ~ . , data = Boston , subset = train , 
                            mtry = 12, importance = TRUE) # Mtry = m
bag.boston

yhat.bag <- predict(bag.boston , newdata = Boston[-train , ])
plot(yhat.bag , boston.test)

points(yhat, boston.test, col = 'blue')
abline (0, 1)

mean (( yhat.bag - boston.test)^2)                       


bag.boston <- randomForest (medv ~ ., data = Boston ,
                            subset = train , mtry = 12, ntree = 25)
yhat.bag <- predict(bag.boston , newdata = Boston[-train , ])
mean (( yhat.bag - boston.test)^2)

set.seed (1)
rf.boston <- randomForest(medv~ ., data = Boston ,
                          subset = train , mtry = 6, importance = TRUE )
yhat.rf <- predict(rf.boston , newdata = Boston[-train , ])
mean (( yhat.rf - boston.test)^2)


## Graficamos importance variables 
varImpPlot(rf.boston)

# Survived: 0=No, 1=Yes
# Pclass = {1=1st Class, 2=2nd Class, 3=3rd Class}
# Name
# Sex = {male, female}
# Age = Age in years
# SibSp = Number of siblings and spouses aboard
# Parch = Number of parents and children aboard
# Ticket = Ticket number
# Fare = Passenger fare
# Cabin = Cabin number
# Embarked = {C=Cherbourg, Q=Queenstown, S=Southampton}

library(lightgbm)

data <- read.csv("Datasets/Titanic.csv")
n <- nrow(data)

X <- data[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
cats <- c("Pclass", "Sex", "Embarked")
for (cat in cats) {
  X[[cat]] <- as.numeric(as.factor(X[[cat]]))
}
y <- data$Survived

train_idx <- sample(n, 0.8 * n)
ds_train <- lgb.Dataset(
  data = as.matrix(X[train_idx, ]),
  label = y[train_idx],
  categorical_feature = which(names(X) %in% cats)
)
X_test <- as.matrix(X[-train_idx, ])
y_test <- y[-train_idx]

params <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.1,
  num_trees = 100,
  num_leaves = 63,
  max_depth = 7
)
model <- lgb.train(
  params = params,
  data = ds_train,
  verbose = -1
)

p_pred <- predict(model, X_test)
y_pred <- as.integer(p_pred > 0.5)
TP <- sum(y_pred == 1 & y_test == 1)
TN <- sum(y_pred == 0 & y_test == 0)
FP <- sum(y_pred == 1 & y_test == 0)
FN <- sum(y_pred == 0 & y_test == 1)
accuracy <- (TP + TN) / length(y_test)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
round(accuracy, 3)
round(precision, 3)
round(recall, 3)
table(Predicted = y_pred, Actual = y_test)

###############################################################
# CONCLUSIONES TEÓRICAS — MODELOS DE ÁRBOLES Y ENSEMBLES
###############################################################

#--------------------------------------------------------------
# 1. ÁRBOLES DE REGRESIÓN
#--------------------------------------------------------------
# - Un árbol de regresión particiona el espacio de predictores
#   en regiones rectangulares y ajusta en cada región el
#   promedio de Y.
# - Ventaja: interpretabilidad, manejo natural de interacciones
#   y no linealidades.
# - Desventaja: alta varianza si el árbol crece sin restricciones.
# - Para construirlo: se elige en cada paso el split que produce
#   mayor reducción de la suma de cuadrados residual (RSS).

#--------------------------------------------------------------
# 2. PODA (PRUNING) CON COST-COMPLEXITY
#--------------------------------------------------------------
# - Un árbol grande sobreajusta. Se genera un "árbol máximo"
#   y luego se poda aplicando una penalización al tamaño.
# - La función de costo es:
#     R_α(T) = RSS(T) + α * |T|
#   donde |T| = número de nodos terminales y α controla
#   el trade-off sesgo-varianza.
# - α grande → árboles muy pequeños (más sesgo, menos varianza).
#   α chico → árboles grandes (menos sesgo, más varianza).
# - La validación cruzada (cv.tree) estima el mejor α.

#--------------------------------------------------------------
# 3. ÁRBOLES DE CLASIFICACIÓN
#--------------------------------------------------------------
# - La idea es la misma, pero la impureza de un nodo se mide
#   con Gini, entropía o error de clasificación.
# - La predicción es la clase mayoritaria en cada región.
# - Métricas relevantes: accuracy, precision, recall.
# - Regla general: los árboles de clasificación también
#   requieren poda para evitar sobreajuste.

#--------------------------------------------------------------
# 4. BAGGING (Bootstrap Aggregating)
#--------------------------------------------------------------
# - Problema: alta varianza de los árboles individuales.
# - Solución: entrenar muchos árboles bootstrap (muestras
#   con reemplazo) y promediar predicciones.
# - Reduce fuertemente la varianza sin aumentar el sesgo.
# - En regresión: se promedia.
#   En clasificación: se vota.
# - mtry = número de variables disponibles en cada split:
#   * En bagging se usan todas (mtry = p).

#--------------------------------------------------------------
# 5. RANDOM FORESTS
#--------------------------------------------------------------
# - Extiende bagging introduciendo *decorrelación* entre árboles:
#   en cada split se selecciona aleatoriamente un subconjunto
#   de predictores de tamaño mtry < p.
# - Esto evita que todos los árboles usen siempre las mismas
#   variables fuertes.
# - Resultado: mejor performance que bagging en general.
# - También provee:
#     * importance = importancia de variables
#     * reducción de MSE y interpretabilidad parcial.

#--------------------------------------------------------------
# 6. BOOSTING (Ej.: LightGBM)
#--------------------------------------------------------------
# - No construye muchos árboles independientes sino una
#   secuencia donde cada árbol intenta corregir los errores
#   del anterior.
# - Modelo aditivo:
#     F_m(x) = F_{m-1}(x) + ν * h_m(x)
#   donde ν = learning rate (típicamente pequeño).
# - Tiende a obtener mejor precisión que bagging/RF cuando
#   los hiperparámetros están bien ajustados.
# - Sensible a:
#     * learning_rate
#     * número de árboles
#     * profundidad máxima
#     * num_leaves (en LightGBM)
# - Métricas típicas para clasificación binaria:
#     accuracy, precision, recall, F1, AUC.

#--------------------------------------------------------------
# 7. COMPARACIÓN GENERAL
#--------------------------------------------------------------
# Árbol solo:
#   + Interpretación excelente
#   – Varianza muy alta, peor performance predictiva
#
# Árbol + poda:
#   + Reduce varianza
#   – Aun así menos preciso que RF y boosting
#
# Bagging:
#   + Baja varianza notablemente
#   – Puede seguir correlacionado si hay predictores muy fuertes
#
# Random Forest:
#   + Reduce varianza y correlación entre árboles
#   + Muy buen baseline
#   – Menos interpretable
#
# Boosting (LightGBM / XGBoost):
#   + El mejor en performance en la mayoría de problemas tabulares
#   – Sensible a hiperparámetros
#   – Menos interpretable aún
















################################################################################
################################################################################
################################# CLASE 6 ######################################
################################################################################
################################################################################

library(ISLR2)


Wage
summary(Wage)

edades = 18:100


#Ajustamos una regresión polinómica de grado 4
fit = lm( wage ~ poly(age,4) , data = Wage )
fit2 = lm( wage ~ I(age) + I(age^2) + I(age^3) + I(age^4) , data = Wage ) #son equivalentes

#Ajustamos una regresión logista para predecir ingresos mayores a 250

glmfit = glm( I(wage > 250) ~ poly(age,4), data = Wage, family = binomial)


#Ajustamos por funciones escalon




fit_escalon = lm( wage ~ I( 30<= age & age < 50) + I( 50<= age & age < 70) + I( 70<= age), data =Wage )
fit_escalon2 = lm(wage ~ I( 20<= age & age < 40) + I( 40<= age & age < 60) + 
                    I( 60<= age & age < 80) + I(80 <= age), data =Wage )

plot(Wage$age, Wage$wage, pch = '.')
lines( edades, predict( fit_escalon , newdata = data.frame( age = edades) ), col = 'purple', lwd = 2)
lines(edades, predict(fit_escalon2, newdata = data.frame(age = edades)), col = 'tomato', lwd = 2  )


## Ajustamos con splines

library(splines)
fit_spline <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage) 
pred <- predict(fit, newdata = list(age = edades), se = TRUE) # se = TRUE da desvio estandar en cada punto.

plot(Wage$age, Wage$wage, pch = '.')
lines( edades, pred$fit, col = 'magenta' )
lines( edades, pred$fit + 2*pred$se.fit, col = 'magenta', lty = 2 )
lines( edades, pred$fit - 2*pred$se.fit, col = 'magenta', lty = 2 )

## Ajustamos con smoothing spline. Podemos elegir por df o pedir validacion cruzada loocv

fit_natural_spline <- lm(wage ~ ns(age, df = 6), data = Wage) # No tiene parametro de suavizado 
pred_ns <- predict(fit, newdata = list(age = edades), se = T) # se = TRUE da desvio estandar en cada punto.



#### Construccion de un grafico para ver la diferencia del suavizado

set.seed(1)
x = runif(30, -3.14, 3.14)
f = function(x) { sin(x)}

y = f(x) + rnorm(length(x) , sd = 0.5)
plot(x, y, pch = 'x')
grilla = seq( -3.14, 3.14, by = 0.1) 

lines(grilla, f(grilla) , lty = 2)

data = data.frame(x = x, y = y)

fit_0 = smooth.spline(data$x, data$y, lambda = 0.000001)
fit_1 = smooth.spline(data$x, data$y, lambda = 0.001)
fit_00 = smooth.spline(data$x, data$y, lambda = 10)
fit_cv = smooth.spline(data$x, data$y, cv = TRUE)

lines(grilla, predict(fit_0, x = grilla )$y , col = 'purple')
lines(grilla, predict(fit_00, x = grilla )$y , col = 'tomato')
lines(grilla, predict(fit_cv, x = grilla )$y , col = 'darkblue')
legend("bottomright", 
       legend = c(
         paste("λ = 0.000001"),
         paste("λ = 10"),
         paste("CV (λ =", round(fit_cv$lambda, 4), ")")
       ),
       col = c('purple', 'tomato', 'darkblue'),
       lty = 1,
       lwd = 2,
       cex = 0.8,
       bty = "n")


### Ajuste gam

gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage) 
#Tenemos que indicar cuanto queremos suavizar

library(gam)


gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education,
              data = Wage)
par(mfrow = c(1,3))

plot(gam.m3, se = TRUE, col = 'purple')

plot.Gam(gam1)

### Logistic regression with gam

gam.lr <- gam( I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")


# sera lineal la funcion de año?

gam.m1 <- gam(wage ~ s(age , 5) + education , data = Wage)
gam.m2 <- gam(wage ~ year + s(age , 5) + education ,
              data = Wage)
sanova(gam.m1 , gam.m2 , gam.m3 , test = "F")


library(gam)
library(splines)

# Ajustar modelos
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)

# Crear grilla para year
year_grid <- seq(min(Wage$year), max(Wage$year), length.out = 100)

# Data frame para predicción (manteniendo otras variables constantes)
newdata <- data.frame(
  year = year_grid,
  age = median(Wage$age),  # Valor fijo
  education = names(which.max(table(Wage$education)))  # Categoría más frecuente
)

# Predecir efecto de year para cada modelo
pred_lm <- predict(gam1, newdata = newdata)
pred_gam <- predict(gam.m3, newdata = newdata)

par(mfrow = c(1,1))

# Líneas de predicción
plot(year_grid, pred_lm, col = "red", lwd = 3, lty = 1, type = 'l')
lines(year_grid, pred_gam, col = "blue", lwd = 3, lty = 2)

# Legend
legend("topleft", 
       legend = c("lm + ns(year,4) - Flexibilidad fija", 
                  "gam + s(year,4) - Suavizado adaptativo"),
       col = c("red", "blue"), lty = 1:2, lwd = 3, cex = 0.8)


###############################################################
# Comentario teórico sobre la comparación entre:
#   1) Modelo lineal con splines naturales: lm + ns(year, 4)
#   2) Modelo GAM con spline suavizado: gam + s(year, 4)
#
# Este bloque resume los conceptos estadísticos ilustrados
# por el código que compara predicciones para "year".
###############################################################

# • Ambos modelos permiten capturar relaciones no lineales,
#   pero lo hacen de formas conceptualmente distintas.

# ─────────────────────────────────────────────────────────────
# 1. Splines naturales (ns(year, 4)): flexibilidad fija
# ─────────────────────────────────────────────────────────────
# • La forma de la curva queda totalmente determinada por:
#       – el número de grados de libertad,
#       – la ubicación de los nudos.
# • No existe penalización a la curvatura: el modelo utiliza
#   toda la flexibilidad disponible.
# • Esto hace que sea un modelo no lineal pero esencialmente
#   paramétrico: su complejidad está prefijada por el analista.
# • Riesgos:
#       – sobreajuste si los df son muy altos,
#       – subajuste si son demasiado bajos.
#
# En resumen:
#   El spline natural ofrece una forma "rígida": la relación
#   año–salario viene impuesta por la elección del usuario.

# ─────────────────────────────────────────────────────────────
# 2. Splines penalizados en GAM (s(year, 4)): suavizado adaptativo
# ─────────────────────────────────────────────────────────────
# • El término s(year, 4) también usa una base spline, pero
#   incorpora un término de penalización sobre la curvatura.
# • El suavizado se selecciona automáticamente mediante GCV/REML:
#       – si los datos sugieren poca curvatura → el modelo se
#         vuelve casi lineal,
#       – si existe variación real → permite más flexibilidad.
# • Por eso se pide un "máximo" de flexibilidad, pero el modelo
#   elige cuánta usar.
# • Es un modelo semiparamétrico: combina partes lineales con
#   componentes no paramétricos regularizados.

# En resumen:
#   El GAM ajusta la complejidad a los datos, evitando tanto
#   el sobreajuste como el subajuste mediante penalización.

# ─────────────────────────────────────────────────────────────
# 3. ¿Qué muestra la comparación gráfica?
# ─────────────────────────────────────────────────────────────
# • Al evaluar "pred_lm" (ns) vs "pred_gam" (s) sobre un grid de year,
#   se observan las diferencias en la forma de f(year).
# • Si la curva del GAM es más suave, implica que los datos no
#   justifican tanta curvatura como permitiría el spline fijo.
# • Si ambas curvas se parecen, significa que el suavizado elegido
#   por el GAM considera necesaria esa cantidad de curvatura.
# • Mantener constantes "age" y "education" permite interpretar la
#   función f(year) como un efecto marginal puro.

# En resumen:
#   La comparación permite visualizar cómo cambia la forma del
#   efecto de year dependiendo de si la suavidad es impuesta
#   (ns) o aprendida (s).

# ─────────────────────────────────────────────────────────────
# 4. Conclusión teórica general
# ─────────────────────────────────────────────────────────────
# • Un spline natural brinda una no linealidad con estructura fija.
# • Un GAM usa suavizado penalizado para elegir automáticamente la
#   complejidad adecuada, actuando como un método de regularización.
# • Comparar ambos enfoques permite evaluar si year necesita un
#   efecto no lineal y si la complejidad puede ser determinada por
#   los datos en lugar de fijarse manualmente.

###############################################################

