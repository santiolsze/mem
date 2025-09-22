library(dplyr)
library(ggplot2)

vinos <- read.csv("../../Downloads/vinos2.txt", sep = ",")
vinos$varie <- as.factor(vinos$varie)
############################# EJERCICIO 1 ######################################
# 1a: Grafique el precio en funci´on del tiempo.
ggplot(data = vinos, aes(x = tiempo, y = precio)) + geom_smooth() + geom_point(size = 0.1)

# 1b:  modelo de regresi´on lineal simple que explique el el precio en funci´on del tiempo.
mod1 <- lm(precio ~ tiempo, data = vinos)

# 1c: Escriba el modelo ajustado. Reporte el valor de σ estimado por la muestra.
### E(precio|tiempo=t) = b0 + b1*t
mod1.summary <- summary(mod1)
mod1.summary$sigma #38.37

# 1d: Superponga al gr´afico del ´ıtem (a) la recta ajustada, en color rojo
ggplot(data = vinos, aes(x = tiempo, y = precio)) +
  geom_smooth() + 
  geom_point(size = 1e-2, color = "black") +
  geom_abline(slope = coef(mod1)["tiempo"], intercept =  coef(mod1)["(Intercept)"], color = "red")

# 1e: ¿Es significativo el coeficiente β que acompa˜na a la covariable tiempo?
#  Interprete el ajuste obtenido. ¿Es razonable el resultado obtenido? O sea,
# ¿tiene sentido la relaci´on encontrada entre ambas variables?
mod1.summary$coefficients["tiempo","Pr(>|t|)"] <= 0.05

# No tiene sentido: el precio medio baja ~1 dolar por cada mes de añejamiento.


############################# EJERCICIO 2 ######################################
# 2a: Repita el gr´afico del precio en funci´on del tiempo, pero esta vez ....
ggplot(data = vinos, aes(x = tiempo, y = precio, colour = varie)) +
  geom_point(size = 1e-1)

# 2b: Modelo de regresi´on lineal m´ultiple aditivo, tiempo y varie.
mod2 <-  lm(precio ~ tiempo + varie, data = vinos)

# 2c: Escriba el modelo ajustado. Reporte el valor de σ estimado por la muestra.
### E(precio|tiempo=t, varie = v) = b0 + b1*t +b2*is_v2 + b3*is_v3 + b4*is_v4
mod2.summary <- summary(mod2)
mod2.summary$sigma #25.46

# 2d: Escriba las 4 rectas ajustadas por este modelo.  

# Con el modelo propuesto, ¿las rectas est´an forzadas a cumplir alguna relaci´on entre s´ı?
# Sí, están forzadas a ser paralelas.
### E(precio|tiempo=t, varie = varie_1) = b0 + b1*t 
### E(precio|tiempo=t, varie = varie_2) = (b0+b2) + b1*t 
### E(precio|tiempo=t, varie = varie_2) = (b0+b3) + b1*t
### E(precio|tiempo=t, varie = varie_2) = (b0+b4) + b1*t

# 2e: Gráfico correspondiente cada una con el color correspondiente.
coefs <- mod2$coefficients
b0 <- coefs["(Intercept)"]
b1 <- coefs["tiempo"]
b2 <- coefs["varie2"]
b3 <- coefs["varie3"]
b4 <- coefs["varie4"]
ggplot(data = vinos, aes(x = tiempo, y = precio, colour = varie)) +
  geom_point(size = 1e-1) + 
  geom_abline(intercept = b0, slope = b1, color = "red") +
  geom_abline(intercept = b0 + b2, slope = b1, color = "darkgreen") +
  geom_abline(intercept = b0 + b3, slope = b1, color = "darkblue") +
  geom_abline(intercept = b0 + b4, slope = b1, color = "violet") 
  
# 2f: ¿Es significativo el coeficiente β que acompa˜na a la covariable tiempo?
mod2.summary$coefficients["tiempo","Pr(>|t|)"] <= 0.05 # Sí

# Es razonable, por cada mes de añejamiento (a variedad constante), aumenta 4 USD el precio.

#2g: ¿Es significativa la variable varie? No sale del summary!
# Si no es significativa, implicaría b2=b3=b4=0, es decir, H0:  b2=b3=b4=0, H1: (b2,b3,b4) != (0,0,0)
anova.mod1.mod2<-anova(mod1, mod2)
anova.mod1.mod2$`Pr(>F)` < 0.05 # Super significativa

#2h: ¿puede estar seguro de que es correcto el modelo planteado con rectas paralelas? 
# No, podría ser correcto incluir interacción tiempo*varie.

############################# EJERCICIO 3 ######################################

# 3a: Modelo con interacción
mod3 <-  lm(precio ~ tiempo*varie, data = vinos)

# 3b: Escriba el modelo ajustado. Reporte el valor de σ estimado por la muestra.
### E(precio|tiempo=t, varie = v) = b0 + b1*t +b2*is_v2 + b3*is_v3*t + b4*is_v4*t +b5*is_v2*t + b6*is_v3*t + b7*is_v4*t
mod3.summary <- summary(mod3)
mod3.summary$sigma #25.46 (idem mod2)

# 3c: Escriba las rectas. Están forzadas a ser paralelas? -> No!
### E(precio|tiempo=t, varie = varie_1) = b0 + b1*t 
### E(precio|tiempo=t, varie = varie_2) = (b0+b2) + (b1+b5)*t 
### E(precio|tiempo=t, varie = varie_2) = (b0+b3) + (b1+b6)*t
### E(precio|tiempo=t, varie = varie_2) = (b0+b4) + (b1+b7)*t

# 3d: Testear si las rectas son paralelas entre sí. 
# H0:  b5=b5=b7=0, H1: (b5,b6,b7) != (0,0,0)
anova.mod2.mod3<-anova(mod2, mod3)
anova.mod2.mod3$`Pr(>F)` < 0.05 # Nope!
# No hay evidencia para concluir que las diferentes variedades aumenten 
# su precio en magnitud diferente con un año de añejamiento. 
