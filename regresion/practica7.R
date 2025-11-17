library(ggplot2)
data(CASchools, package = "AER")
data<-CASchools

head(data)

################################################################################
############################### EJERCICIO 1 ####################################
################################################################################

# 1a. Graficas math ~ income
ggplot(data, aes(x = income, y = math)) + geom_point() 

# 1b. Modelo lineal simple
model1 <- lm(math ~ income, data = data)
model1.sum <- summary(model1) 

# 1c. Escribir el modelo; reportar sigma.
#E(math | income) = b0 + b1*income
model1.sum$sigma

#1d. Agregar linea al gráfico.
ggplot(data, aes(x = income, y = math)) + geom_point() + 
  geom_abline(slope = model1.sum$coefficients[2], 
              intercept = model1.sum$coefficients[1], color = "red")

#1e. Es significativo el b1? Interpretar.

# Es significativo y tiene sentido (se ve una relación creciente claramente marcada).
# Es evidente que la forma funcional puede mejorarse, porque se ve derivada no-constante.

#1f. Residuos vs. fitted.
par(mfrow = c(2,2))
plot(model1)
# Se aprecia que especialmente en una zona de alto income,
# los residuos son muy negativos, es decir, el modelo claramente sobre-estima. Hay evidencia de no-linealidad.

# 1g. Modelo con polinomio
model2 <- lm(math ~ poly(income, 2), data = data)
#E(math | income) = b0 + b1*income + b2*income**2

# 1h. Superponer el gráfico
ggplot(data, aes(x = income, y = math)) + geom_point() + 
  geom_abline(slope = model1.sum$coefficients[2], 
              intercept = model1.sum$coefficients[1], color = "red") +
  geom_line(aes(x = data$income, y = model2$fitted.values), color = "blue")


plot(model2, 1) # Ahora sí! Hmocedástico y con la media en cero.

# 1i. Significatividad del término cuadrático
summary(model2) # Super, y claro, tiene sentido.


# 1j. Modelo con polinomio de grade 3
model3 <- lm(math ~ poly(income, 3), data = data)
summary(model3)
ggplot(data, aes(x = income, y = math)) + geom_point() + 
  geom_abline(slope = model1.sum$coefficients[2], 
              intercept = model1.sum$coefficients[1], color = "red") +
  geom_line(aes(x = data$income, y = model2$fitted.values), color = "blue")+
  geom_line(aes(x = data$income, y = model3$fitted.values), color = "darkgreen")

plot(model3, 1)

# 1k. Test en simultáneo para cuadrático y cúbico
anova(model3, model1) # Sí. Son significativos.

# 1l. R cuadrado. 
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared # Gana
summary(model3)$adj.r.squared

# 1m. Sigmas

summary(model1)$sigma
summary(model2)$sigma # Gana
summary(model3)$sigma

# 1n. Ganador.
# Más parsimonioso; mejor sigma, mejor adj.r.squared...

# 1ñ. Ajustar log(income)
model4 <- lm(math ~ log(income), data = data)

ggplot(data, aes(x = income, y = math)) + geom_point() + 
  geom_abline(slope = model1.sum$coefficients[2], 
              intercept = model1.sum$coefficients[1], color = "red") +
  geom_line(aes(x = data$income, y = model2$fitted.values), color = "blue")+
  geom_line(aes(x = data$income, y = model3$fitted.values), color = "darkgreen") + 
  geom_line(aes(x = data$income, y = model4$fitted.values), color = "yellow")



summary(model2)$sigma 
summary(model4)$sigma # Gana

summary(model2)$adj.r.squared 
summary(model4)$adj.r.squared # Gana

################################################################################
############################### EJERCICIO 2 ####################################
################################################################################
data <- data %>%  mutate(stratio = students/teachers,
                 score = (math + read)/2, 
                 low_no_eng = english <= 8,
                 classeng = factor(case_when(english <= 5 ~ 'C1',
                                      english <=10 ~ 'C2',
                                      english <= 20 ~ 'C3',
                                      english <= 50 ~ 'C4',
                                      T ~ 'C5')),
                 compst = computer/students)

# 2a. Chequeo de correlación
ggplot(data, aes(x = read, y = math)) + geom_point() +
  annotate("text", 
           x = max(data$read), 
           y = min(data$math), 
           label = paste0("r muestral = ", round(cor(data$read, data$math), 2)), 
           hjust = 1, vjust = 0, size = 5, color = "blue")

#2b. . Grafique score versus stratio. Proponga un 
# modelo con la variable score como respuesta y la variable stratio como explicativa

ggplot(data, aes(x = stratio, y = score)) + geom_point() + geom_smooth(method = "lm", 
                                                                       show.legend = T)

model5 <- lm(score ~ stratio, data = data)
summary(model5)
#Superponga al scatter plot la recta ajustada. 
# ¿Es significativa la variable?  Sí
# ¿Proporciona una buena explicaci´on de las notas?  no, al menos no solo
# ¿Es razonable el v´ınculo encontrado entre ellas?  sí, pero ...

# 2c. Agregado de variables.
ggplot(data, aes(x = stratio, y = score, color = low_no_eng)) + geom_point() + geom_smooth(method = "lm", 
                                                                       show.legend = T)


model6 <- lm(score ~ stratio*low_no_eng, data = data)

summary(model6)
#E(Y|X) = bo + b1*stratio + b2*low_no_eng + b3*stratio*low_no_eng
#E(Y|X = (stratio, T)) =  (bo + b2) + (b1+b3)*stratio
#E(Y|X = (stratio, T)) =  (bo) + (b1)*stratio


model7 <- lm(score ~ stratio + low_no_eng, data = data)
summary(model7)


ggplot(data, aes( y = score, x = classeng, color = classeng)) + geom_boxplot()
ggplot(data, aes(x = stratio, color = classeng, y = score)) + geom_point()


model8 <- lm(score ~ stratio*classeng, data = data)
model8.base <- lm(score ~ stratio + classeng, data = data)

anova(model8.base, model8) #no significativo, saco la interaccion.

ggplot(data, aes(x = stratio, y = score, color = classeng)) + geom_point() +
  geom_abline(intercept = model8.base$coefficients[1],
              slope = model8.base$coefficients[2], color = "red") +
  geom_abline(intercept = model8.base$coefficients[1] + model8.base$coefficients[3],
              slope = model8.base$coefficients[2], color = "lightgreen") +
  geom_abline(intercept = model8.base$coefficients[1] + model8.base$coefficients[4],
              slope = model8.base$coefficients[2], color = "darkgreen") +
  geom_abline(intercept = model8.base$coefficients[1] + model8.base$coefficients[5],
              slope = model8.base$coefficients[2], color = "blue") +
  geom_abline(intercept = model8.base$coefficients[1] + model8.base$coefficients[6],
              slope = model8.base$coefficients[2], color = "violet")



# 2.e. Ajuste un modelo lineal para explicar score con las dos variables stratio y english.
model9 <- lm(score ~ stratio + english, data = data)
summary(model9)$adj.r.squared # Gana

# 2f.
model10 <- lm(score ~ stratio + english + lunch, data = data)
summary(model10)$adj.r.squared # Gana

# 2g.
model11 <- lm(score ~ stratio + english + lunch + compst, data = data)
summary(model11)$adj.r.squared # Gana
