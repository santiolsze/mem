library(ggplot2)
################################ EJERCICIO 6 ###################################
salary <- read.csv("../../Downloads/salary.txt", sep = " ")
attach(salary)

Sex <- as.factor(Sex)


# a: gráficos
par(mfrow = c(1,2))
boxplot(Salary ~ Sex)
boxplot(Salary ~ Sex + Rank)



#b. Test de hipotesis para salario medio
t.test(Salary[Sex == 0], Salary[Sex == 1], alternative = "greater")

# Ajuste lineal
modelo.base <- lm(Salary ~ Sex + Year)

summary(modelo.base)
b0 <- coef(modelo.base)["(Intercept)"]
b1 <- coef(modelo.base)["Sex1"]
b2 <- coef(modelo.base)["Year"]

ggplot(data = salary, aes(x=Year, y = Salary, color = factor(Sex))) + 
  geom_point() + 
  geom_abline(slope = b2, intercept = b0 + b1, color = "blue") + 
  geom_abline(slope = b2, intercept = b0, color = "red")

