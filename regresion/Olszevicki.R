data <- read.table("../../Downloads/diabetes1.csv",sep = " ", header = T)


# Cuentas en ejercicio 2
# a
qt(1-0.0928/2, df = 97)

# b
1.9993 - qt(0.975, df = 97) * 0.4264
1.9993 + qt(0.975, df = 97) * 0.4264

#c
pt(-1.286, df = 97)

#d
sigma_hat <- matrix(c(2.62, -0.51, -0.09, -0.51, 0.4264**2, -0.01, -0.09,-0.01, 0.1277**2), nrow = 3, byrow = T)
resultado <- sigma_hat / (7.221**2)

# ITEM 3A
model1 <- lm(y ~ age, data = data)
model2 <- lm(y ~ fem, data = data)
model3 <- lm(y ~ bmi, data = data)
model4 <- lm(y ~ bp, data = data)
model5 <- lm(y ~ tc, data = data)
model6 <- lm(y ~ ldl, data = data)
model7 <- lm(y ~ hdl, data = data)
model8 <- lm(y ~ tch, data = data)
model9 <- lm(y ~ ltg, data = data)
model10 <- lm(y ~ glu, data = data)

summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared #MAX
summary(model4)$r.squared
summary(model5)$r.squared
summary(model6)$r.squared
summary(model7)$r.squared
summary(model8)$r.squared
summary(model9)$r.squared
summary(model10)$r.squared


# ITEM 3
model.3b <- lm(y~bmi + glu + ltg + age, data = data)
summary(model.3b)$sigma
model.3b.restringido <- lm(y~bmi + ltg, data = data)

anova(model.3b, model.3b.restringido)


# ITEM 3C
model.3c <- lm(y~bmi  + ltg + age + fem*bp, data = data)
summary(model.3c)
confint(model.3c)

model.3c.restringido <-  lm(y~bmi  + ltg + age + fem + bp, data = data)

anova(model.3c, model.3c.restringido)


predict(model.3c, newdata = data.frame(bmi = 22.4, bp = 71, ltg = 4.5, fem = 0, age = 50))



