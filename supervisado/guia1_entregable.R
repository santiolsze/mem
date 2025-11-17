########################## EJERCICIO 9  ########################################
# DGP: Y = 1 + 0.5 × X**4 + ε; X ∼ U(−1, 1) y ε ∼ N (0, σ2).
# Tres modelos
#f1(X) = β0
#f2(X) = β0 + β1X + β2X2 + β3X3 + β4X4
#f3(X) = β0 + β4X4

#### Item a:  E(Y|X)
# E(Y|X=x) = E(1 + 0.5*X**4 | X=x) + E(ε |X=x) = =1+0.5x**4

####  Item b: Ordenarlos del menos al más flexible.
# f1 (p = 1) < f3 (p = 2) < f3 (p = 5)

#### Item c: Sesgo de los estimadores
# Depende de cómo se estimen, pero solo f2 y f3 tienen "chances" de ser insesgados.
# Si son LM, por OLS lo serán.

#### Item d: Definir en R una funci´on que tome como argumentos n y σ, y devuelva un data.frame con n observaciones generadas por el proceso.
dgp.teor <- function(x){
  return(1 + 0.5 * (x**4))
}
dgp <- function(n, sigma){
  x <- runif(n = n, -1, 1)
  e <- rnorm(n = n, sd = sigma)
  return(
    data.frame(x = x, sd = as.factor(sigma), error = e) %>% mutate(y = dgp.teor(x) + e)
  )
}

# Generar muestras con n = 20 y σ = 0.01, 0.2 y 1
n = 50
sd1 <- 0.01
sd2 <- 0.2
sd3 <- 1
data <- rbind(dgp(n, sd1),
      dgp(n, sd2),
      dgp(n, sd3))

# ¿Con cu´al de las tres muestras ser´a m´as f´acil aprender la verdadera funci´on f?
# Claramente, con la de menor desvío estandar. El error irreducible de estimación es sigma**2,
# que en magnitud puede incluso superar al irreducible.
ggplot(data, aes(x = x, y=y, colour = sd)) + geom_point() + 
  geom_function(fun = ~dgp.teor(.x), color = "black") + facet_wrap(facets = ~sd, ncol = 1) +ylim(0,2)

#### Item e: Ajustar y graficar los tres modelos param´etricos. Incluir en cada gráfico la verdadera función f(x).
f1 <- as.formula(y ~ 1)
modelo.f1.sd1 <- lm(f1, data = data %>% filter(sd == sd1)) %>% predict(newdata = data)
modelo.f1.sd2 <- lm(f1, data = data %>% filter(sd == sd2))  %>% predict(newdata = data)
modelo.f1.sd3 <- lm(f1, data = data %>% filter(sd == sd3)) %>% predict(newdata = data)

f2 <- as.formula(y ~ x + I(x^2) + I(x^3) + I(x^4))
modelo.f2.sd1 <- lm(f2, data = data %>% filter(sd == sd1)) %>% predict(newdata = data)
modelo.f2.sd2 <- lm(f2, data = data %>% filter(sd == sd2))  %>% predict(newdata = data)
modelo.f2.sd3 <- lm(f2, data = data %>% filter(sd == sd3)) %>% predict(newdata = data)

f3 <- as.formula(y ~ I(x^4))
modelo.f3.sd1 <- lm(f3, data = data %>% filter(sd == sd1)) %>% predict(newdata = data)
modelo.f3.sd2 <- lm(f3, data = data %>% filter(sd == sd2))  %>% predict(newdata = data)
modelo.f3.sd3 <- lm(f3, data = data %>% filter(sd == sd3)) %>% predict(newdata = data)


data <- data %>% mutate(f1.preds = case_when(sd == sd1 ~ modelo.f1.sd1,
                                     sd == sd2 ~ modelo.f1.sd2,
                                     sd == sd3 ~ modelo.f1.sd3,
                                     T ~ NA),
                f2.preds =case_when(sd == sd1 ~ modelo.f2.sd1,
                                    sd == sd2 ~ modelo.f2.sd2,
                                    sd == sd3 ~ modelo.f2.sd3,
                                    T ~ NA),
                f3.preds =case_when(sd == sd1 ~ modelo.f3.sd1,
                                    sd == sd2 ~ modelo.f3.sd2,
                                    sd == sd3 ~ modelo.f3.sd3,
                                    T ~ NA))



ggplot(data %>%
         pivot_longer(cols = starts_with("f"), 
                      names_to = "model", 
                      values_to = "pred"), aes(x = x, y = y)) +
  geom_point(shape = 1, color = "black", alpha = 0.7) + 
  geom_function(fun = ~dgp.teor(.x), aes(color = "True DGP"), size = 1) +
  geom_line(aes(y = pred, color = model), size = 1) +
  facet_wrap(~sd, ncol = 3) +
  ylim(0, 2) +
  scale_color_manual(
    values = c("f1.preds" = "#E41A1C", 
               "f2.preds" = "#377EB8", 
               "f3.preds" = "#4DAF4A", 
               "DGP" = "darkgray"),
    labels = c("F1", "F2", "F3", "DGP")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  ) +
  labs(x = "X", y = "Y")

#### Item f y g: Simulación nueva; repetir 2000 veces.
Nrep <- 2000
res <- data.frame()
for (i in 1:Nrep){
train <- dgp(n = 20, sigma = 0.5)
test <- dgp(n = 1000, sigma = 0.5)

m1 <- lm(f1, data =train)
m2 <- lm(f2, data = train)
m3 <- lm(f3, data = train)


test.pred<-test %>% mutate(f1.pred = predict(m1, newdata = .),
                f2.pred = predict(m2, newdata = .),
                f3.pred = predict(m3, newdata = .)) 


ggplot(test.pred %>%
         pivot_longer(cols = starts_with("f"), 
                      names_to = "model", 
                      values_to = "pred"), aes(x = x, y = y)) +
  geom_point(shape = 1, color = "black", alpha = 0.7) + 
  geom_function(fun = ~dgp.teor(.x), aes(color = "True DGP"), size = 1) +
  geom_line(aes(y = pred, color = model), size = 1) +
  facet_wrap(~sd, ncol = 3) +
  ylim(0, 2) +
  scale_color_manual(
    values = c("f1.pred" = "#E41A1C", 
               "f2.pred" = "#377EB8", 
               "f3.pred" = "#4DAF4A", 
               "DGP" = "darkgray"),
    labels = c("F1", "F2", "F3", "DGP")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    strip.background = element_rect(fill = "lightgray", color = NA),
    strip.text = element_text(face = "bold")
  ) +
  labs(x = "X", y = "Y")



r <- test.pred %>% mutate(f1.res = y - f1.pred,
                     f2.res = y - f2.pred,
                     f3.res = y - f3.pred) %>% summarise(
                       f1.mse = sum(f1.res^2) / n(),
                       f2.mse = sum(f2.res^2) / n(),
                       f3.mse = sum(f3.res^2) / n(),
                       f1.res_mean = mean(f1.res),
                       f2.res_mean = mean(f2.res),
                       f3.res_mean = mean(f3.res),
                       sim = i
                     )

res <- rbind(res, r)
}
colMeans(res)
