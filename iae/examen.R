
# 1) ---------------------------------------
astro <- read.csv("../../Downloads/astro.csv")

plot(astro$BjMAG,astro$Rmag)

# Se observa cierto grado de correlación positiva entre ambas variables,
# asociándose valores más altos de una con valores más altos de la otra. 


# 2) ---------------------------------------
set.seed(1)
t.t.split <- sample(1:nrow(astro), nrow(astro)/2)
train <- astro[t.t.split,]
test <- astro[-t.t.split,]

# 3) ---------------------------------------

## a) ---------------------------------------

x <- train$BjMAG
y <- train$Rmag
x_nueva <- -17.65
h <- 1.2

y_1 <- mean(y[x >= (x_nueva - h/2) & x <= (x_nueva + h/2)])
y_2 <- mean(
  y[
    ((x >= (x_nueva - h)) & (x < (x_nueva -h/2))) | 
 (x > (x_nueva + h/2) & x <= (x_nueva +h))]
)
pred <- (2/3)*y_1 + (1/3)*y_2
print(pred)
# [1] 24.40631
## b) ---------------------------------------
pred_PromPes_loc <- function(x, y, x_nueva, h){
  y_1 <- mean(y[x >= (x_nueva - h/2) & x <= (x_nueva + h/2)])
  y_2 <- mean(
    y[
      ((x >= (x_nueva - h)) & (x < (x_nueva -h/2))) | 
        (x > (x_nueva + h/2) & x <= (x_nueva +h))]
  )
  

    pred <- (2/3)*y_1 + (1/3)*y_2
    
  pred
}

print(pred_PromPes_loc(x = train$BjMAG,
                       y = train$Rmag,
                       x_nueva = -17.65,
                       h = 1.2))
# [1] 24.40631 (Validación de resultado)
## c) ---------------------------------------
xs <- seq(-22, -8, 0.5)
preds <- c()
for (x_nueva in xs){
  preds <- c(preds, pred_PromPes_loc(x = train$BjMAG, y = train$Rmag, x_nueva = x_nueva, h = 1.2)
 )
}

plot(train$BjMAG, train$Rmag)
lines(xs[!is.na(preds)], preds[!is.na(preds)], col = "darkblue", lwd =4)

# 4) ---------------------------------------
library(tree)
## a) ---------------------------------------
arbol <- tree(formula = Rmag ~ BjMAG, data = astro, subset = t.t.split, mindev = 0.005)
plot(arbol)
text(arbol, cex = .5)

summary(arbol) #Number of terminal nodes:  11

# El árbol tiene 11 hojas (nodos terminales)

## b) ---------------------------------------
set.seed(3)
cv_arbol <- cv.tree(arbol, K = 10)
cv_arbol

## c) ---------------------------------------
plot(cv_arbol$size, cv_arbol$dev, type = "l")
arbol_podado <- prune.tree(arbol, best = 2)

plot(arbol_podado)
text(arbol_podado, cex = .5)

## d) ---------------------------------------
predict(arbol_podado,data.frame(BjMAG = c(-17.65)))
# 24.39

# Graficamente también se aprecia.  

# 5) ---------------------------------------
prom_preds <- c()
for (x_nueva in test$BjMAG){
  prom_preds <- c(prom_preds, pred_PromPes_loc(x = train$BjMAG,
                   y = train$Rmag,
                   x_nueva = x_nueva,
                   h = 1.2))
}

test$pred_promedios <- prom_preds
test$pred_arbol_poda <- predict(arbol_podado, test)

MSE_test_prom <- sum((test$Rmag - test$pred_promedios)**2)  / nrow(test)
MSE_test_arbol_podado <- sum((test$Rmag - test$pred_arbol_poda)**2)  / nrow(test)

print(paste("MSE test promedios ponderados locales:", round(MSE_test_prom,3)))
print(paste("MSE test promedios arbol podado:", round(MSE_test_arbol_podado,3)))
# El método de promedios locales tiene menor MSE en datos no vistos, por lo que
# es el que tiene mejor desempeño predictivo.



