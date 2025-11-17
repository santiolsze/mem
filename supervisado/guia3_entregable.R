library(tibble)
library(dplyr)
library(ggplot2)
library(leaps)

set.seed(123)

media  = -4
desvio = 1
n = 100

X <- rnorm(n, media, desvio)
Xs <- as_tibble(sapply(1:10, function(i) X^i))
b0 = 20; b1 = 21; b2 = 22; b3 = 24; b7
errors <- runif(n, -desvio, desvio)
Y <- b0 + b1*X + b2*X^2 + b3*X^3 + errors
Y2 <-  

regsubsets.results <- function(data, method){
  
  models <- regsubsets(Y ~ ., data = data, method = method)
  sum.models <- summary(models)
  
  tibble(
    n      = 1:length(sum.models$bic),
    bic    = sum.models$bic,
    method = method
  )
}

res_exh <- regsubsets.results(cbind(Xs,Y), "exhaustive")
res_fwd <- regsubsets.results(cbind(Xs,Y), "forward")
res_seq <- regsubsets.results(cbind(Xs,Y), "backward")

res_all <- bind_rows(res_exh, res_fwd, res_seq)

ggplot(res_all, aes(x = n, y = bic, color = method)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Comparación de BIC según método de selección",
    x = "Número de predictores (n)",
    y = "BIC",
    color = "Método"
  ) +
  theme_minimal()

library(glmnet)
lasso <- cv.glmnet(as.matrix(Xs), Y, alpha = 1)

lasso.coef <- predict(lasso , type = "coefficients",
                      s = lasso$lambda.min)
lasso.coef

lasso.coef[lasso.coef != 0]