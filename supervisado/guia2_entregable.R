library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(patchwork)
set.seed(081)


data <- read.csv("../../Downloads/Default.csv")
data <- data %>% mutate(default_num = case_when(default == 'No'~ 0, T ~ 1),
                        b_i_ratio = balance/income) %>% filter(balance < income)

cor(data$b_i_ratio, data$balance)

# Viz de datos disponibles

ggplot(data, aes(x = student, y =b_i_ratio, fill = default)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))


# Particionar data: me guardo un poquito, simplemente para ver cómo da luego.
trainIndex <- createDataPartition(data$default_num, p = .9, 
                                  list = FALSE, 
                                  times = 1)

train <- data[ trainIndex,]
test  <- data[-trainIndex,]

lr <- glm(default_num ~ student + balance, family = 'binomial', data = train)
lr2 <- glm(default_num ~ student*balance, family = 'binomial', data = train) # Peor en AIC
lr3 <- glm(default_num ~ student + I(balance^2), family = 'binomial', data = train) # Peor en AIC

train$fitted.values <- lr$fitted.values

b0 <- lr$coefficients["(Intercept)"]
b1 <- lr$coefficients["studentYes"]
b2 <- lr$coefficients["balance"]

p_i <- function(model, newdata){
  predict(model, newdata, "response")
}

G_i <- function(model, newdata){
  newdata %>% mutate( probs = p_i(model, newdata),
                      gains = 0.15*balance*(1-probs) - balance*(probs))
 }

balances <- seq(quantile(data$balance,probs = 0.05), quantile(data$balance, probs = 0.95), by = 50)
newdata <- rbind(tibble(student = "Yes", balance = balances),
                 tibble(student = "No", balance = balances))
gain_students <- G_i(lr, newdata)
max_gain_df <- gain_students %>%
  group_by(student) %>%
  slice_max(order_by = gains, n = 1, with_ties = FALSE)
# Plot 1: probabilities
p1 <- ggplot(gain_students, aes(x = balance, y = probs, colour = student)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_vline(data = max_gain_df, aes(xintercept = balance, colour = student),
             linewidth = .25) +
  theme_minimal()

# Plot 2: gains
p2 <- ggplot(gain_students, aes(x = balance, y = gains, colour = student)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed") +
  geom_vline(data = max_gain_df, aes(xintercept = balance, colour = student),
             linewidth = .25) +
  theme_minimal()

# Combine plots
p1 / p2

# SI TOCA ENTREGAR:
# G = ganancia dada por el iésimo individuo
# D = default del iésimo individuo (1 = default)
# B = Balance del iésimo individuo.
# G|B = 0.15*B si D=0, -B si D = 1
# E(G|B) = 0.15*B*(1-p) - p*B
# Estimo p con logistica: logit(p) = bo + b1*income + b2*estudiante
# Ahí queda una fórmula que solo depende de income y estudiante.
# Hago grilla y búsco el balance para estudiantes (y para no estudiantes) que 
# maximiza la gananacia. Abría que asignarle ese blaance a todos los estudiantes.
# Si se mete el ingreso como continua, ahí sí daría _un balance personalizado_.
# Se puede evaluar si estamos prediciendo bien la probabilidad en el conjunto de validación.


################################################################################
################################################################################
################################################################################