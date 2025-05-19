library(stringr)
library(dplyr)
df <- read.csv("../../Downloads/Libro1.xlsx - Hoja1.tsv",
               encoding="Latin-1",
               sep = "\t",
               stringsAsFactors = FALSE)

# Emprolijo el formato
df <- df %>% 
  select(-NOMBRE) %>% 
  mutate_all(funs(stringr::str_replace(., ",", "."))) %>% 
  mutate(across(everything(), as.numeric))

par(mfrow=c(1,4))
hist(df$ej1a, xlim = c(0, 0.5),nclass  = 10)
hist(df$ej1b, xlim = c(0, 0.5), nclass  = 10)
hist(df$ej1c, xlim = c(0, 0.5), nclass  = 10)
hist(df$ej1d, xlim = c(0, 0.5), nclass  = 10)

boxplot(df$ej1a,df$ej1b, df$ej1c, df$ej1d)

# Entrega 1.2
datos<- scan()
n <- length(datos)
# Estimar u 
mean(datos)

# var estimado
var_hat <- var(datos) * ((n - 1) / n)
var_hat

# var muestral estimado
S2_hat <- var(datos)
S2_hat
# Error estandar de u con var = 2
sqrt(2) / sqrt(n) 

# Sesgo de var 
-2/n
## Sesgo de var muestral
0

 # Error estandar de u con var = s2
 sqrt(sd(datos) **2) / sqrt(n) 
 
 
 # Entrega 3
 datos<- scan()
 2*mean(datos)
 max(datos)
 