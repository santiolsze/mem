###################
### ENTREGA 5.1 ###
###################
nivel <- 0.92
datos_a <- scan()
n_a <- length(datos_a)
datos_b <- scan()
n_b <- length(datos_b)


sp.2 <- (var(datos_a)*(n_a-1) + var(datos_b)*(n_b-1))/(n_a + n_b - 2)

t_left <- qt((1-nivel)/2, lower.tail = T, df = n_a + n_b - 2)
t_right <-qt((1-nivel)/2, lower.tail = F, df = n_a + n_b - 2)

pivot_denom <- sqrt(sp.2 * (1/n_a + 1/n_b))

t_left*pivot_denom - mean(datos_a) + mean(datos_b)
t_right*pivot_denom - mean(datos_a) + mean(datos_b)
###################
### ENTREGA 5.2 ###
###################
sum_xi <- 54
n <- 117
p.obs <- sum_xi/n
nivel <- 0.99
long_max <- 0.07

z <- abs(qnorm((1-nivel)/2))
# TCL
Re(polyroot(c((n*p.obs**2),-(2*n*p.obs + z**2),(n+z**2))))

prop.test(sum_xi, n, conf.level = nivel, correct = F)


# Slutsky


p.obs.worse <- 0.5

ceiling((2*sqrt(p.obs.worse*(1-p.obs.worse))*z/long_max)**2)

###################
### ENTREGA 5.3 ###
###################
estimar_p_intervalo <- function(nivel, datos){
  p.obs <- mean(datos)
  n <- length(datos)
  z <- qnorm((1-nivel)/2)
  left <- p.obs - z*sqrt(p.obs*(1-p.obs)/n)
  right <- p.obs + z*sqrt(p.obs*(1-p.obs)/n)
  return(c(left,right))
}

estimar_p_intervalo_alt <- function(nivel, datos){
  sum_xi.obs <- sum(datos)
  n <- length(datos)
  prp.out <- prop.test(x=sum_xi.obs, n=n, conf.level=nivel, correct=F)
  right <- p.obs + z*sqrt(p.obs*(1-p.obs)/n)
  return(c(prp.out$conf.int[1],prp.out$conf.int[2]))
}


hallar_n_p <- function(nivel, L){
  z <- qnorm((1-nivel)/2)
  return(ceiling((2*sqrt(0.5*(1-0.5))*z/L)**2))
}

calcular_cubrimiento_longitud_empirico <- function(p, nivel, n, Nrep, seed, metodo = estimar_p_intervalo){
  set.seed(seed)
  cubre <- 0
  longs <- numeric(Nrep)
  for (x in 1:Nrep){
  datos <- rbinom(n = n, size = 1, prob = p)
  ci <- metodo(nivel, datos)
  cubre <- cubre + (p > min(ci) & p<max(ci))
  longs[x] <- max(ci)-min(ci)
  }
  c(cubre/Nrep, mean(longs))
  }

# Función principal para armar las dos tablas
generar_tablas_cubrimiento_longitud <- function(p_vals, n_vals, nivel = 0.95, Nrep = 1000, seed = 5,
                                                metodo = estimar_p_intervalo_alt) {
  cobertura_mat <- matrix(NA, nrow = length(p_vals), ncol = length(n_vals))
  longitud_mat <- matrix(NA, nrow = length(p_vals), ncol = length(n_vals))
  
  rownames(cobertura_mat) <- paste0("p=", p_vals)
  colnames(cobertura_mat) <- paste0("n=", n_vals)
  rownames(longitud_mat) <- paste0("p=", p_vals)
  colnames(longitud_mat) <- paste0("n=", n_vals)
  
  for (i in seq_along(p_vals)) {
    for (j in seq_along(n_vals)) {
      resultado <- calcular_cubrimiento_longitud_empirico(
        p = p_vals[i], nivel = nivel, n = n_vals[j],
        Nrep = Nrep, seed = seed, metodo = metodo
      )
      cobertura_mat[i, j] <- resultado[1]
      longitud_mat[i, j] <- resultado[2]
    }
  }
  
  list(cobertura = cobertura_mat, longitud = longitud_mat)
}

combinar_tablas_cubrimiento_longitud <- function(cobertura_mat, longitud_mat, digits = 3) {
  tabla_combinada <- matrix("", nrow = nrow(cobertura_mat), ncol = ncol(cobertura_mat))
  for (i in seq_len(nrow(cobertura_mat))) {
    for (j in seq_len(ncol(cobertura_mat))) {
      cobertura <- round(cobertura_mat[i, j], digits)
      longitud <- round(longitud_mat[i, j], digits)
      tabla_combinada[i, j] <- paste0(cobertura, " (", longitud, ")")
    }
  }
  rownames(tabla_combinada) <- rownames(cobertura_mat)
  colnames(tabla_combinada) <- colnames(cobertura_mat)
  return(tabla_combinada)
}

tablas<- generar_tablas_cubrimiento_longitud(p_vals = c(0.1,0.5),
                                                  n_vals = c(5,10,30,50,100,1000),
                                                  nivel = 0.95, 
                                                  Nrep = 1000, 
                                                  seed = 5,
                                                  metodo = estimar_p_intervalo)

tablas_alt <- generar_tablas_cubrimiento_longitud(p_vals = c(0.1,0.5),
                                                n_vals = c(5,10,30,50,100,1000),
                                                nivel = 0.95, 
                                                Nrep = 1000, 
                                                seed = 5,
                                                metodo = estimar_p_intervalo_alt) 

t1 <- combinar_tablas_cubrimiento_longitud(tablas$cobertura, tablas$longitud)
t2 <- combinar_tablas_cubrimiento_longitud(tablas_alt$cobertura, tablas_alt$longitud)


t2


