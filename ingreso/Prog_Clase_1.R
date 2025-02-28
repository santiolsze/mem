############# Ejercicios
# 1.Se desea tener un programa que dada la variable grados, que representa la temperatura en grados Farenheit, calcule en otra variable el valor en Celsius.
#Celsius=59×(Farenheit−32)

t_farenheit <- 106
t_celsius <- 5/9*(t_farenheit - 32 )

print(paste("Fahrenheit:", t_farenheit, "Celsius:", t_celsius))

#2. Escribir otro programa que se comporte a la inversa, es decir, que dada una variable que represente la temperatura en Celsius, calcule su equivalente en Farenheit.

t_celsius <- 41
t_farenheit <- 9/5*(t_celsius) + 32

print(paste("Fahrenheit:", t_farenheit, "Celsius:", t_celsius))

#3. Escribir un conversor de kilometros a millas.
km_a_millas <- function(km){
  return(km / 1.609)
}

km_a_millas(100)

#4. Dado un cuadrado, que el largo de su base se encuentra guardado en una variable llamada base, calcular:
base <- 5
#a. El perímetro
perimetro <- base * 4
print(perimetro)

#b . El área
area <- base ** 2
print(area)

# 5. Idem anterior pero para un triangulo equilatero.
perimetro <- base*3
area <- (base ** 2) / 2

#6 Asumiendo que los años tienen siempre 365 días, calcular:
edad <- 28  
# a. cuántos días vas a cumplir tu próximo cumpleaños,
# b. cuántas horas vas a haber vivido,
# c. y cuántos segundos.
dias <- edad * 365
horas <- dias * 24
segundos <- horas * 60 * 60

#7. Se tienen las notas de 3 materias en sus respectivas variables: matematica, lengua, dibujo. Calcular el promedio de dichas notas.
mate <- 5
lengua <- 6
dibujo <- 7

promedio <- (mate + lengua + dibujo) / 3

#8. Repetir el ítem anterior, pero ahora con los valores guardados en un vector llamado notas. Hint: se puede acceder a los elementos de un vector con []. Ej: c(4, 6, 8)[2] nos da el valor 6.
notas <-  c(mate, lengua, dibujo)
promedio <- mean(notas)

#9. Si en el ítem anterior no usaste la función lenght() y sum(), volvé a resolverlo usandolas.
promedio <- sum(notas) / length(notas)

# Si en el ítem anterior no usaste la función mean(), volvé a resolverlo usandola.
# No aplica

# 11. Definir dos variables, perro y gato donde tengan asignado un nombre. (Por ejemplo: “Sultán” y “Sr Botones”). Escribir un programa que intercambie los valores de ambas variables. Es decir que ahora perro tenga el nombre asignado al gato y gato el nombre asignado al perro.
perro <- "Sultán"
gato <- "Sr Botones"

ex_perro <- perro
perro <- gato
gato <- ex_perro

print(perro)
print(gato)

# 12- Calcular el índice de masa corporal (IMC) de una persona cuya altura es 1.78m y su peso es 80kg. # IMC=pesoaltura2
imc <- function(altura, peso){
  return(peso / altura**2)
}

imc(1.78, 80)

#13. Si tenemos los pesos y las alturas de 5 personas en 2 arreglos pesos y alturas. Realizar un programa que obtenga un arreglo con los IMC de cada una de las personas.
pesos <- c(60, 70, 80, 90, 100)
alturas <- c(1.65, 1.80, 1.70, 1.90, 1.67)
imcs <- imc(alturas, pesos)
print(imcs)

#14. Sobre el cálculo del ejercicio anterior, encontrar el valor máximo, el mínimo, el promedio y la mediana de los IMCs.
mean(imcs)
max(imcs)
min(imcs)
median(imcs)

