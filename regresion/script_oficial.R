#SCRIPT OFICIAL
#Operaciones de matrices
A%*%B #
t(A) #
solve(A) #

#Graficos de ejemplo
X1<-0:20
X2<-sample(7:9,21,rep=TRUE)
Y<-sample(0:20)
#Grafico 1
plot(X1,X2,col="red")
lines(X1,X2,col="green")
#Grafico 2
plot(X1,Y,type="l")
points(X1,Y,col=factor(X2),pch=16)
abline(20,-2)
#Interaccion
interaction.plot(categ1,categ2,resp) #

#Importado y tratamiento de datos
datos <-read.table(file.choose(),header = TRUE) #
datos_clean <-na.omit(datos) #
datos_2 <-data.frame(X1,X2,Y)
datos_2$X2<-factor(datos_2$X2)
cut(Y, breaks = c(-Inf, 5, 15, Inf), 
    labels = c(1,2,3), 
    right = FALSE)

#Ajuste de modelos
ajuste<-lm(Y~X1,data=datos_2)
ajuste_2<-lm(Y~1,data=datos_2)
ajuste_completo<-lm(Y~.,data=datos_2)
ajuste_3<-lm(Y~X1*X2,data=datos_2)
ajuste_x<-lm(Y~.,data=datos_2,x=TRUE)
anova(ajuste,ajuste_completo) 

#Parametros de la regresion
ajuste_x$x
ajuste$coefficients
confint(ajuste,level=0.95)
summary(ajuste)$sigma
vcov(ajuste)
ajuste$fitted.values
ajuste$residuals
nuevos <- data.frame(X1=c(21,22,23))
predict(ajuste,newdata=nuevos)
