#examen
#María Fernanda Monterroso Colindres
#999003305

#pregunta 11
Ho > o igual a 18
Hi < a 18

c11 <- 0.95
sd11 <- 4
n11 <- 15
mean11 <- 173.47
alfa11 <- 0.05/2

#normal ajustada
normal11 <- qnorm(1-alfa11, 0, 1, lower.tail = TRUE)

#error
error11<-  sd11/sqrt(n11)

#margen
margen11 <- normal11*error11
margen
#límites

limInf<- mean11 - margen11
limsup <- mean11 + margen11

#pregunta 12

Ho > o igual a 18
Hi < a 18


c12 <- 0.95
sd12 <- 4
n12 <- 15
mean12 <- 173.47
alfa12<- 0.05/2

#normal ajustada
normal12 <- qnorm(1-alfa12, 0, 1, lower.tail = TRUE)

#error
error12<-  sd12/sqrt(n12)

#margen
margen12 <- normal12*error12
margen


#límites

limInf<- mean12 - margen12
limsup <- mean12 + margen12

#ejercicio 13

Ho > o igual a 18
Hi < a 18

c13 <- 0.95
sd13 <- 4
n13 <- 15
mean13 <- 173.47
alfa13<- 0.20/2

#normal
normal13 <- qnorm(1-alfa13, 0, 1, lower.tail = TRUE)

#pregunta 14 respuesta dada por la ingeniera

#pregunta 15
Ho > o igual a 18
Hi < a 18

c15 <- 0.95
sd15 <- 4
n15 <- 15
mean15 <- 173.47
alfa15<- 0.05/2

#normal ajustada
normal15 <- qnorm(1-alfa15, 0, 1, lower.tail = TRUE)

#error
error15<-  sd15/sqrt(n15)

#margen
margen15 <- normal15*error15
margen

alfa15.1<- 0.2/2

#normal ajustada
normal15.1 <- qnorm(1-alfa15.1, 0, 1, lower.tail = TRUE)

#error
error15.1<-  sd15/sqrt(n15)

#margen
margen15.1 <- normal15.1*error15.1

#pregunta 16

#H0: mu>= 800
#Hi mu < 800

sd16 <- 120
n16 <- 50
mean16 <- 750
alfa16 <- 0.01
mu16 <- 800

#estadístico de prueba
z16<- (mean16-mu16)/(sd16/sqrt(n16))

#valor de la distribución
zAlfa16<- qnorm(alfa16,0,1, lower.tail = TRUE)
#validar la hipótesis
z16 <  zAlfa16

#pregunta 20

Ho s < igual 4
Hi s > 4

sigma20 <- 4
alfa20 <- 0.05
s20 <- 4.9
n20 <-24 

#estadístico de prueba
X20 <- ((n20-1)*s20)/sigma20

#distribución
chi_sd20<- qchisq(1-alfa20, n20-1)

#validar la hipótesis
X20 > chi_sd20

#pregunta 25
data("trees")
call("trees")
#correlación entrew volume y height
lm.trees <- lm(Volume~Height, data=trees)
summary(lm.trees)

#pregunta 26
#correlación entrew volume y girth
lm.trees <- lm(Girth~Volume, data=trees)
summary(lm.trees)

#pregunta 27
#regresión lineal
plot(trees$Girth~trees$Volume)
abline(lm.trees)
lm.trees2 <- lm(Girth~Volume, data=trees)
summary(lm.trees2)


#fórmula Y=mX+b
#girth=0.1846(volume)+7.67



