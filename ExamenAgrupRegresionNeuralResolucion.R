#leo csv
combu<-read.csv("combustibles.csv",stringsAsFactors = TRUE)
set.seed(35971187)

# 1-
#   a. Realizar un agrupamiento jerárquico de 6 grupos sin usar la columna Combustible.


#hago una primera lectura de los datos para entender el contenido con summary y head
summary(combu)
head(combu)

# Saco columna Combustible, escalo las numericas y calculo distincia con dist() 
combu2 <- combu[,-1]
summary(combu2)

distCombu <- dist(scale(combu2))
summary(distCombu)
head(distCombu)

#Aplico hclust
hc <- hclust(distCombu)

#Grafico
plot(hc)

#Agrupo en cuadradantes
#muestra las agrupaciones en la viz
rect.hclust(hc,k=6, border=3:10)

#Agrego columna grupo
combu2 <- cbind(combu, Grupo=cutree(hc,k=6))
head(combu2)


# b. Realizar un gráfico con eje x=Respuesta. eje y=Ruido. color=agrupamiento 
# según punto a.


#Grafico con ggplot  
#(le agrego también shape con Combustible para una mejor identificación)
library(ggplot2)
summary(combu2)
ggplot(combu2,aes(x=Respuesta,y=Ruido,col=as.factor(Grupo),shape=as.factor(Combustible))) + geom_point()


# c. Identificar cuales grupos pertenecerían a cada uno de los combustibles. 
# Cual es la proporción de vehículos que quedarían agrupados erróneamente?


#Matriz de confusion para ver tipo de combustible por grupo:
table(combu2$Combustible,combu2$Grupo)

#         1  2  3  4  5  6
# Diesel  0  0 11  7  4  6
# Gas     0  0  4 10 23  0
# Nafta  14 21  0  0  0  0

# 1c. Analizando el gráfico y la matriz puedo decir que:

# Los grupos más claros son el 1 y el 2, que pertenecen al combustible Nafta
# Quedaron bien agrupados y sin error
# Luego los grupos 3,4,5 y 6 son más confusos porque quedaron más mezclados el Diesel y el Gas
# El gas quedó principalmente agrupado en el 4 y el 5 (mayor presencia en el 5)
# Pero Diesel tiene presencia en los 4 grupos (mayor presencia en 3 y 4)
#
# Proporcion de agrupamiento erronea:
# 14 de Gas (grupo 3 y 4) y 10 de Diesel (grupo 5 y 6)= 24/100
# El 24%

############################################################################################################

#   2- Utilizando el 75% de los datos para entrenamiento:
#   a. Realizar una regresión lineal sin usar la columna combustible 
#   para estimar la velocidad máxima.
 

combu3<-read.csv("combustibles.csv",stringsAsFactors = TRUE)
set.seed(35971187)
summary(combu3)
head(combu3)

#Saco columna Combustible
combu4 <- combu3[,-1]
summary(combu4)

#hago muestra del 75%
s<-sample(1:100,75)
#Tomo los 75 como Train
c4_train<-combu4[s,]
#Tomo el resto como Test
c4_test<-combu4[-s,]


#Aplico funcion lm() con parametros numericos
modelolm<-lm(formula = Velocidad.maxima ~ Ruido  + Respuesta + Volumen, data = c4_train)

#Chequeo coeficientes
modelolm
# Coefficients:
#   (Intercept)        Ruido    Respuesta      Volumen  
#     158.7181      -1.6660       0.5761       0.7708  

summary(modelolm)
#Chequeo R2: Adjusted R-squared:  0.2881

#Uso modelo para hacer prediccion sobre la muestra test
pred<-predict(modelolm,c4_test)

#Agrego una columna predVelMax para la velocidad que predice
c4_test$predVelMax<-pred
head(c4_test)

#     Ruido Respuesta Velocidad.maxima Volumen predVelMax
# 2   33.5      97.8            187.4     2.3   161.0210
# 6   33.1      75.6            221.5     1.7   148.4361
# 12  36.0      93.9            152.8     2.7   154.9178
# 16  41.4      65.3            141.6     3.5   130.0627
# 19  42.7      54.7            145.5     2.7   121.1740
# 27  36.4      67.8            148.7     1.5   138.2910



# b. Cuál sería la velocidad máxima de un vehículo con ruido, respuesta 
# y volumen promedios de todos los vehículos?

#Chequeo los valores promedio
summary(combu4)

#   Ruido         Respuesta      Velocidad.maxima    Volumen   
# Mean   :36.74   Mean   : 72.83   Mean   :141.5    Mean   :2.259


#Hago prediccion

predict(modelolm,data.frame(Ruido=36.74,Respuesta=72.83,Volumen=2.259))

#Velocidad.maxima estimada = 141.2073 

############################################################################################################

