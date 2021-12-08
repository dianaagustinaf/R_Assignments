TP Clustering 
# agrupamiento jerárquico

#importo
vinos <- read.csv("vinos.csv")
head(vinos)
summary(vinos)

#Realizar un agrupamiento en 3 grupos - no utilizar la columna bodega:
#Saco Bodega, escalo, y calculo distincia con dist()
v2 <- vinos[,-1]
summary(v2)
distV2 <- dist(scale(v2[,c("flavonoides","color_int")]))

#Aplico hclust
hc <- hclust(distV2)

#Grafico
plot(hc)

#Agrupo en 3 cuadradantes y muestro las agrupaciones en la viz
rect.hclust(hc,k=3, border=3:10)


#Agrego columna grupo
v2 <- cbind(vinos, grupo=cutree(hc,k=3))

#Matriz de confusion
table(v2$bodega,v2$grupo)

#     1  2  3
# 1  58  1  0
# 2  18 53  0
# 3   0 20 28

#Grafico con ggplot
library(ggplot2)
ggplot(v2,aes(x=flavonoides,y=color_int,col=as.factor(grupo),shape=as.factor(bodega))) + geom_point()


# El agrupamiento clasifico como Bodega3 a 28, cuando en realidad eran 48 
# un 42% de error

# Clasifico como Bodega2 a 74, cuando en realidad eran 71
# el error parece minimo, pero 20 de esos 74 en realidad eran los que pertenecian a B3
# y le falto tomar 18, que se los asigno erroneamente a B1

# Y en B1 clasifico 58+18
# siendo esos 18 como decia antes pertenecientes a B2

# No resulta un buen predictor


###############################################################

#kmeans

#importo y saco bodega
vinos <- read.csv("vinos.csv")
v3 <- vinos[,-1]

#seed para la muestra, escalo y agrego centroide 
set.seed(35971187)
sample(1:178,3)

centros<-scale(v3[,c("flavonoides","color_int")])[c(94,165,63),]

km <- kmeans(scale(v3[,c("flavonoides","color_int")]), centers=centros)

#Grafico
plot(x=scale(v3$flavonoides),y=scale(v3$color_int))

#Agrego los centroides
points(centros, pch=3, col=2:6, cex = 5)

#Ubico los clusters
points(scale(v3[,c("flavonoides","color_int")]),pch=1,col=(km$cluster+1), cex=1)

#Ubico los nuevos centros
points(km$centers, pch=2, col=2:6, cex = 4)

# Comparacion jeraquico - kmeans
table(cutree(hc, k=3), km$cluster )

#    1  2  3
# 1 21  0 55
# 2 58 16  0
# 3  0 28  0


# este agrupamiento clasifico 79 como B1, cuando en realidad eran 76
# (pero de estos 79, en realidad 58 pertenecen a B2)
# clasifico 44 como B2, cuando en realidad eran 74
# clasifico 55 como B3, cuando en realidad eran 28 
# (y estos 55 agrupados en realidad pertenecian a B1)
# No resulta un buen predictor


###############################################################

