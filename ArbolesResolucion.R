TP Arboles - Resolución

#Diana Fernandez

# CONSIGNA
# La base HMDA ("Home Mortgage Disclosure Act Data") 
# contiene datos sobre 2380 solicitudes de hipoteca y si fueron denegadas (deny=yes) o no (deny=no).
# 
# Se puede leer la especificación de la base en: https://rdrr.io/cran/AER/man/HMDA.html


#importo
#paso strings a factores
hmda <- read.csv("HMDA.csv",stringsAsFactors = TRUE)

head(hmda)

#elimino columna 1 porque no aporta valor, solo numera
hmda<-hmda[,-1]
head(hmda)

#- Discretizar las columnas que no sean factores (tener en cuenta que la función C5.0 rechaza valores que contengan el caracter ",")

#discretizo columunas numericas
library(arules)

hmda$chist<-as.factor(hmda$chist)
hmda$mhist<-as.factor(hmda$mhist)
hmda$pirat<-discretize(hmda$pirat,breaks = 4,labels =c("bajo","medio bajo","medio alto","alto"))
hmda$hirat<-discretize(hmda$hirat,breaks = 4,labels =c("bajo","medio bajo","medio alto","alto"))
hmda$lvrat<-discretize(hmda$lvrat,breaks = 4,labels =c("bajo","medio bajo","medio alto","alto"))


#Libreria C50
library(C50)

# - Armar un árbol de decisión para determinar si una hipoteca debería ser denegada 
# usando C50 o J48 con una muestra del 80% de los datos. Analizar su significado. 
# Qué es lo primero que deberíamos fijarnos para determinar si otorgar una hipoteca o no?
# - Evaluar el árbol con los datos no utilizados en la muestra y comparar con el valor real. 
# Armar su matriz de confusión y analizar.


#creo muestra del 80% y antes establezco semilla
set.seed(35971187)
s<-sample(1:2380,1904)
h_train<-hmda[s,]
h_test<-hmda[-s,]

#Creo arbol
arbol<-C5.0(h_train[,names(h_train)!="deny"],h_train$deny)
arbol
summary(arbol)


# Decision tree:
#   
#   insurance = yes: yes (41/4)
# insurance = no:
#   :...phist = no: no (1729/135)
# phist = yes:
#   :...afam = yes: yes (44/18)
# afam = no:
#   :...pirat = medio alto: no (37/4)
# pirat in {bajo,medio bajo,alto}:
#   :...single = no: no (35/11)
# single = yes: yes (18/6)
# 
# 
# Evaluation on training data (1904 cases):
#   
#   Decision Tree   
# ----------------  
#   Size      Errors  
# 
#     6  178( 9.3%)   <<
#   
#   
#   (a)   (b)    <-classified as
# ----  ----
#   1651    28    (a): class no
#    150    75    (b): class yes
# 
# 
# Attribute usage:
#   
# 100.00%	insurance
# 97.85%	phist
# 7.04%	afam
# 4.73%	pirat
# 2.78%	single

##
#lo primero a mirar para determinar si otorgar una hipoteca o no
# es el insurance


