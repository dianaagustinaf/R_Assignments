# Generador de lista de supermercado para posterior analisis:

set.seed(123)

otrosArticulos <- c("Mandarina","Agua","Fideos","Yerba","Detergente")
lmenor<-list()
i<-1
while (i <= 1000) {
  findeLargo <- runif(1) < 0.5
  if (findeLargo) {
    invita=runif(1) < 0.4
  } else {
    invita=runif(1) < 0.1
  }
  comeCarne=runif(1) < 0.7
  if (comeCarne) {
    if (invita) {
      asado=runif(1) < 0.6
    } else {
      asado=runif(1) < 0.15
    }
  } else {
    asado=FALSE
  }
  if (asado) {
    compraCarne=TRUE
    compraCarbon=runif(1) < 0.7
  } else if (comeCarne) {
    compraCarne=runif(1) < 0.5
    compraCarbon=runif(1) < 0.15
  } else {
    compraCarne=runif(1) < 0.2
    compraCarbon=runif(1) < 0.05
  }
  if (compraCarne) {
    compraVino=runif(1) < 0.4
  } else {
    compraVino=runif(1) < 0.2
  }
  res<-c()
  if (findeLargo) {
    res<-c(res,"FindeLargo")
  }
  if (compraCarne) {
    res<-c(res,"Carne")
  }
  if (compraCarbon) {
    res<-c(res,"Carbon")
  }
  if (compraVino) {
    res<-c(res,"Vino")
  }
  cantOtros=round(runif(1)*3)
  res<-c(res,sample(otrosArticulos,cantOtros,replace=FALSE))
  lmenor[[i]] <- res
  i <- i + 1
}


######################################################################
######################################################################

# Diana Fernandez 

# script R 1000 listas de compras de un supermercado 

#install.packages("arules")
#install.packages("arulesViz")
library(arules)
library(arulesViz)

ap1 <- apriori(lmenor,parameter = list(support=2/100, conf = 0.5))
inspect(ap1)

# me devuelve 89 reglas que permite ver asociaciones entre 3/4 items
# por ejemplo
#   [87] {Agua,Carbon,FindeLargo,Vino}  => {Carne}      0.021   0.9545455  0.022    1.8534863  21
#   [63] {Carbon,FindeLargo,Vino}       => {Carne}      0.048   0.9411765  0.051    1.8275271  48
# = dos posibles combinaciones con mas de 3 items en la que el cliente ademas compra carne 
# ambos casos con alta confianza y alto lift


# sin embargo, aumentando el soporte al 20% solo se obtienen 5 reglas

ap2 <- apriori(lmenor,parameter = list(support=20/100, conf = 0.5))
inspect(ap2)

# eliminando las opciones con lift menor a 1, queda
#   [3] {Vino}       => {Carne}      0.204   0.6891892  0.296    1.3382314 204
# un 20% de posibilidades que quien compre vino tambien compre carne


ltxs <- as(lmenor,"transactions")
ltxs
# (convierto a transaccion (antes tambien funciono))
# pruebo promedio entre ambas con soporte del 10% 

ap3 <- apriori(ltxs,parameter = list(support=10/100, conf = 0.4))
inspect(ap3)

# devuelve 21 reglas 

plot(ap3,engine="plotly")

# analizando segun este grafico, las mejores 3 opciones, teniendo en cuenta lift y conf son:
  
#   [4]  {Carbon}           => {Carne}      0.170   0.7657658  0.222    1.4869238 170 
#   [14] {Vino}             => {Carne}      0.204   0.6891892  0.296    1.3382314 204 
#   [19] {FindeLargo,Vino}  => {Carne}      0.110   0.7189542  0.153    1.3960277 110
