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


