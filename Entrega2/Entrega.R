getwd()
setwd("C:/Users/Alberto/Desktop/Computacional/Entrega2")

CuatroAses<-function(Mostrar=F, Maximo=1000){
  Extracciones=0
  Resultado=1:Maximo
  Ases=c(0,0,0,0)
  
  repeat{
    if(Maximo<=Extracciones){
      if(Mostrar)
        cat("No he podido obtener 4 ases en ", Extracciones, "extracciones. \n")
      
      return(list(E=NA, R=Resultado, Conseguido=F))
    }
    Extracciones = Extracciones+1
    SacoUna = sample(52,1)
    Resultado[Extracciones]=SacoUna
    
    if(SacoUna%%13!=1)
      next #módulo
    
    Ases[(SacoUna-1)%/%13+1]=1 #div. entera
    if(sum(Ases)==4)
      break
  }
  length(Resultado)=Extracciones
  if(Mostrar)
    cat("He necesitado ", Extracciones, " extracciones para obtener cuatro ases.\n")
  
  return(list(E = Extracciones , R=Resultado, Conseguido=T))
}

DistriAses = function(n=5, Maximo=1000){
  Saco = vector(length=n)
  for(i in 1:n)
    Saco[i] = CuatroAses(F,Maximo)$E
  return(Saco)
}

Distribucion<-DistriAses(10 ,200)
Distribucion<-DistriAses(15 ,200)

n<-10
estudio<-vector(length=6)
for(i in 1:6){
  estudio[i]<-summary(DistriAses(n,200))[7]/n
  n<-n*10
}

CuatroAses_sin = function(Mostrar=F){
  Ases=0
  Resultado=sample(52)
  for(i in 1:52){
    if(Resultado[i]%%13!=1)
      next
    Ases=Ases+1
    if(Ases==4)
      break
    if(Mostrar)
      cat("He necesitado", i, " extracciones para obtener cuatro ases\n")
    
    return(list(E=i, R=Resultado[1:i]))
  }
}

DistriAses_sin = function(n=5){
  Saco=vector(length=n)
  for(i in 1:n)
    Saco[i]=CuatroAses_sin()$E
  return (Saco)
}

exp<-DistriAses_sin(30)
summary(exp)
summary(exp)[5]

help(fivenum)
fivenum(exp)

install.packages("Hmisc")
library(Hmisc)
help(describe)
describe(exp)

exp<-DistriAses_sin(10)
exp
range(exp)
median(exp)
mean(exp)

help(sapply)
sapply(exp, quantile)

install.packages("pastecs")
library(pastecs)
help(stat.desc)
stat.desc(exp)



# 1/6000 de prob. de salir canto
canto<-function(){
  v<-sample(6000,1)
  if(v==1)
    return(TRUE)
  return(FALSE)
}


Lanz_moneda<-function(n=10){
  cara=0
  cruz=0
  canto=0
  for(i in 1:n){
    if(canto()==TRUE)
      canto=canto+1
    else{
      random=sample(c(0,1), size=1)
      if(random==0)
        cara=cara+1
      else
        cruz=cruz+1
    }
  }
  return(c(cara, cruz, canto))
}

res_moneda=Lanz_moneda(10)

help(data.frame)
nombres<-c("Cruz", "Cara", "Canto")
hoja_experimentos<-data.frame(row_names=nombres, n_50=Lanz_moneda(50),
                 n_500=Lanz_moneda(500),n_5000=Lanz_moneda(5000))
hoja_experimento


barplot(Lanz_moneda(5), names.arg = c("cara","cruz","canto"), 
        main="Lanzamiendo de 5 monedas",
        ylab = "Número de apariciones",
        col = c("royalblue", "red","seagreen"))

barplot(Lanz_moneda(50), names.arg = c("cara","cruz","canto"), 
        main="Lanzamiendo de 50 monedas",
        ylab = "Número de apariciones",
        col = c("royalblue", "red","seagreen"))


barplot(Lanz_moneda(10000), names.arg = c("cara","cruz","canto"), 
        main="Lanzamiendo de 10000 monedas",
        ylab = "Número de apariciones",
        col = c("royalblue", "red","seagreen"))


barplot(Lanz_moneda(100000), names.arg = c("cara","cruz","canto"), 
        main="Lanzamiendo de 100000 monedas",
        ylab = "Número de apariciones",
        col = c("royalblue", "red","seagreen"))


lanz_dados=function(n){
  return(sample(c(1:6), replace=TRUE, size=n))
}

help(plot)
plot(lanz_dados(12), ylab="valor", xlab="tiradas", main="Experimento Dados", col="red", type="o")
lines(lanz_dados(12), type="o", pch=22, lty=2, col="blue")


exp_lanz=function(n,m){
  contador=0
  repeat{
    contador=contador+1
    if(sum(lanz_dados(n))==m)
      break
  }
  cat("Se necesitan ",contador," tiradas para obtener",m,"con",n," dados\n")
  return(contador)
}

tiradas<-vector(length=6)
for(i in 1:6){
  tiradas[i]<-exp_lanz(i,i*6)
}
n<-c(1:6)
m<-n*6
hoja<-data.frame(tiradas,n,m)
hoja

plot(hoja, main="Experimento dados", col="red")
plot(hoja[2:1], main="Tiradas según n", col="red")

anterior<-getwd()
setwd("C:/Users/Alberto/Desktop/Computacional/Entrega")
datos<-read.csv("datos.txt", header=TRUE, sep=",")
datos

head(datos,n=3)
datos[1:3,]

datos[datos$Sexo=="Mujer",]

datos[datos$Sexo=="Mujer"&datos$Altura>175,]

plot(datos)

library(dplyr)
datos_mod<-select(datos, Peso, Altura, Edad)
plot(datos_mod, main="Experimento modificado")


install.packages("ggthemes")
library(ggthemes)
library(ggplot2)

ggplot(data=datos, aes(x=Edad, y=Peso))+geom_point()+theme_stata()+ggtitle("Relacion Edad y Peso")
ggplot(data=datos, aes(x=Altura, y=Edad))+geom_point()+theme_stata()+ggtitle("Relacion Altura y Edad")

install.packages("dummies")
library(dummies)
datos.new<-dummy.data.frame(datos, sep=" ")
heatmap(as.matrix(datos.new[,1:5]))



datos2<-read.csv("Datos2.txt", header=TRUE, sep=" ")
library(dplyr)
datos2_mod<-select(datos2, Peso, Altura, Edad)
datos2_mod2<-subset(Datos2, select=c(Peso,Altura, Edad))
identical(datos2_mod, datos2_mod2)
datos2_mod

ggplot(data=datos2, aes(x=Edad, y=Peso))+geom_point()+theme_stata()+ggtitle("Relacion Edad y Peso (Datos2.txt)")
ggplot(data=datos2, aes(x=Altura, y=Edad))+geom_point()+theme_stata()+ggtitle("Relacion Altura y Edad (Datos2.txt)")

barplot(datos$Peso, main="Peso", col=c("yellow","red"), names.arg=c("Muestras"), density=50, border="black", ylab="Kilogramos")
hist(datos$Peso, main="Peso", col=c("black","yellow"), xlab="Kilogramos", ylab="Frecuencia", density=120)