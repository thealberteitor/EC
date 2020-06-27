getwd()
setwd("C:/Users/Alberto/Desktop/Computacional/Entrega5")

#////////////////////////////////////////1

dibuja_recinto <- function(x0,xf, y0,yf){
        first = function(x){x^2}
        second = function(x){sqrt(100*x)}
        third= function(x){-sqrt(100*x)}
        plot(c(0,1), c(0,1), type="n", xlab="x", ylab="y", xlim=c(x0,xf), ylim=c(y0,yf))
        curve(first(x), col="goldenrod1", add=T, lwd=2)
        curve(second(x), col="indianred4", xlim=c(0,xf), add=T, lwd=2)
        curve(third(x), col="indianred4", xlim=c(0,xf), add=T, lwd=2)
        legend("topleft", inset=.02, legend=c("y=x^2", "y^2=100*x"),
                col=c("goldenrod1","indianred4", "indianred4"), lty=1, box.lty=0, cex=0.8)
}


dibuja_punto <- function(x,y, text){
        points(x, y, pch=20)
        if(text == TRUE)
                text(x+0.1, y, toString(c(x,y)),col="black",cex=0.8,adj=0)
}

dibuja_recinto(0,5, -25,25)
dibuja_punto(2,12, text=TRUE)


dibuja_recinto <- function(x0,xf, y0,yf){
        first = function(x){x^2}
        second = function(x){sqrt(100*x)}
        plot(c(0,1), c(0,1), type="n", xlab="x", ylab="y", xlim=c(x0,xf), ylim=c(y0,yf))
        curve(first(x), col="goldenrod1", add=T, lwd=2)
        curve(second(x), col="indianred4", xlim=c(0,xf), add=T, lwd=2)
        legend("topleft", inset=.02, legend=c("y=x^2", "sqrt(100*x)"),
                col=c("goldenrod1","indianred4"), lty=1, box.lty=0, cex=0.8)
}


dibuja_recinto(0,5, 0,25)
dibuja_punto(2,12, text=TRUE)



#////////////////////////////////////////2

posicion <- function(x,y){
        if (y>x^2 & y^2<100*x)
                es = "interior"
        else if (y<x^2 | y^2>100*x | x<0 | y<0)
                es = "exterior"
        else
                es = "frontera"
        return (es)
}

posicion(2,12)
posicion(0,0)	
posicion(-1,1)	
posicion(30,30)


#////////////////////////////////////////3

aniade <- function(x, y, a){
        A<-runif(1, min=-a, max=a)
        B<-rnorm(1, mean=0, sd=sqrt(a))

        return (c(x+A, y+B))
}

p0<-aniade(2, 12, 1/100)
p0



#////////////////////////////////////////4


lanz_monedas<-function(n){
        return (sample(c("cara","cruz"), size=n))
}

lanz_dados<-function(n){
        return(sample(c(1:6), replace=TRUE, size=n))
}

recorrido_aleat<-function(x,y, M){
        coord_x <- x
        coord_y <- y

        pasos <- 0

        for(i in 1:M){
                monedas <- lanz_monedas(2)
                dados <- lanz_dados(2)

                if(sum(dados)>5)
                        x=x+0.05
                else
                        x=x-0.05

                if(monedas[1]=="cara" & monedas[2]=="cara")
                        y=y+0.05
                else if(monedas[1]=="cruz" & monedas[2]=="cruz")
                        y=y
                else
                        y=y-0.05

                coord_x <- c(coord_x, x)
                coord_y <- c(coord_y, y)

                pasos <- pasos+1

                if(posicion(x,y)!="interior")
                        break;
        }
        if(pasos == M)
                pasos = NA
        return(list(CX=coord_x, CY=coord_y, P=pasos))
}


rec <- recorrido_aleat(p0[1],p0[2], 5)
rec

rec2 <- recorrido_aleat(p0[1],p0[2], 60)
rec2



#////////////////////////////////////////5

dibuja_simulacion <- function(total, tipo, color){
        if(tipo == "lines")
                lines(x=total$CX, y=total$CY, col=color, cex=0.6)
        else if(tipo == "points")
                points(x=total$CX, y=total$CY, pch=1, col=color, cex=1)
        else
                print("Introduzca un tipo de representación válida")
}

total <- recorrido_aleat(p0[1], p0[2], 10000)
dibuja_recinto(0,5, 0,25)
dibuja_simulacion(total, tipo="lines", color="black")


#////////////////////////////////////////6


dibuja_recinto_centrado <- function(x,y,c){
        first = function(x){x^2}
        second = function(x){sqrt(100*x)}
        plot(c(0,1), c(0,1), xlab="x", ylab="y", type="n", xlim=c(x-c,x+c), ylim=c(y-c,y+c))
        curve(first(x), col="goldenrod1", add=T, lwd=2)
        curve(second(x), col="indianred4", add=T, lwd=2)
        legend("topleft", inset=.02, legend=c("y=x^2", "sqrt(100*x)"),
                col=c("goldenrod1","indianred4"), lty=1, box.lty=0, cex=0.8)
}

dibuja_recinto_centrado(p0[1],p0[2], c=2)
dibuja_punto(p0[1], p0[2], text=FALSE)
dibuja_simulacion(total, tipo="lines", color="black")	


#////////////////////////////////////////7

dibuja_simulaciones <- function(juntas=FALSE, p0, colores, tipo, M){

        if(juntas==TRUE){
                dibuja_recinto_centrado(p0[1],p0[2], c=2)
                dibuja_punto(p0[1], p0[2], text=FALSE)

                for(i in 1:length(colores)){
                        rec<-recorrido_aleat(p0[1], p0[2], M)
                        dibuja_simulacion(rec, tipo, color=colores[i])
                }
        }
        else{
                col<-2
                fil<-ceiling(length(colores)/col)
                par(mfrow=c(fil,col))
                for(i in 1:(fil*col)){
                        dibuja_recinto_centrado(p0[1],p0[2], c=2)
                        dibuja_punto(p0[1], p0[2], text=FALSE)
                        rec<-recorrido_aleat(p0[1], p0[2], M)
                        dibuja_simulacion(rec, tipo, color=colores[i])
                }
        }
}

dibuja_recinto_centrado(p0[1],p0[2], c=2)
dibuja_punto(p0[1], p0[2], text=FALSE)
	
colores<-c("darkorange", "ivory4", "forestgreen", "maroon4")
dibuja_simulaciones(juntas=FALSE, p0, colores, "lines", 10000)

dibuja_recinto_centrado(p0[1],p0[2], c=2)
dibuja_punto(p0[1], p0[2], text=FALSE)
colores<-c("darkorange", "ivory4", "forestgreen", "maroon4")
dibuja_simulaciones(juntas=TRUE, p0, colores, "lines", 10000)

dibuja_recinto_centrado(p0[1],p0[2], c=2)
dibuja_punto(p0[1], p0[2], text=FALSE)
colores<-c("darkorange", "ivory4", "forestgreen", "maroon4")
dibuja_simulaciones(juntas=TRUE, p0, colores, "points", 10000)


#////////////////////////////////////////8

distribucion_recorridos<-function(p0, n, M){
        total<-c()
        for(i in 1:n)
                total<-c(recorrido_aleat(p0[1], p0[2], M)$P, total)
        return(total)
}

distri<-distribucion_recorridos(p0, 10, 60)
distri


#////////////////////////////////////////9

min(distri)
min(distri, na.rm=TRUE)

N<-1000
M<-3000
pasos_necesarios <- distribucion_recorridos(p0, N, M)

summary(pasos_necesarios)

hist(pasos_necesarios, xlab="pasos necesarios", ylab="Frecuencia",
        main="Pasos necesarios para llegar al límite", col="gray47", border="black")

hist(pasos_necesarios,  prob=T, xlab="Pasos necesarios", ylab="dDensidad",
main="Pasos necesarios para llegar al límite", col="gray47", border="black", ylim=c(0,0.055))

lines(density(pasos_necesarios), lwd=2, col="chocolate3")

curve(dunif(x, min=min(pasos_necesarios), max=max(pasos_necesarios)), add=T, lwd=2, col="orange")
curve(dnorm(x, mean=mean(pasos_necesarios), sd=sd(pasos_necesarios)) , add=T, lwd=2, col="dodgerblue")
lines(0:max(pasos_necesarios), dpois(0:max(pasos_necesarios),
        lambda=mean(pasos_necesarios)), lwd=2, col="red3")
#curve(dpois(x, lambda=mean(pasos_necesarios)), add=T, lwd=2, col="red3")


u <- paste("Uniforme" , "(a=" ,min(pasos_necesarios), "," , "b=", max(pasos_necesarios), ")" )
n <- paste("Normal" , "(µ=" ,round(mean(pasos_necesarios),2), "," , "s= " , round(sd(pasos_necesarios),2) , ")" )
p <- paste("Poisson" , "(L=" ,round(mean(pasos_necesarios),2) , ")" )

u<-gsub("\\s", replacement="", x=u)
n<-gsub("\\s", replacement="", x=n)
p<-gsub("\\s", replacement="", x=p)


legend("topright", legend=c("Pasos necesarios", u , n, p),  lwd=2,
        col=c("chocolate3","orange", "dodgerblue", "red3"), lty=1, box.lty=0, cex=0.9)

max(pasos_necesarios)


