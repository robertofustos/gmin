#' @title Graficar el variograma experimental omnidireccional
#' @description Esta funcion permite graficar el variograma experimental omnidireccional
#' y ademas incorporar a el un variograma teorico
#' @param expvar un objeto proveniente de la funcion "expvar_omni"
#' @param pares un valor TRUE o FALSE, indicando si se desea agregar el numero de pares de cada lag en el grafico
#' @param fitvar un objeto proveniente de la funcion "fitvar_omni". Se entrega en el
#' caso de querer agregar el variograma teorico al grafico
#' @details Nada
#' @examples
#' @export
plotvar_omni=function(expvar,pares,fitvar){
  if(missing(pares)){pares=FALSE}
  if(missing(fitvar)){ajustado=FALSE}else{ajustado=TRUE}
  a1=cbind(matrix(0,3,1),matrix(1,3,4),matrix(0,3,1))
  layout(a1)
  plot(expvar$lags,expvar$variograma,type="l",lty=4,
       xlim=c(0,max(expvar$lags)),
       ylim=c(0,1.25*max(expvar$variograma)),
       axes=FALSE,xlab="",ylab="",col="blue")
  points(expvar$lags,expvar$variograma)
  if(ajustado){
    if(fitvar$modelo=="Exponencial"){
      xx=seq(0,max(expvar$lags),length.out = 1000)
      yy=fitvar$pepa+fitvar$meseta*(1-exp(-3*xx/fitvar$rango))
      points(xx,yy,type="l",col="red")
    }else if(fitvar$modelo=="Esferico"){
      xx=seq(0,max(expvar$lags),length.out = 1000)
      yy=ifelse(xx<=fitvar$rango,fitvar$pepa+fitvar$meseta*(1.5*xx/fitvar$rango-0.5*(xx/fitvar$rango)**3),fitvar$pepa+fitvar$meseta)
      points(xx,yy,type="l",col="red")
    }else if(fitvar$modelo=="Gaussiano"){
      xx=seq(0,max(expvar$lags),length.out = 1000)
      yy=fitvar$pepa+fitvar$meseta*(1-exp(-3*(xx/fitvar$rango)**2))
      points(xx,yy,type="l",col="red")
    }
  }
  if(pares){
    aux1=range(expvar$variograma)[2]-range(expvar$variograma)[1]
    aux2=rep(c(0.15,-0.15),round(length(expvar$variograma)/2+1))[1:length(expvar$variograma)]
    text(expvar$lags,expvar$variograma+aux1*aux2,labels = expvar$pares,cex=0.8)
  }
  axis(side = 2, las = 1)
  mtext(side = 2, expression(gamma(h)), line = 3, las=2,cex = 0.8)
  axis(side = 1, las = 1)
  mtext(side = 1, "Lags", line = 2.5,cex=0.8)
  # regresar ventana grafica a la normalidad
  par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
}
