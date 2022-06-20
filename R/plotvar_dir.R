#' @title Graficar el variograma experimental direccional
#' @description Esta funcion permite graficar el variograma experimental direccional
#' y ademas incorporar a el un variograma teorico
#' @param expvar un objeto proveniente de la funcion "expvar_dir"
#' @param pares un valor TRUE o FALSE, indicando si se desea agregar el numero de pares de cada lag en el grafico
#' @param fitvar un objeto proveniente de la funcion "fitvar_dir". Se entrega en el
#' caso de querer agregar el variograma teorico al grafico
#' @details Nada
#' @examples
#' @export
plotvar_dir=function(expvar,pares,fitvar){
  if(missing(pares)){pares=FALSE}
  if(missing(fitvar)){ajustado=FALSE}else{ajustado=TRUE}
  if(is.matrix(expvar$direcciones)){uu=nrow(expvar$direcciones)}else{uu=length(expvar$direcciones)}
  a1=cbind(matrix(0,3,1),matrix(1,3,4),matrix(0,3,1))
  layout(a1)
  a=as.numeric(expvar$variograma)
  plot(c(0,0),type="n",lty=4,
       xlim=c(0,max(expvar$cutoff)),
       ylim=c(0,1.25*max(a[!is.nan(a)])),
       axes=FALSE,xlab="",ylab="")
  for(ii in 1:uu){
    a=expvar$pares[,ii];
    points(expvar$lags[a!=0,ii],expvar$variograma[a!=0,ii],type="l",lty=4,col=ii)
    points(expvar$lags[a!=0,ii],expvar$variograma[a!=0,ii],col=ii)
  }
  if(ajustado){
    for(ii in 1:uu){
      if(fitvar[[ii]]$modelo=="Exponencial"){
        xx=seq(0,max(expvar$lags[,ii]),length.out = 1000)
        yy=fitvar[[ii]]$pepa+fitvar[[ii]]$meseta*(1-exp(-3*xx/fitvar[[ii]]$rango))
        points(xx,yy,type="l",col=ii)
      }else if(fitvar[[ii]]$modelo=="Esferico"){
        xx=seq(0,max(expvar$lags[,ii]),length.out = 1000)
        yy=ifelse(xx<=fitvar[[ii]]$rango,fitvar[[ii]]$pepa+fitvar[[ii]]$meseta*(1.5*xx/fitvar[[ii]]$rango-0.5*(xx/fitvar[[ii]]$rango)**3),fitvar[[ii]]$pepa+fitvar[[ii]]$meseta)
        points(xx,yy,type="l",col=ii)
      }else if(fitvar[[ii]]$modelo=="Gaussiano"){
        xx=seq(0,max(expvar$lags[,ii]),length.out = 1000)
        yy=fitvar[[ii]]$pepa+fitvar[[ii]]$meseta*(1-exp(-3*(xx/fitvar[[ii]]$rango)**2))
        points(xx,yy,type="l",col=ii)
      }
    }
  }
  if(pares){
    for(ii in 1:uu){
      a=expvar$pares[,ii];
      aux1=range(expvar$variograma[a!=0,ii])[2]-range(expvar$variograma[a!=0,ii])[1]
      aux2=rep(c(0.15,-0.15),round(length(expvar$variograma[a!=0,ii])/2+1))[1:length(expvar$variograma[a!=0,ii])]
      text(expvar$lags[a!=0,ii],expvar$variograma[a!=0,ii]+aux1*aux2,labels = expvar$pares[a!=0,ii],cex=0.8,col=ii)
    }
  }
  axis(side = 2, las = 1)
  mtext(side = 2, expression(gamma(h)), line = 3, las=2,cex = 0.8)
  axis(side = 1, las = 1)
  mtext(side = 1, "Lags", line = 2.5,cex=0.8)
  a=as.numeric(expvar$variograma)
  if(is.matrix(expvar$direcciones)){
    vv=paste("Azimuth ",expvar$direcciones[,1]," y ","Dip ",expvar$direcciones[,2],sep="")
  }else{
    vv=paste("Azimuth ",expvar$direcciones,sep="")
  }
  legend(max(expvar$cutoff)*0.5,0.25*max(a[!is.nan(a)]),
         legend=vv,
         box.col = "white",
         col=1:uu,
         pch=1,pt.cex=1.2,cex=0.9)
  # regresar ventana grafica a la normalidad
  par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
}
