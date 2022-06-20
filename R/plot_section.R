#' @import plot3D
#' @import rgl
#' @import grDevices
#' @import graphics
#' @title Graficar secciones de sondajes
#' @description Esta funcion permite graficar secciones 2D de un conjunto de sondajes
#' usando o no una variable continua o categorica en base a una escala de colores predefinida
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a graficar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param variable corresponde al nombre de la variable a graficar
#' @param plane corresponde a la dimension del plano a graficar, XY, XZ o YZ
#' @param level corresponde a la ubicacion del plano
#' @param tol corresponde a un parametro que permite capturar mas datos dentro del plano
#' @param pt.type es la clase de punto en el grafico
#' @param pt.size es el tamano del punto en el grafico
#' @param pt.col es el color del punto en el grafico
#' @details En el caso de no definir un level se graficara el punto medio de la dimension de interes
#' @examples
#' @export
plot_section=function(drillhole,coordinates,variable,plane,level,tol,pt.type,pt.size,pt.col){

  # comprobando paquetes adicionales
  list.of.packages <- c("rgl", "plot3D")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # integridad de los parametros de entrada
  {
    # archivos de entrada
    if(missing(drillhole)){stop("se debe ingresar data.frame con los sondajes")}
    if(missing(coordinates)){stop("se deben entregar los nombres de las coordenadas")}
    if(missing(plane)){stop("se debe indicar el tipo de plano a realizar (XY,XZ,YZ)")}
    # parametros opcionales
    if(missing(level)){level="no"}
    if(missing(tol)){tol=0.05}
    if(missing(pt.type)){pt.type=20}
    if(missing(pt.size)){pt.size=1.5}
    if(missing(pt.col )){pt.col="black"}
    if(is.null(nrow(drillhole))){stop("verificar integridad de la base de drillhole")}
    if(length(coordinates)!=3){stop("aplicable solo a sondajes 3D")}
  }
  # funciones auxiliares
  {
    # pause a code
    pause = function(arg){
      if (interactive())
      {
        invisible(readline(prompt = paste(arg," Press <Enter> to continue...",sep="")))
      }
      else
      {
        cat(c(arg," Press <Enter> to continue...",sep=""))
        invisible(readLines(file("stdin"), 1))
      }
    }
    # agregar ceros intermedios a un vector (para la leyenda)
    zeros = function(vector){
      zz=NULL
      for(ii in 1:length(vector)){
        zz=c(zz,c(vector[ii],0))
      }
      zz=zz[-length(zz)]
      return(zz)
    }
    # agregar blancos intermedios a un vector de colores(para la leyenda)
    zeros_color = function(vector){
      zz=NULL
      for(ii in 1:length(vector)){
        zz=c(zz,c(vector[ii],"#FFFFFF"))
      }
      zz=zz[-length(zz)]
      return(zz)
    }
  }
  # variable=no; plano=XY; tipo=no
  {
    if( missing(variable) & plane == "XY" ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      dat=drillhole[,c(c1,c2,c3)]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,3])}
      # ventanas graficas
      a1=cbind(0,matrix(1,4,4),0)
      layout(a1)
      # subset
      b0=as.numeric(level-aux2[3])
      b1=as.numeric(level+aux2[3])
      dat1=dat[b0 <=dat[,3] & dat[,3] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=pt.col,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = aux0[1,2], z0 = level,
             x1 = aux0[2,1], y1 = aux0[2,2],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # ventanas graficas
      a1=cbind(0,matrix(1,4,4),0)
      layout(a1)
      # puntos en el plano
      plot(dat1[,c(1,2)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           col=pt.col,
           main=paste(names(dat1)[3],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
    }
  }
  # variable=no; plano=XZ; tipo=no
  {
    if( missing(variable) & plane == "XZ" ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      dat=drillhole[,c(c1,c2,c3)]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,2])}
      # ventana grafica
      a1=cbind(0,matrix(1,4,4),0)
      layout(a1)
      # subset
      b0=as.numeric(level-aux2[2])
      b1=as.numeric(level+aux2[2])
      dat1=dat[b0 <=dat[,2] & dat[,2] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=pt.col,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = level, z0 = aux0[1,3],
             x1 = aux0[2,1], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # ventana grafica
      a1=cbind(0,matrix(1,4,4),0)
      layout(a1)
      # puntos en el plano
      plot(dat1[,c(1,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=pt.col,
           main=paste(names(dat1)[2],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
    }
  }
  # variable=no; plano=YZ; tipo=no
  {
    if( missing(variable) & plane == "YZ" ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      dat=drillhole[,c(c1,c2,c3)]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,1])}
      # ventana grafica
      a1=cbind(0,matrix(1,4,4),0)
      layout(a1)
      # subset
      b0=as.numeric(level-aux2[1])
      b1=as.numeric(level+aux2[1])
      dat1=dat[b0 <=dat[,1] & dat[,1] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=pt.col,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = level, y0 = aux0[1,2], z0 = aux0[1,3],
             y1 = aux0[2,2], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # puntos en el plano
      plot(dat1[,c(2,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=pt.col,
           main=paste(names(dat1)[1],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
    }
  }
  # variable=si; plano=XY; tipo=cont
  {
    if( !missing(variable) & plane == "XY" & is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(50)[
                              as.numeric(cut(dat[,4],breaks=50))]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,3])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[3])
      b1=as.numeric(level+aux2[3])
      dat1=dat[b0 <=dat[,3] & dat[,3] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = aux0[1,2], z0 = level,
             x1 = aux0[2,1], y1 = aux0[2,2],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(1,2)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           col=cl,
           main=paste(names(dat1)[3],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
    }
  }
  # variable=si; plano=XZ; tipo=cont
  {
    if( !missing(variable) & plane == "XZ" & is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(50)[
                              as.numeric(cut(dat[,4],breaks=50))]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,2])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[2])
      b1=as.numeric(level+aux2[2])
      dat1=dat[b0 <=dat[,2] & dat[,2] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = level, z0 = aux0[1,3],
             x1 = aux0[2,1], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(1,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=cl,
           main=paste(names(dat1)[2],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
    }
  }
  # variable=si; plano=YZ; tipo=cont
  {
    if( !missing(variable) & plane == "YZ" & is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(50)[
                              as.numeric(cut(dat[,4],breaks=50))]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,1])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[1])
      b1=as.numeric(level+aux2[1])
      dat1=dat[b0 <=dat[,1] & dat[,1] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = level, y0 = aux0[1,2], z0 = aux0[1,3],
             y1 = aux0[2,2], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(2,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=cl,
           main=paste(names(dat1)[1],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                     "yellow","orange","red","red4"))(50),
            axes=FALSE,main=variable)
      c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
      c2=seq(0,1,length.out=10)
      axis(2, at = c2, labels = round(c1,1),
           tick = TRUE,lty = "solid",las=2)
    }
  }
  # variable=si; plano=XY; tipo=cat
  {
    if( !missing(variable) & plane == "XY" & !is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      clases=levels(dat[,4])
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(
                              length(clases))[as.numeric(dat[,4])]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,3])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[3])
      b1=as.numeric(level+aux2[3])
      dat1=dat[b0 <=dat[,3] & dat[,3] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = aux0[1,2], z0 = level,
             x1 = aux0[2,1], y1 = aux0[2,2],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(1,2)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           col=cl,
           main=paste(names(dat1)[3],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
    }
  }
  # variable=si; plano=XZ; tipo=cat
  {
    if( !missing(variable) & plane == "XZ" & !is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      clases=levels(dat[,4])
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(
                              length(clases))[as.numeric(dat[,4])]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,2])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[2])
      b1=as.numeric(level+aux2[2])
      dat1=dat[b0 <=dat[,2] & dat[,2] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = aux0[1,1], y0 = level, z0 = aux0[1,3],
             x1 = aux0[2,1], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(1,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=cl,
           main=paste(names(dat1)[2],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
    }
  }
  # variable=si; plano=YZ; tipo=cat
  {
    if( !missing(variable) & plane == "YZ" & !is.numeric(drillhole[[variable]]) ){
      # preparar datos
      a1=names(drillhole)==coordinates[1]
      a2=names(drillhole)==coordinates[2]
      a3=names(drillhole)==coordinates[3]
      a4=names(drillhole)==variable
      if(sum(a1)==0){stop("coordinate [1] is missing")}
      if(sum(a2)==0){stop("coordinate [1] is missing")}
      if(sum(a3)==0){stop("coordinate [1] is missing")}
      if(sum(a4)==0){stop("variable no encontrada")}
      c1=(1:ncol(drillhole))[a1]
      c2=(1:ncol(drillhole))[a2]
      c3=(1:ncol(drillhole))[a3]
      c4=(1:ncol(drillhole))[a4]
      dat=drillhole[,c(c1,c2,c3,c4)]
      clases=levels(dat[,4])
      cl=colorRampPalette(c("blue4","blue","cyan","green",
                            "yellow","orange","red","red4"))(
                              length(clases))[as.numeric(dat[,4])]
      aux0=apply(dat[,1:3],2,range)
      aux1=aux0[2,]-aux0[1,]
      aux2=aux1*tol
      if(level=="no"){level=mean(aux0[,1])}
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # subset
      b0=as.numeric(level-aux2[1])
      b1=as.numeric(level+aux2[1])
      dat1=dat[b0 <=dat[,1] & dat[,1] <= b1,]
      # plot 3d con plano
      scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
                theta=45,phi = 15, bty ="g",scale=FALSE,
                col=cl,pch=pt.type,cex=pt.size,
                xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
                ticktype = "detailed",cex.axis=0.7,pos=2,outer=FALSE,las=2,colkey = FALSE)
      rect3D(x0 = level, y0 = aux0[1,2], z0 = aux0[1,3],
             y1 = aux0[2,2], z1 = aux0[2,3],
             bty = "g", facets = TRUE,
             border = "black", col ="#7570B3", alpha=0.5,
             lwd = 2, phi = 20,add=TRUE)
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
      # ventana grafica
      a2=cbind(0,matrix(1,4,4),2,0)
      layout(a2)
      # puntos en el plano
      plot(dat1[,c(2,3)],cex=pt.size,pch=20,asp=1,
           xlim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
           ylim=c(min(dat1[,3])-aux2[3],max(dat1[,3])+aux2[3]),
           col=cl,
           main=paste(names(dat1)[1],"=",round(level,1)," (tol",round(tol*100,1),"%)",sep=""))
      grid(col="gray80")
      # escala de colores
      zz1=zeros(1:length(clases))
      zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                         "yellow","orange","red","red4"))(length(clases)))
      image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
      c2=seq(0,1,length.out=length(clases))
      a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
      axis(2, at = c2, labels = paste(clases,sep=""),
           tick = FALSE,lty = "solid",las=2)
      axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
           tick = FALSE,lty = "solid",las=2)
    }
  }
  # regresar ventana grafica a la normalidad
  par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
}
