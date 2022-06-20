#' @import plot3D
#' @import rgl
#' @import grDevices
#' @import graphics
#' @title Graficar sondajes
#' @description Esta funcion permite graficar sondajes 2D o 3D usando o no una
#' variable continua o categorica en base a una escala de colores predefinida
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a graficar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param variable corresponde al nombre de la variable a graficar
#' @param pt.type es la clase de punto en el grafico
#' @param pt.size es el tamano del punto en el grafico
#' @param pt.col es el color del punto en el grafico
#' @details En el caso de usar una variable continua el grafico mostrara ademas
#' de los sondajes un histograma con la densidad de la variable. En el caso de una
#' variable categorica mostrara un grafico de torta con las clases y sus porcentajes.
#' En el caso de no definir una variable se graficaran solamente las posiciones de los sondajes.
#' @examples
#' @export
plot_drillhole=function(drillhole,coordinates,variable,pt.type,pt.size,pt.col){

  # comprobando paquetes adicionales
  list.of.packages <- c("rgl", "plot3D")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # integridad de los parametros de entrada
  if(missing(drillhole)){stop("sondajes no especificados")}
  if(missing(coordinates)){stop("nombres de coordenadas no especificados")}
  if(!is.data.frame(drillhole)){stop("sondajes deben ser un objeto de tipo data.frame")}
  if(!is.vector(coordinates)){stop("coordenadas deben ser un objeto de tipo vector")}
  if(missing(pt.type)){pt.type=20}
  if(missing(pt.size)){pt.size=1.5}
  if(missing(pt.col )){pt.col="black"}
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
  # ver integridad de los parametros pedidos
  if(is.null(nrow(drillhole))){stop("sondajes sin observaciones")}
  if(length(coordinates)<2 | length(coordinates)>3){stop("verificar integridad de las coordenadas")}
  # plot drillhole 2d sin variable
  if(missing(variable) & length(coordinates)==2){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    if(sum(a1)==0){stop("coordinate [1] is missing")}
    if(sum(a2)==0){stop("coordinate [2] is missing")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    dat=drillhole[,c(c1,c2)]
    tol=0.01
    aux0=apply(dat[,1:2],2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    # ventana grafica
    a1=cbind(0,matrix(1,4,4),0)
    layout(a1)
    # grafico 1
    plot(dat[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,col=pt.col,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]))
    grid(col="gray80")
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }
  # plot drillhole 2d con variable numerica
  if(!missing(variable) & length(coordinates)==2 & class(drillhole[,variable])=="numeric"){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==variable
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    dat=drillhole[,c(c1,c2,c3)]
    # codigo
    cl=colorRampPalette(c("blue4","blue","cyan","green",
                          "yellow","orange","red","red4"))(50)[
                            as.numeric(cut(dat[,3],breaks=50))]
    dat1=dat[,c(1,2)]
    tol=0.01
    aux0=apply(dat1,2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    # ventana grafica
    a2=cbind(0,matrix(1,4,4),2,0)
    layout(a2)
    # grafico 1
    plot(dat1[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,
         xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
         ylim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
         col=cl)
    grid(col="gray80")
    image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                   "yellow","orange","red","red4"))(50),
          axes=FALSE,main=variable)
    c1=seq(min(dat[,3]),max(dat[,3]),length.out=10)
    c2=seq(0,1,length.out=10)
    axis(2, at = c2, labels = round(c1,1),
         tick = TRUE,lty = "solid",las=2)
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }
  # plot drillhole 2d con variable categorica
  if(!missing(variable) & length(coordinates)==2 & class(drillhole[,variable])=="factor"){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==variable
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    dat=drillhole[,c(c1,c2,c3)]
    #codigo
    clases=levels(dat[,3])
    cl=colorRampPalette(c("blue4","blue","cyan","green",
                          "yellow","orange","red","red4"))(
                            length(clases))[as.numeric(dat[,3])]
    dat1=dat[,c(1,2)]
    tol=0.01
    aux0=apply(dat1,2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    # ventana grafica
    a2=cbind(0,matrix(1,4,4),2,0)
    layout(a2)
    # grafico 1
    plot(dat1[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,
         xlim=c(min(dat1[,1])-aux2[1],max(dat1[,1])+aux2[1]),
         ylim=c(min(dat1[,2])-aux2[2],max(dat1[,2])+aux2[2]),
         col=cl)
    grid(col="gray80")
    zz1=zeros(1:length(clases))
    zz2=zeros_color(colorRampPalette(c("blue4","blue","cyan","green",
                                       "yellow","orange","red","red4"))(length(clases)))
    image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
    c2=seq(0,1,length.out=length(clases))
    a=round(table(dat[,3])/sum(table(dat[,3]))*100,1)
    axis(2, at = c2, labels = paste(clases,sep=""),
         tick = FALSE,lty = "solid",las=2)
    axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
         tick = FALSE,lty = "solid",las=2)
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }
  # plot drillhole 3d sin variable
  if(missing(variable) & length(coordinates)==3){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    if(sum(a1)==0){stop("coordinate [1] is missing")}
    if(sum(a2)==0){stop("coordinate [2] is missing")}
    if(sum(a3)==0){stop("coordinate [3] is missing")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    dat=drillhole[,c(c1,c2,c3)]
    tol=0.01
    aux0=apply(dat[,1:3],2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    # ventana grafica
    a1=cbind(0,matrix(1,4,4),0)
    layout(a1)
    # graf 1
    scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
              theta=45,phi = 15, bty ="g",scale=FALSE,
              col=pt.col,pch=pt.type,cex=pt.size,
              xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
              ticktype = "detailed",cex.axis=0.8,pos=2,outer=FALSE,las=2)
    # graf 2
    plot(dat[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,col=pt.col,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]))
    grid(col="gray80")
    # graf 3
    plot(dat[,c(1,3)],cex=pt.size,pch=pt.type,asp=1,col=pt.col,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    # graf 4
    plot(dat[,c(2,3)],cex=pt.size,pch=pt.type,asp=1,col=pt.col,
         xlim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    # grafico interactivo
    points3d(dat[,1:3], col=pt.col,size=pt.size+1)
    box3d()
    grid3d(c("x", "y+", "z"))
    title3d('', '', names(dat)[1], names(dat)[2], names(dat)[3])
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }
  # plot drillhole 3d con variable numerica
  if(!missing(variable) & length(coordinates)==3 & class(drillhole[,variable])=="numeric"){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    a4=names(drillhole)==variable
    if(sum(a1)==0){stop("coordinate [1] is missing")}
    if(sum(a2)==0){stop("coordinate [2] is missing")}
    if(sum(a3)==0){stop("coordinate [3] is missing")}
    if(sum(a4)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    c4=(1:ncol(drillhole))[a4]
    dat=drillhole[,c(c1,c2,c3,c4)]
    tol=0.01
    aux0=apply(dat[,1:3],2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    cl=colorRampPalette(c("blue4","blue","cyan","green",
                          "yellow","orange","red","red4"))(50)[
                            as.numeric(cut(dat[,4],breaks=50))]
    # ventana grafica
    a2=cbind(0,matrix(1,4,4),2,0)
    layout(a2)
    # graf 1
    scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
              theta=45,phi = 15, bty ="g",scale=FALSE,
              col=cl,pch=pt.type,cex=pt.size,
              xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
              ticktype = "detailed",cex.axis=0.8,pos=2,outer=FALSE,las=2,colkey = FALSE)
    image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                   "yellow","orange","red","red4"))(50),
          axes=FALSE,main=variable)
    c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
    c2=seq(0,1,length.out=10)
    axis(2, at = c2, labels = round(c1,1),
         tick = TRUE,lty = "solid",las=2)
    # graf 2
    plot(dat[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]))
    grid(col="gray80")
    image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                   "yellow","orange","red","red4"))(50),
          axes=FALSE,main=variable)
    c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
    c2=seq(0,1,length.out=10)
    axis(2, at = c2, labels = round(c1,1),
         tick = TRUE,lty = "solid",las=2)
    # graf 3
    plot(dat[,c(1,3)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                   "yellow","orange","red","red4"))(50),
          axes=FALSE,main=variable)
    c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
    c2=seq(0,1,length.out=10)
    axis(2, at = c2, labels = round(c1,1),
         tick = TRUE,lty = "solid",las=2)
    # graf 4
    plot(dat[,c(2,3)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    image(matrix(1:50,1,50),col=colorRampPalette(c("blue4","blue","cyan","green",
                                                   "yellow","orange","red","red4"))(50),
          axes=FALSE,main=variable)
    c1=seq(min(dat[,4]),max(dat[,4]),length.out=10)
    c2=seq(0,1,length.out=10)
    axis(2, at = c2, labels = round(c1,1),
         tick = TRUE,lty = "solid",las=2)
    # grafico interactivo
    points3d(dat[,1:3], col=cl,size=pt.size+1)
    box3d()
    axes3d()
    grid3d(c("x", "y+", "z"))
    title3d('', '', names(dat)[1], names(dat)[2], names(dat)[3])
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }
  # plot drillhole 3d con variable categorica
  if(!missing(variable) & length(coordinates)==3 & class(drillhole[,variable])=="factor"){
    # codigo
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    a4=names(drillhole)==variable
    if(sum(a1)==0){stop("coordinate [1] is missing")}
    if(sum(a2)==0){stop("coordinate [2] is missing")}
    if(sum(a3)==0){stop("coordinate [3] is missing")}
    if(sum(a4)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    c4=(1:ncol(drillhole))[a4]
    dat=drillhole[,c(c1,c2,c3,c4)]
    tol=0.01
    aux0=apply(dat[,1:3],2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    clases=levels(dat[,4])
    cl=colorRampPalette(c("blue4","blue","cyan","green",
                          "yellow","orange","red","red4"))(
                            length(clases))[as.numeric(dat[,4])]
    # ventana grafica
    a2=cbind(0,matrix(1,4,4),2,0)
    layout(a2)
    # graf 1
    scatter3D(dat[,c(1)],dat[,c(2)],dat[,c(3)],
              theta=45,phi = 15, bty ="g",scale=FALSE,
              col=cl,pch=pt.type,cex=pt.size,
              xlab=names(dat)[1],ylab=names(dat)[2],zlab=names(dat)[3],
              ticktype = "detailed",cex.axis=0.8,pos=2,outer=FALSE,las=2,colkey = FALSE)
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
    # graf 2
    plot(dat[,c(1,2)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]))
    grid(col="gray80")
    image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
    c2=seq(0,1,length.out=length(clases))
    a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
    axis(2, at = c2, labels = paste(clases,sep=""),
         tick = FALSE,lty = "solid",las=2)
    axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
         tick = FALSE,lty = "solid",las=2)
    # graf 3
    plot(dat[,c(1,3)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
    c2=seq(0,1,length.out=length(clases))
    a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
    axis(2, at = c2, labels = paste(clases,sep=""),
         tick = FALSE,lty = "solid",las=2)
    axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
         tick = FALSE,lty = "solid",las=2)
    # graf 4
    plot(dat[,c(2,3)],cex=pt.size,pch=pt.type,asp=1,col=cl,
         xlim=c(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2]),
         ylim=c(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3]))
    grid(col="gray80")
    image(matrix(zz1,1,length(zz1)),col=c("#FFFFFF",zz2),axes=FALSE,main=variable)
    c2=seq(0,1,length.out=length(clases))
    a=round(table(dat[,4])/sum(table(dat[,4]))*100,1)
    axis(2, at = c2, labels = paste(clases,sep=""),
         tick = FALSE,lty = "solid",las=2)
    axis(4, at = c2, labels = paste(" (",a,"%)",sep=""),
         tick = FALSE,lty = "solid",las=2)
    # grafico interactivo
    points3d(dat[,1:3], col=cl,size=pt.size+1)
    box3d()
    axes3d()
    grid3d(c("x", "y+", "z"))
    title3d('', '', names(dat)[1], names(dat)[2], names(dat)[3])
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }

}
