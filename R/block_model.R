#' @import plot3D
#' @import rgl
#' @title Crear un modelo de bloques
#' @description Esta funcion permite crear un modelo de bloques en base a sondajes 2D o 3D. El resultado
#' corresponde a los centroides de los bloques que se encuentran a una cierta distancia de los sondajes
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a graficar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param dim es la dimension de cada bloque
#' @param pot es el porcentaje de la distancia maxima a la cual se incluiran los bloques cercanos  a los sondajes
#' @details Nada
#' @examples
#' @export
block_model=function(drillhole,coordinates,dim,df){

  # comprobando paquetes adicionales
  list.of.packages <- c("rgl", "plot3D")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  # integridad de los parametros de entrada
  if(missing(drillhole)){stop("drillhole is missing")}
  if(missing(coordinates)){stop("coordinate names is missing")}
  if(missing(dim)){stop("dimension of blocks is missing")}
  if(missing(df)){stop("distance factor is missing")}
  if(!is.data.frame(drillhole)){stop("drillhole data is not a data.frame")}
  if(!is.vector(coordinates)){stop("coordinates is not a vector")}
  # funciones auxiliares
  {
    vecindad=function(sondajes,coordenadas.sondajes,localidades,coordenadas.localidades,distancia.maxima){
      # definir si son 2d o 3d
      if(length(coordenadas.sondajes)==2){
        d1.1=(1:ncol(sondajes))[names(sondajes)==coordenadas.sondajes[1]]
        d1.2=(1:ncol(sondajes))[names(sondajes)==coordenadas.sondajes[2]]
        compositos=as.matrix(sondajes[,c(d1.1,d1.2)])
        d2.1=(1:ncol(localidades))[names(localidades)==coordenadas.localidades[1]]
        d2.2=(1:ncol(localidades))[names(localidades)==coordenadas.localidades[2]]
        localidades=as.matrix(localidades[,c(d2.1,d2.2)])
      }else{
        d1.1=(1:ncol(sondajes))[names(sondajes)==coordenadas.sondajes[1]]
        d1.2=(1:ncol(sondajes))[names(sondajes)==coordenadas.sondajes[2]]
        d1.3=(1:ncol(sondajes))[names(sondajes)==coordenadas.sondajes[3]]
        compositos=as.matrix(sondajes[,c(d1.1,d1.2,d1.3)])
        d2.1=(1:ncol(localidades))[names(localidades)==coordenadas.localidades[1]]
        d2.2=(1:ncol(localidades))[names(localidades)==coordenadas.localidades[2]]
        d2.3=(1:ncol(localidades))[names(localidades)==coordenadas.localidades[3]]
        localidades=as.matrix(localidades[,c(d2.1,d2.2,d2.3)])
      }
      nn1=as.integer(nrow(compositos))
      nn2=as.integer(nrow(localidades))
      storage.mode(compositos)<-"double"
      storage.mode(localidades)<-"double"
      storage.mode(distancia.maxima)<-"double"
      output=matrix(0,nn2,nn1)
      storage.mode(output)<-"integer"
      if(length(coordenadas.sondajes)==2){
        library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
        res=.Fortran("near_2d",nn1=nn1,nn2=nn2,
                     compositos=compositos,
                     localidades=localidades,
                     vec=distancia.maxima,output=output,PACKAGE = "all")
      }else{
        library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
        res=.Fortran("near_3d",nn1=nn1,nn2=nn2,
                     compositos=compositos,
                     localidades=localidades,
                     vec=distancia.maxima,output=output,PACKAGE = "all")
      }
      return(res$output)
    }
  }
  if(length(coordinates)==2){
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    dat=drillhole[,c(c1,c2)]
    tol=0.05
    aux0=apply(dat,2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    x=seq(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1],by=dim[1])
    y=seq(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2],by=dim[2])
    block=expand.grid(x,y)
    seleccionados=vecindad(dat,names(dat),block,names(block),distancia.maxima=max(aux2)*df)
    block=block[rowSums(seleccionados)!=0,]
    names(block)=coordinates
    return(block)
  }else{
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("coordenada [3] no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    dat=drillhole[,c(c1,c2,c3)]
    tol=0.05
    aux0=apply(dat,2,range)
    aux1=aux0[2,]-aux0[1,]
    aux2=aux1*tol
    x=seq(min(dat[,1])-aux2[1],max(dat[,1])+aux2[1],by=dim[1])
    y=seq(min(dat[,2])-aux2[2],max(dat[,2])+aux2[2],by=dim[2])
    z=seq(min(dat[,3])-aux2[3],max(dat[,3])+aux2[3],by=dim[3])
    block=expand.grid(x,y,z)
    seleccionados=vecindad(dat,names(dat),block,names(block),distancia.maxima=max(aux2)*df)
    block=block[rowSums(seleccionados)!=0,]
    names(block)=coordinates
    return(block)
  }
}
