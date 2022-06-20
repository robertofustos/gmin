#' @title Calcular el variograma experimental omnidireccional
#' @description Esta funcion permite calcular el variograma experimental omnidireccional
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a utilizar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param variable corresponde al nombre de la variable a utilizar
#' @param lags es el numero de lags del variograma experimental
#' @param maxdist es la maxima distancia a la cual se calcula el variograma
#' @param tol.lag corresponde a la tolerancia porcentual en el lag de los variogramas
#' @details Esta funcion sirve para sondajes 2D o 3D
#' @examples
#' @export
expvar_omni=function(drillhole,coordinates,variable,lags,maxdist,tol.lag){
  # comprobando paquetes adicionales
  list.of.packages <- c("rgl", "plot3D")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){install.packages(new.packages)}
  # integridad de los parametros de entrada
  if(missing(drillhole)){stop("sondajes no especificados")}
  if(missing(coordinates)){stop("nombres de coordenadas no especificados")}
  if(missing(variable)){stop("nombre de la variable no especificado")}
  if(missing(lags)){stop("se debe entregar la dimension de los bloques")}
  if(missing(maxdist)){stop("se debe entregar la distancia maxima del variograma")}
  if(missing(tol.lag)){stop("se debe entregar la tolerancia el en lag")}
  if(!is.data.frame(drillhole)){stop("sondajes deben ser un objeto de tipo data.frame")}
  if(!is.vector(coordinates)){stop("coordenadas deben ser un objeto de tipo vector")}
  # 2d o 3d
  if(length(coordinates)==2){
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==variable
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    dat=as.matrix(drillhole[,c(c1,c2,c3)])
    storage.mode(dat)<-"double"
    nn=as.integer(nrow(dat))
    pp=as.integer(2)
    storage.mode(lags)<-"integer"
    storage.mode(maxdist)<-"double"
    storage.mode(tol.lag)<-"double"
    variograma=rep(0,lags)
    pares=rep(0,lags)
    lag=rep(0,lags)
    storage.mode(variograma)<-"double"
    storage.mode(pares)<-"double"
    storage.mode(lag)<-"double"
    storage.mode(tol.lag)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("exp_var1",
                 nn=nn,
                 pp=pp,
                 dat=dat,
                 nlag=lags,
                 maxdist=maxdist,
                 tollag=tol.lag,
                 variograma=variograma,
                 pares=pares,
                 lags=lag,PACKAGE = "all")
    variograma=res$variograma/(2*res$pares)
    return(list(variograma=variograma,lags=res$lags,pares=res$pares))
  }else{
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    a4=names(drillhole)==variable
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("coordenada [3] no encontrada")}
    if(sum(a4)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    c4=(1:ncol(drillhole))[a4]
    dat=as.matrix(drillhole[,c(c1,c2,c3,c4)])
    storage.mode(dat)<-"double"
    nn=as.integer(nrow(dat))
    pp=as.integer(3)
    storage.mode(lags)<-"integer"
    storage.mode(maxdist)<-"double"
    variograma=rep(0,lags)
    pares=rep(0,lags)
    lag=rep(0,lags)
    storage.mode(variograma)<-"double"
    storage.mode(pares)<-"double"
    storage.mode(lag)<-"double"
    storage.mode(tol.lag)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("exp_var1",
                 nn=nn,
                 pp=pp,
                 dat=dat,
                 nlag=lags,
                 maxdist=maxdist,
                 tollag=tol.lag,
                 variograma=variograma,
                 pares=pares,
                 lags=lag,PACKAGE = "all")
    variograma=res$variograma/(2*res$pares)
    return(list(variograma=variograma,lags=res$lags,pares=res$pares))
  }
}
