#' @title Calcular el variograma experimental direccional
#' @description Esta funcion permite calcular el variograma experimental direccional
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a utilizar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param variable corresponde al nombre de la variable a utilizar
#' @param azimuth corresponde a los azimuth de las direcciones a utilizar
#' @param dip corresponde a los dip de las direcciones a utilizar
#' @param lags es el numero de lags del variograma experimental
#' @param maxdist es la maxima distancia a la cual se calcula el variograma
#' @param tol.lag corresponde a la tolerancia porcentual en el lag de los variogramas
#' @param tol.dir corresponde a la tolerancia angular en la direccion de los variogramas
#' @details Esta funcion sirve para sondajes 2D o 3D
#' @examples
#' @export
expvar_dir=function(drillhole,coordinates,variable,azimuth,dip,lags,maxdist,tol.lag,tol.dir){

  # comprobando paquetes adicionales
  list.of.packages <- c("rgl", "plot3D")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){install.packages(new.packages)}
  # comprobar parametros
  if(missing(tol.lag)){tol.lag=rep(0.5,length(azimuth))}
  if(missing(tol.dir)){tol.dir=rep(5,length(azimuth))}
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
    if(length(lags)!=length(azimuth)    |
       length(maxdist)!=length(azimuth) |
       length(tol.lag)!=length(azimuth) |
       length(tol.dir)!=length(azimuth)){
      stop("en el largo de los parametros variograficos")
    }
    dat=as.matrix(dat);storage.mode(dat)<-"double"
    nn=as.integer(nrow(dat))
    pp=as.integer(2)
    kk=as.integer(1)
    ve_type=as.integer(1)
    ve_ndir=as.integer(length(azimuth))
    ve_cutoff=maxdist
    storage.mode(ve_cutoff)<-"double"
    ve_nlag=as.integer(lags)
    ve_tlag=tol.lag*100
    storage.mode(ve_tlag)<-"double"
    ve_tdir=tol.dir
    storage.mode(ve_tdir)<-"double"
    ve_direction=matrix(azimuth,ncol=1);storage.mode(ve_direction)<-"double"
    var_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(var_mat)<-"double"
    count_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(count_mat)<-"double"
    lag_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(lag_mat)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("dexp_var",nn=nn,pp=pp,kk=kk,dat=dat,
                 ve_type=ve_type,
                 ve_ndir=ve_ndir,
                 ve_cutoff=ve_cutoff,
                 ve_nlag=ve_nlag,
                 ve_tlag=ve_tlag,
                 ve_tdir=ve_tdir,
                 ve_direction=ve_direction,
                 vm_nlag=as.integer(max(ve_nlag)),
                 var_mat=var_mat,
                 count_mat=count_mat,
                 lag_mat=lag_mat,PACKAGE = "all")
    res$var_mat=(res$var_mat/(2*res$count_mat))
    return(list(variable=variable,
                direcciones=as.numeric(ve_direction),
                cutoff=as.numeric(ve_cutoff),
                variograma=res$var_mat[,,1],
                lags=res$lag_mat[,,1],
                pares=res$count_mat[,,1]))
  }else{
    if(missing(dip)){stop("dip no especificado")}
    a1=names(drillhole)==coordinates[1]
    a2=names(drillhole)==coordinates[2]
    a3=names(drillhole)==coordinates[3]
    a4=names(drillhole)==variable
    if(sum(a1)==0){stop("coordenada [1] no encontrada")}
    if(sum(a2)==0){stop("coordenada [2] no encontrada")}
    if(sum(a3)==0){stop("coordenada [4] no encontrada")}
    if(sum(a4)==0){stop("variable no encontrada")}
    c1=(1:ncol(drillhole))[a1]
    c2=(1:ncol(drillhole))[a2]
    c3=(1:ncol(drillhole))[a3]
    c4=(1:ncol(drillhole))[a4]
    dat=as.matrix(drillhole[,c(c1,c2,c3,c4)])
    if(length(lags)!=length(azimuth)    |
       length(maxdist)!=length(azimuth) |
       length(tol.lag)!=length(azimuth) |
       length(tol.dir)!=length(azimuth)){
      stop("en el largo de los parametros variograficos")
    }
    dat=as.matrix(dat);storage.mode(dat)<-"double"
    nn=as.integer(nrow(dat))
    pp=as.integer(3)
    kk=as.integer(1)
    ve_type=as.integer(1)
    ve_ndir=as.integer(length(azimuth))
    ve_cutoff=maxdist
    storage.mode(ve_cutoff)<-"double"
    ve_nlag=as.integer(lags)
    ve_tlag=tol.lag*100
    storage.mode(ve_tlag)<-"double"
    ve_tdir=tol.dir
    storage.mode(ve_tdir)<-"double"
    ve_direction=as.matrix(cbind(azimuth,dip));storage.mode(ve_direction)<-"double"
    var_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(var_mat)<-"double"
    count_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(count_mat)<-"double"
    lag_mat=array(0,dim=c(max(ve_nlag),ve_ndir,kk));storage.mode(lag_mat)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("dexp_var",nn=nn,pp=pp,kk=kk,dat=dat,
                 ve_type=ve_type,
                 ve_ndir=ve_ndir,
                 ve_cutoff=ve_cutoff,
                 ve_nlag=ve_nlag,
                 ve_tlag=ve_tlag,
                 ve_tdir=ve_tdir,
                 ve_direction=ve_direction,
                 vm_nlag=as.integer(max(ve_nlag)),
                 var_mat=var_mat,
                 count_mat=count_mat,
                 lag_mat=lag_mat,PACKAGE = "all")
    res$var_mat=(res$var_mat/(2*res$count_mat))
    return(list(variable=variable,
                direcciones=cbind(azimuth,dip),
                cutoff=as.numeric(ve_cutoff),
                variograma=res$var_mat[,,1],
                lags=res$lag_mat[,,1],
                pares=res$count_mat[,,1]))
  }
}
