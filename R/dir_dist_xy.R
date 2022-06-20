dir_dist_xy=function(drillhole,coordinates,localidad,azimuth,dip){
  # verificar parametros pedidos
  if(missing(drillhole)){stop("se requiere ingresar un data.frame con los sondajes")}
  if(missing(coordinates)){stop("se requiere ingresar el nombre de las coordenadas")}
  if(missing(localidad)){stop("se requiere ingresar la localidad")}
  if(missing(azimuth)){stop("se requiere ingresar los valores de azimuth")}
  if(missing(dip)){dip=0}
  if(length(coordinates)==3 & missing(dip)){stop("se requiere ingresar los valores de dip")}
  # cargar rutinas en fortran
  # if(Sys.info()[1]=="Linux"){dyn.load("dist_dir3d_tov.so")}else{dyn.load("dist_dir3d_tov.dll")}
  # if(Sys.info()[1]=="Linux"){dyn.load("dist_dir2d_tov.so")}else{dyn.load("dist_dir2d_tov.dll")}
  # ver integridad de los parametros pedidos
  if(is.null(nrow(drillhole))){stop("verificar integridad de la base de sondajes")}
  if(length(coordinates)<2 | length(coordinates)>3){stop("verificar integridad de las coordenadas")}
  if(length(localidad)<2 | length(localidad)>3){stop("verificar integridad de la localidad")}
  if(length(localidad)<2 | length(localidad)>3){stop("verificar integridad de los azimuth")}
  if(length(coordinates)==3 & length(dip)!=3){stop("verificar integridad de los dip")}
  # calcular distancias en caso 2d o 3d
  if(length(coordinates)==2){
    d1.1=(1:ncol(drillhole))[names(drillhole)==coordinates[1]]
    d1.2=(1:ncol(drillhole))[names(drillhole)==coordinates[2]]
    compositos=as.matrix(drillhole[,c(d1.1,d1.2)])
    nn=as.integer(nrow(compositos))
    coord=as.matrix(compositos)
    storage.mode(coord)<-"double"
    storage.mode(localidad)<-"double"
    storage.mode(azimuth)<-"double"
    ans=array(0,dim=c(nn,length(azimuth)))
    storage.mode(ans)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("dist_dir2d_tov",nn=nn,coord=coord,loc=localidad,azi=azimuth,output=ans,PACKAGE = "all")
    return(res$output)
  }else{
    d1.1=(1:ncol(drillhole))[names(drillhole)==coordinates[1]]
    d1.2=(1:ncol(drillhole))[names(drillhole)==coordinates[2]]
    d1.3=(1:ncol(drillhole))[names(drillhole)==coordinates[3]]
    compositos=as.matrix(drillhole[,c(d1.1,d1.2,d1.3)])
    nn=as.integer(nrow(compositos))
    coord=as.matrix(compositos)
    storage.mode(coord)<-"double"
    storage.mode(localidad)<-"double"
    storage.mode(azimuth)<-"double"
    storage.mode(dip)<-"double"
    ans=array(0,dim=c(nn,length(azimuth)))
    storage.mode(ans)<-"double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    res=.Fortran("dist_dir3d_tov",nn=nn,coord=coord,vec=localidad,azi=azimuth,dip=dip,output=ans,PACKAGE = "all")
    return(res$output)
  }
}
