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
