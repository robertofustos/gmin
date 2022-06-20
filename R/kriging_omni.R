#' @title Kriging omnidireccional 2D y 3D
#' @description Esta funcion permite hacer prediccion por medio de kriging omnidireccional, para muestras 2D y 3D
#' @param sondajes corresponde a la base de datos con las coordenadas y la variable a utilizar
#' @param coordenadas.sondajes corresponde a los nombres de las coordenadas en los sondajes
#' @param variable corresponde al nombre de la variable a utilizar
#' @param localidades corresponde a los centroides del modelo de bloques a construir
#' @param coordenadas.localidades corresponde a los nombres de las coordenadas del modelo de bloques
#' @param kriging corresponde al tipo de Kriging a realizar, el que puede ser "Simple" o "Ordinario"
#' @param variograma corresponde al modelo de variograma a utilizar, el que puede ser "Exponencial", "Esferico" o "Gaussiano"
#' @param pepa corresponde a la pepa del variograma a utilizar para la prediccion
#' @param meseta corresponde a la meseta del variograma a utilizar para la prediccion
#' @param alcance corresponde al alcance del variograma a utilizar para la prediccion
#' @param media corresponde a la media de la variable en caso de realizar Kriging Simple
#' @param max.num.pares corresponde al numero maximo de pares para realizar la prediccion
#' @details Esta funcion sirve para sondajes 2D o 3D
#' @examples
#' @export
kriging_omni=function(sondajes,coordenadas.sondajes,variable,
                      localidades,coordenadas.localidades,
                      kriging,variograma,pepa,meseta,alcance,
                      media,max.num.pares){
  # verificar parametros pedidos
  if(missing(sondajes)){stop("se requiere ingresar un data.frame con los sondajes")}
  if(missing(coordenadas.sondajes)){stop("se requiere ingresar el nombre de las coordenadas de los sondajes")}
  if(missing(variable)){stop("se requiere ingresar el nombre de la variable a predecir")}
  if(missing(localidades)){stop("se requiere ingresar las localidades de estimacion")}
  if(missing(coordenadas.localidades)){stop("se requiere ingresar el nombre de las coordenadas de las localidades")}
  if(missing(kriging)){stop("se requiere ingresar el tipo de kriging (Ordinario o Simple)")}
  if(missing(variograma)){stop("se requiere ingresar el modelo de variograma (Exponencial, Gaussiano o Esferico)")}
  if(missing(pepa)){stop("se requiere ingresar la pepa del modelo de variograma")}
  if(missing(meseta)){stop("se requiere ingresar la meseta del modelo de variograma")}
  if(missing(alcance)){stop("se requiere ingresar el alcance del modelo de variograma")}
  if(kriging=="Simple" & missing(media)){stop("se requiere ingresar la media para el kriging Simple")}
  if(missing(media)){media=0}
  if(missing(max.num.pares)){stop("se requiere ingresar el maximo numero de pares")}
  # ver integridad de los parametros pedidos
  if(is.null(nrow(sondajes))){stop("verificar integridad de la base de sondajes")}
  if(length(coordenadas.sondajes)<2 | length(coordenadas.sondajes)>3){stop("verificar integridad de las coordenadas")}
  if(!is.character(variable)){stop("la variable debe ser un caracter")}
  if(is.null(nrow(sondajes))){stop("verificar integridad de la base de localidades de prediccion")}
  if(length(coordenadas.localidades)<2 | length(coordenadas.localidades)>3){stop("verificar integridad de las coordenadas")}
  if(!is.character(kriging)){stop("el tipo de kriging debe ser un caracter")}
  if(!is.character(variograma)){stop("el modelo de variograma puede ser: Exponencial, Esferico o Gaussiano")}
  # calcular vecinos
  dist.max=apply(sondajes[,coordenadas.sondajes],2,range)[2,]-apply(sondajes[,coordenadas.sondajes],2,range)[1,]
  vecinos=vecindad(sondajes = sondajes,
                   coordenadas.sondajes = coordenadas.sondajes,
                   localidades = localidades,
                   coordenadas.localidades = coordenadas.localidades,
                   distancia.maxima = min(dist.max))
  # hacer kriging
  predicciones=rep(NA,nrow(localidades))
  varianza=rep(NA,nrow(localidades))
  pares=rowSums(vecinos)
  pb=txtProgressBar(min = 1, max = nrow(localidades), style = 3)
  # kriging ordinario
  if(kriging=="Ordinario"){
    for(ii in 1:nrow(localidades)){
      if(pares[ii]!=0){
        # escoger solo hasta max.num.pares
        ll=sondajes[as.logical(vecinos[ii,]),]
        if(nrow(ll)>max.num.pares){
          dt=0
          for(jj in 1:length(coordenadas.sondajes)){
            dt=dt+(ll[,coordenadas.sondajes[jj]]-localidades[ii,coordenadas.sondajes[jj]])**2
          }
          orden=rank(sqrt(dt))
          ll=ll[orden<=max.num.pares,]
          pares[ii]=max.num.pares
        }else{
          orden=1:nrow(ll)
        }
        coord=rbind(ll[,coordenadas.sondajes],localidades[ii,])
        dd=as.matrix(dist(coord))
        if(variograma=="Exponencial"){
          vxx=pepa+meseta*(1-exp(-3*dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance))
          vxy=pepa+meseta*(1-exp(-3*dd[nrow(coord),][-nrow(coord)]/alcance))
        }
        if(variograma=="Gaussiano"){
          vxx=pepa+meseta*(1-exp(-3*(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance)**2))
          vxy=pepa+meseta*(1-exp(-3*(dd[nrow(coord),][-nrow(coord)]/alcance)**2))
        }
        if(variograma=="Esferico"){
          vxx=ifelse(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]<=alcance,
                     pepa+meseta*(1.5*dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance-0.5*(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance)**3),
                     pepa+meseta)
          vxy=ifelse(dd[nrow(coord),][-nrow(coord)]<=alcance,
                     pepa+meseta*(1.5*dd[nrow(coord),][-nrow(coord)]/alcance-0.5*(dd[nrow(coord),][-nrow(coord)]/alcance)**3),
                     pepa+meseta)
        }
        vxx=rbind(cbind(vxx,rep(1,nrow(coord)-1)),c(rep(1,nrow(coord)-1),0))
        vxy=matrix(c(vxy,1),nrow(coord),1)
        # if(det(vxx)>0.001){
        lambda=as.numeric(solve(vxx)%*%vxy)
        zz=as.numeric(sondajes[as.logical(vecinos[ii,]),variable][orden<=max.num.pares])
        predicciones[ii]=sum(lambda[-nrow(coord)]*zz)
        varianza[ii]=sum(lambda[-nrow(coord)]*vxy[-nrow(coord),1])-lambda[nrow(coord)]
        # }
      }
      # actualizar progreso
      setTxtProgressBar(pb, ii)
    }
  }else{
    for(ii in 1:nrow(localidades)){
      if(pares[ii]!=0){
        # escoger solo hasta max.num.pares
        ll=sondajes[as.logical(vecinos[ii,]),]
        if(nrow(ll)>max.num.pares){
          dt=0
          for(jj in 1:length(coordenadas.sondajes)){
            dt=dt+(ll[,coordenadas.sondajes[jj]]-localidades[ii,coordenadas.sondajes[jj]])**2
          }
          orden=rank(sqrt(dt))
          ll=ll[orden<=max.num.pares,]
          pares[ii]=max.num.pares
        }else{
          orden=1:nrow(ll)
        }
        coord=rbind(ll[,coordenadas.sondajes],localidades[ii,])
        dd=as.matrix(dist(coord))
        if(variograma=="Exponencial"){
          vxx=pepa+meseta*exp(-3*dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance)
          vxy=pepa+meseta*exp(-3*dd[nrow(coord),][-nrow(coord)]/alcance)
        }
        if(variograma=="Gaussiano"){
          vxx=pepa+meseta*exp(-3*(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance)**2)
          vxy=pepa+meseta*exp(-3*(dd[nrow(coord),][-nrow(coord)]/alcance)**2)
        }
        if(variograma=="Esferico"){
          vxx=ifelse(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]<=alcance,
                     (pepa+meseta)-pepa-meseta*(1.5*dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance-0.5*(dd[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcance)**3),
                     pepa)
          vxy=ifelse(dd[nrow(coord),][-nrow(coord)]<=alcance,
                     (pepa+meseta)-pepa-meseta*(1.5*dd[nrow(coord),][-nrow(coord)]/alcance-0.5*(dd[nrow(coord),][-nrow(coord)]/alcance)**3),
                     pepa)
        }
        vxy=matrix(vxy,nrow(coord)-1,1)
        # if(det(vxx)>0.001){
        lambda=as.numeric(solve(vxx)%*%vxy)
        a=(1-sum(lambda))*media
        zz=as.numeric(sondajes[as.logical(vecinos[ii,]),variable][orden<=max.num.pares])
        predicciones[ii]=a+sum(lambda*zz)
        varianza[ii]=pepa+meseta-sum(lambda*as.numeric(vxy))
        # }
      }
      # actualizar progreso
      setTxtProgressBar(pb, ii)
    }
  }
  # devolver resultado
  return(data.frame(predicciones=predicciones,
                    varianza=varianza,
                    pares=pares))
}
