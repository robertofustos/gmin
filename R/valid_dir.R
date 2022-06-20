#' @title Validacion cruzada direccional 2D y 3D
#' @description Esta funcion permite hacer validacion cruzada por medio de kriging direccional, para muestras 2D y 3D
#' @param sondajes corresponde a la base de datos con las coordenadas y la variable a utilizar
#' @param coordenadas.sondajes corresponde a los nombres de las coordenadas en los sondajes
#' @param variable corresponde al nombre de la variable a utilizar
#' @param kriging corresponde al tipo de Kriging a realizar, el que puede ser "Simple" o "Ordinario"
#' @param azimuth corresponde a los azimuth de los variogramas direccionales
#' @param dip corresponde a los dip de los variogramas direccionales, solo en el caso 3D
#' @param variogramas corresponde a los modelos de variogramas a utilizar, los que pueden ser "Exponencial", "Esferico" o "Gaussiano"
#' @param pepa corresponde a la pepa de los variogramas a utilizar para la prediccion
#' @param meseta corresponde a las mesetas de los variogramas a utilizar para la prediccion
#' @param alcance corresponde a los alcances de los variogramas a utilizar para la prediccion
#' @param media corresponde a la media de la variable en caso de realizar Kriging Simple
#' @param max.num.pares corresponde al numero maximo de pares para realizar la prediccion
#' @details Esta funcion sirve para sondajes 2D o 3D
#' @examples
#' @export
valid_dir=function(sondajes,coordenadas.sondajes,variable,
                   kriging,azimuth,dip,variogramas,pepa,mesetas,alcances,
                   media,max.num.pares){
  # los que tienen que ir si o si
  if(missing(sondajes)){stop("se requiere ingresar un data.frame con los sondajes")}
  if(missing(coordenadas.sondajes)){stop("se requiere ingresar el nombre de las coordenadas de los sondajes")}
  if(missing(variable)){stop("se requiere ingresar el nombre de la variable a predecir")}
  if(missing(kriging)){stop("se requiere ingresar el tipo de kriging (Ordinario o Simple)")}
  if(missing(azimuth)){stop("se requiere ingresar los azimuth de las direcciones principales")}
  if(length(coordenadas.sondajes)==3 & missing(dip)){stop("se requiere ingresar los dip de las direcciones principales")}
  if(missing(variogramas)){stop("se requiere ingresar los modelos de variograma (Exponencial, Gaussiano o Esferico)")}
  if(missing(pepa)){stop("se requiere ingresar la pepa del modelo de variograma")}
  if(missing(mesetas)){stop("se requiere ingresar la meseta del modelo de variograma")}
  if(missing(alcances)){stop("se requiere ingresar el alcance del modelo de variograma")}
  if(kriging=="Simple" & missing(media)){stop("se requiere ingresar la media para el kriging Simple")}
  if(missing(media)){media=0}
  if(missing(max.num.pares)){stop("se requiere ingresar el maximo numero de pares")}
  # ver integridad de los parametros pedidos
  if(is.null(nrow(sondajes))){stop("verificar integridad de la base de sondajes")}
  if(length(coordenadas.sondajes)<2 | length(coordenadas.sondajes)>3){stop("verificar integridad de las coordenadas")}
  if(!is.character(variable)){stop("la variable debe ser un caracter")}
  if(!is.character(kriging)){stop("el tipo de kriging debe ser un caracter")}
  if(!is.character(variogramas)){stop("el modelo de variograma puede ser: Exponencial, Esferico o Gaussiano")}
  if(length(coordenadas.sondajes)!=length(variogramas)){stop("verificar los modelos de variograma")}
  if(length(coordenadas.sondajes)!=length(mesetas)){stop("verificar las mesetas")}
  if(length(coordenadas.sondajes)!=length(alcances)){stop("verificar los alcances")}
  if(length(coordenadas.sondajes)!=length(azimuth)){stop("verificar los azimuth de las direcciones principales")}
  if(length(coordenadas.sondajes)==2 & missing(dip)){dip=0}
  # calcular vecinos
  dist.max=apply(sondajes[,coordenadas.sondajes],2,range)[2,]-apply(sondajes[,coordenadas.sondajes],2,range)[1,]
  vecinos=vecindad(sondajes = sondajes,
                   coordenadas.sondajes = coordenadas.sondajes,
                   localidades = sondajes,
                   coordenadas.localidades = coordenadas.sondajes,
                   distancia.maxima = min(dist.max))
  # hacer kriging
  predicciones=rep(NA,nrow(sondajes))
  varianza=rep(NA,nrow(sondajes))
  pares=rowSums(vecinos)
  pb=txtProgressBar(min = 1, max = nrow(sondajes), style = 3)
  # tipo de kriging: ordinario o simple
  if(kriging=="Ordinario"){
    for(ii in 1:nrow(sondajes)){
      if(pares[ii]!=0){
        # quitar el mismo
        escoger=as.logical(vecinos[ii,])
        escoger[ii]=FALSE
        # escoger solo hasta max.num.pares
        ll=sondajes[escoger,]
        if(nrow(ll)>max.num.pares){
          dt=0
          for(jj in 1:length(coordenadas.sondajes)){
            dt=dt+(ll[,coordenadas.sondajes[jj]]-sondajes[ii,coordenadas.sondajes[jj]])**2
          }
          orden=rank(sqrt(dt))
          ll=ll[orden<=max.num.pares,]
          pares[ii]=max.num.pares
        }else{
          orden=1:nrow(ll)
        }
        coord=rbind(ll[,coordenadas.sondajes],sondajes[ii,coordenadas.sondajes])
        # comienza la parte direccional
        if(length(coordenadas.sondajes)==3){
          dd=dir_dist_xx(coord,coordenadas.sondajes,azimuth,dip)
        }else{
          dd=dir_dist_xx(coord,coordenadas.sondajes,azimuth)
        }
        vxx=0
        vxy=0
        for(jj in 1:length(coordenadas.sondajes)){
          ddp=dd[,,jj]
          if(variogramas[jj]=="Exponencial"){
            pxx=mesetas[jj]*(1-exp(-3*ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj]))
            pxy=mesetas[jj]*(1-exp(-3*ddp[nrow(coord),][-nrow(coord)]/alcances[jj]))
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
          if(variogramas[jj]=="Gaussiano"){
            pxx=mesetas[jj]*(1-exp(-3*(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj])**2))
            pxy=mesetas[jj]*(1-exp(-3*(ddp[nrow(coord),][-nrow(coord)]/alcances[jj])**2))
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
          if(variogramas[jj]=="Esferico"){
            pxx=ifelse(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]<=alcances[jj],
                       mesetas[jj]*(1.5*ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj]-0.5*(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj])**3),
                       mesetas[jj])
            pxy=ifelse(ddp[nrow(coord),][-nrow(coord)]<=alcances[jj],
                       meseta*(1.5*ddp[nrow(coord),][-nrow(coord)]/alcances[jj]-0.5*(ddp[nrow(coord),][-nrow(coord)]/alcances[jj])**3),
                       mesetas[jj])
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
        }
        vxx=vxx+pepa
        vxy=vxy+pepa
        # lagrangeano
        vxx=rbind(cbind(vxx,rep(1,nrow(coord)-1)),c(rep(1,nrow(coord)-1),0))
        vxy=matrix(c(vxy,1),nrow(coord),1)
        # if(det(vxx)>0.001){
        lambda=as.numeric(solve(vxx)%*%vxy)
        zz=as.numeric(sondajes[escoger,variable][orden<=max.num.pares])
        predicciones[ii]=sum(lambda[-nrow(coord)]*zz)
        varianza[ii]=sum(lambda[-nrow(coord)]*vxy[-nrow(coord),1])-lambda[nrow(coord)]
        # }
      }
      # actualizar progreso
      setTxtProgressBar(pb, ii)
    }
  }else{
    for(ii in 1:nrow(sondajes)){
      if(pares[ii]!=0){
        # quitar el mismo
        escoger=as.logical(vecinos[ii,])
        escoger[ii]=FALSE
        # escoger solo hasta max.num.pares
        ll=sondajes[escoger,]
        if(nrow(ll)>max.num.pares){
          dt=0
          for(jj in 1:length(coordenadas.sondajes)){
            dt=dt+(ll[,coordenadas.sondajes[jj]]-sondajes[ii,coordenadas.sondajes[jj]])**2
          }
          orden=rank(sqrt(dt))
          ll=ll[orden<=max.num.pares,]
          pares[ii]=max.num.pares
        }else{
          orden=1:nrow(ll)
        }
        coord=rbind(ll[,coordenadas.sondajes],sondajes[ii,coordenadas.sondajes])
        # comienza la parte direccional
        if(length(coordenadas.sondajes)==3){
          dd=dir_dist_xx(coord,coordenadas.sondajes,azimuth,dip)
        }else{
          dd=dir_dist_xx(coord,coordenadas.sondajes,azimuth)
        }
        vxx=0
        vxy=0
        for(jj in 1:length(coordenadas.sondajes)){
          ddp=dd[,,jj]
          if(variogramas[jj]=="Exponencial"){
            pxx=mesetas[jj]*(1-exp(-3*ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj]))
            pxy=mesetas[jj]*(1-exp(-3*ddp[nrow(coord),][-nrow(coord)]/alcances[jj]))
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
          if(variogramas[jj]=="Gaussiano"){
            pxx=mesetas[jj]*(1-exp(-3*(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj])**2))
            pxy=mesetas[jj]*(1-exp(-3*(ddp[nrow(coord),][-nrow(coord)]/alcances[jj])**2))
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
          if(variogramas[jj]=="Esferico"){
            pxx=ifelse(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]<=alcances[jj],
                       mesetas[jj]*(1.5*ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj]-0.5*(ddp[1:(nrow(coord)-1),1:(nrow(coord)-1)]/alcances[jj])**3),
                       mesetas[jj])
            pxy=ifelse(ddp[nrow(coord),][-nrow(coord)]<=alcances[jj],
                       mesetas[jj]*(1.5*ddp[nrow(coord),][-nrow(coord)]/alcances[jj]-0.5*(ddp[nrow(coord),][-nrow(coord)]/alcances[jj])**3),
                       mesetas[jj])
            vxx=vxx+pxx
            vxy=vxy+pxy
          }
        }
        vxx=vxx+pepa
        vxy=vxy+pepa
        vxy=matrix(vxy,nrow(coord)-1,1)
        cxy=sum(mesetas)+pepa-vxy
        cxx=sum(mesetas)+pepa-vxx
        # if(det(vxx)>0.001){
        lambda=as.numeric(solve(cxx)%*%cxy)
        a=(1-sum(lambda))*media
        zz=as.numeric(sondajes[escoger,variable][orden<=max.num.pares])
        predicciones[ii]=a+sum(lambda*zz)
        varianza[ii]=pepa+sum(mesetas)-sum(lambda*as.numeric(cxy))
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
