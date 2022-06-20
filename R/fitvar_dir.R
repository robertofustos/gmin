#' @title Ajustar el variograma experimental direccional
#' @description Esta funcion permite ajustar el variograma experimental direccional
#' @param variograma corresponde a los variogramas experimentales direccional
#' @param lags corresponde a los lags de los variogramas entregados
#' @param pares corresponde al numero de pares de cada lag de los variogramas direccional
#' @param modelos corresponde a los modelos teoricos a ajustar a los variogramas experimentales.
#' Los modelos permitidos son "Pepa","Exponencial","Esferico" y "Gaussiano".
#' @param nug corresponde a un valor de verdadero o falso, indicando si se incluira una
#' pepa en los variogramas a ajustar o no.
#' @details Nada
#' @examples
#' @export
fitvar_dir=function(variograma,lags,pares,modelos,nug){
  res=list()
  for(ii in 1:ncol(variograma)){res[[ii]]=fitvar_omni(variograma[,ii],lags[,ii],pares[,ii],modelos[ii],nug)}
  # encontrar la pepa minima
  min_pepa=c()
  for(ii in 1:ncol(variograma)){min_pepa[ii]=res[[ii]]$pepa}
  min_pepa=min_pepa[min_pepa==min(min_pepa)][1]
  # corregir cada meseta en base a min_pepa
  for(ii in 1:ncol(variograma)){
    res[[ii]]$meseta=res[[ii]]$meseta+(res[[ii]]$pepa-min_pepa)
  }
  # corregir todas las pepas
  for(ii in 1:ncol(variograma)){
    res[[ii]]$pepa=min_pepa
  }
  return(res)
}
