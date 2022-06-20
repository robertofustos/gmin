#' @title Ajustar el variograma experimental omnidireccional
#' @description Esta funcion permite ajustar el variograma experimental omnidireccional
#' @param variograma corresponde al variograma experimental omnidireccional
#' @param lags corresponde a los lags del variograma entregado
#' @param pares corresponde al numero de pares de cada lag del variograma omnidireccional
#' @param modelo corresponde al modelo teorico a ajustar al variograma experimental.
#' Los modelos permitidos son "Pepa","Exponencial","Esferico" y "Gaussiano".
#' @param nug corresponde a un valor de verdadero o falso, indicando si se incluira una
#' pepa en el variograma a ajustar o no.
#' @details Nada
#' @examples
#' @export
fitvar_omni=function(variograma,lags,pares,modelo,nug){
  # corregir valores de variograma con ceros
  variograma=variograma[pares!=0]
  lags=lags[pares!=0]
  pares=pares[pares!=0]
  # definir parametros iniciales
  error_ini=1e23
  teorico=rep(0,length(variograma))
  pepa_ini=min(variograma)
  meseta_ini=max(variograma)
  alcance_ini=mean(lags)
  # algoritmo
  if(nug){
    for(ii in 1:1000){
      # definir los nuevos parametros
      pepa_new=pepa_ini+pepa_ini*runif(1,-0.05,0.05)
      meseta_new=meseta_ini+meseta_ini*runif(1,-0.05,0.05)
      alcance_new=alcance_ini+alcance_ini*runif(1,-0.05,0.05)
      # calcular el variograma
      if(modelo=="Exponencial"){
        teorico=pepa_new+meseta_new*(1-exp(-3*lags/alcance_new))
      }else if(modelo=="Esferico"){
        teorico=ifelse(lags<=alcance_new,pepa_new+meseta_new*(1.5*lags/alcance_new-0.5*(lags/alcance_new)**3),pepa_new+meseta_new)
      }else if(modelo=="Gaussiano"){
        teorico=pepa_new+meseta_new*(1-exp(-3*(lags/alcance_new)**2))
      }
      # calcular el error
      error_new=sum((teorico-variograma)**2)
      # decicion
      if(error_new<=error_ini){
        pepa_ini=pepa_new
        meseta_ini=meseta_new
        alcance_ini=alcance_new
        error_ini=error_new
      }
    }
  }else{
    for(ii in 1:1000){
      # definir los nuevos parametros
      meseta_new=meseta_ini+meseta_ini*runif(1,-0.05,0.05)
      alcance_new=alcance_ini+alcance_ini*runif(1,-0.05,0.05)
      # calcular el variograma
      if(modelo=="Exponencial"){
        teorico=meseta_new*(1-exp(-3*lags/alcance_new))
      }else if(modelo=="Esferico"){
        teorico=ifelse(lags<=alcance_new,meseta_new*(1.5*lags/alcance_new-0.5*(lags/alcance_new)**3),meseta_new)
      }else if(modelo=="Gaussiano"){
        teorico=meseta_new*(1-exp(-3*(lags/alcance_new)**2))
      }
      # calcular el error
      error_new=sum((teorico-variograma)**2)
      # decicion
      if(error_new<=error_ini){
        meseta_ini=meseta_new
        alcance_ini=alcance_new
        error_ini=error_new
      }
    }
  }
  return(list(modelo=modelo,pepa=ifelse(nug,pepa_ini,0),meseta=meseta_ini,rango=alcance_ini))
}
