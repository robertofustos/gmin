#' @title Compositar sondajes
#' @description Esta funcion permite compositar sondajes
#' @param drillhole corresponde a una salida de la funcion import_drillhole
#' @param composite corresponde al tamano del composito
#' @details Esta funcion sirve para compositar sondajes
#' @examples
#' @export
composite_drillhole=function(drillhole,composite){
  # get variable type from drillhole
  vt=c()
  for(ii in 1:ncol(drillhole$drillhole)){vt[ii]=class(drillhole$drillhole[,ii])}
  # composite by drillhole
  out=list()
  for(ii in 1:length(drillhole$by_drillhole)){
    # partial drillhole
    aux=drillhole$by_drillhole[[ii]]
    # composite by length variable
    breaks=cut(aux$length,seq(0,max(aux$length),by=composite))
    # composite easting
    comp_easting=tapply(aux$easting,breaks,mean)
    # composite northing
    comp_northing=tapply(aux$northing,breaks,mean)
    # composite elevation
    comp_elevation=tapply(aux$elevation,breaks,mean)
    # join composites coordinates
    comp_drillhole=cbind(comp_easting,comp_northing,comp_elevation)
    # composite variables
    for(jj in 7:ncol(aux)){
      if(is.numeric(aux[,jj])){
        aux2=as.numeric(tapply(aux[,jj],breaks,mean))
        aux2[is.na(aux2)]=NA
        comp_drillhole=data.frame(comp_drillhole,aux2)
      }else{
        aux2=as.character(tapply(aux[,jj], breaks,
                                 function(s) {
                                   counts <- table(s)
                                   names(counts)[which.max(counts)]
                                 }))
        for(kk in 1:length(aux2)){if(aux2[kk]=="NULL"){aux2[kk]=NA}}
        comp_drillhole=data.frame(comp_drillhole,aux2)
      }
    }
    names(comp_drillhole)[4:ncol(comp_drillhole)]=names(aux)[7:ncol(aux)]
    row.names(comp_drillhole)=1:nrow(comp_drillhole)
    # composite length
    comp_length=tapply(aux$length,breaks,mean)
    comp_drillhole=data.frame(comp_drillhole,length=comp_length)
    # assign variable type
    for(jj in 4:(ncol(comp_drillhole)-1)){
      if        (vt[jj+3]=="character"){
        comp_drillhole[,jj]=as.character(comp_drillhole[,jj])
      } else if (vt[jj+3]=="integer") {
        comp_drillhole[,jj]=as.integer(comp_drillhole[,jj])
      } else {
        comp_drillhole[,jj]=as.numeric(comp_drillhole[,jj])
      }
    }
    out[[ii]]=data.frame(drillhole=aux$drillhole[1],comp_drillhole)
  }
  # join all the drillhole
  all=c()
  for(ii in 1:length(drillhole$by_drillhole)){all=rbind(all,out[[ii]])}
  # make output
  return(list(comp_drillhole=as.data.frame(all),comp_by_drillhole=out))
}
