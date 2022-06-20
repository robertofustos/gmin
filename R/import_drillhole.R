#' @title Importar sondajes divididos por tablas
#' @description Esta funcion permite importar sondajes que se presentan
#' como tablas segmentadas. Este tipo de tablas se utilizan en diversos software mineros y
#' deben estar en formato data.frame. Estas tablas por lo general corresponden a:
#' @param header contiene el nombre de los sondajes y sus coordenadas
#' @param survey contiene el nombre de los sondajes, sus largos y direcciones
#' @param assays contiene el nombre de los sondajes, sus largos, direcciones y las variables numericas
#' @param litho contiene el nombre de los sondajes, sus largos, direcciones y descripciones geologicas
#' @details Esta funcion sirve para importar sondajes
#' los nombres de las tablas deden ser
#' \itemize{
#' \item \code{header = [drillhole, easting, norhing, elevation]}
#' \item \code{survey = [drillhole, from, to, azimuth, dip]}
#' \item \code{assays = [drillhole, from, to, ...]}
#' \item \code{litho = [drillhole, from, to, ...]}
#' }
#'Usualmente, la base de datos llamada assays contiene informacion sobre variables
#'numericas, las que estan relacionadas con las leyes minerales. La base de datos
#'litho contiene la informacion geologica, la que corresponde a descripciones de
#'tipo categorico sobre el cuerpo mineralizado. La salida corresponde a un archivo en
#'formato data.frame con los sondajes compositados por defecto a 0.1 unidades de medida.
#' @examples
#'data(header_example)
#'data(survey_example)
#'data(assays_example)
#'data(litho_example)
#' # importando solo la vista de los sondajes
#' sondajes=import_drillhole(header, survey)
#' # importando los sondajes con leyes minerales y descripcion geologica
#' sondajes=import_drillhole(header, survey, assays, litho)
#' @export
import_drillhole=function(header,survey,assays,litho){
  # critical parameters in header file
  if(length(names(header))!=4){stop("header data is not the correct size")}
  if(sum(names(header)!=c("drillhole","easting","norhing","elevation"))!=0){stop("variable names in header do not match")}
  # critical parameters in survey file
  if(length(names(survey))!=5){stop("survey data is not the correct size")}
  if(sum(names(survey)!=c("drillhole","from","to","azimuth","dip"))!=0){stop("variable names in survey do not match")}
  # optional files
  if(missing(assays)){use_assays=FALSE}else{
    use_assays=TRUE
    if(sum(names(assays)[1:3]!=c("drillhole","from","to"))!=0){stop("variable names in assays do not match")}
  }
  if(missing(litho)){use_litho=FALSE}else{
    use_litho=TRUE
    if(sum(names(litho)[1:3]!=c("drillhole","from","to"))!=0){stop("variable names in litho do not match")}
  }
  # import header and survey (+6 column in output file)
  total_drillhole=list()
  names_drillhole=names(table(header$drillhole))
  for(ii in 1:length(names_drillhole)){
    # starting point
    collar=subset(header,drillhole==names_drillhole[ii])[,-1]
    # survey information
    direction=subset(survey,drillhole==names_drillhole[ii])
    # make drillhole
    ix=as.double(collar[1])
    iy=as.double(collar[2])
    iz=as.double(collar[3])
    from=as.double(direction[1,]$from)
    to=as.double(direction[1,]$to)
    azimuth=as.double(direction[1,]$azimuth)
    dip=as.double(direction[1,]$dip)
    out_row=as.integer((to-from)/0.1)
    out_points=matrix(0,out_row,4)
    storage.mode(out_points)="double"
    library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
    ini=.Fortran("make_drillhole",
                 ix=ix,iy=iy,iz=iz,
                 from=from,to=to,
                 azimuth=azimuth,dip=dip,
                 out_row=out_row,out_points=out_points,PACKAGE = "all")$out_points
    tot=cbind(as.data.frame(ini),jj=1,names_drillhole[ii])
    if(nrow(direction)>1){
      for(jj in 2:nrow(direction)){
        ix=as.double(tot[nrow(tot),1])
        iy=as.double(tot[nrow(tot),2])
        iz=as.double(tot[nrow(tot),3])
        from=as.double(direction[jj,]$from)
        to=as.double(direction[jj,]$to)
        azimuth=as.double(direction[jj,]$azimuth)
        dip=as.double(direction[jj,]$dip)
        out_row=as.integer((to-from)/0.1)
        out_points=matrix(0,out_row,4)
        storage.mode(out_points)="double"
        library.dynam("all", package="gmin", lib.loc = substr(find.package("gmin"), 1, nchar(find.package("gmin"))-4))
        ini=.Fortran("make_drillhole",
                     ix=ix,iy=iy,iz=iz,
                     from=from,to=to,
                     azimuth=azimuth,dip=dip,
                     out_row=out_row,out_points=out_points,PACKAGE = "all")$out_points
        if(nrow(ini)>1){
          tot=rbind(tot,cbind(as.data.frame(ini),jj,names_drillhole[ii]))
        }else{
          tot=rbind(tot,cbind(as.data.frame(ini),jj,names_drillhole[ii]))
        }
      }
    }
    names(tot)=c("easting","northing","elevation","length","section","drillhole")
    tot=tot[,c(6,5,1:4)]
    total_drillhole[[ii]]=tot
  }
  # import assays (+nvar column in output file)
  if(use_assays){
    for(ii in 1:length(total_drillhole)){
      # extract partial drillhole
      partial_drillhole=total_drillhole[[ii]]
      # extract partial assays information
      partial_assays=subset(assays,drillhole==partial_drillhole$drillhole[1])
      # sort partial assays information by from and to
      partial_assays=partial_assays[order(partial_assays$from, partial_assays$to),]
      # if there are information in assays dataframe
      if(nrow(partial_assays)!=0){
        # search and complete missing information
        miss=c()
        for(jj in 1:(nrow(partial_assays)-1)){
          if((partial_assays$to[jj]-partial_assays$from[jj+1])!=0){
            miss=rbind(miss,c(partial_assays$to[jj],partial_assays$from[jj+1]))
          }
        }
        if(!is.null(miss)){
          assays_variables=ncol(partial_assays)-3
          miss_segment=data.frame(partial_assays$drillhole[1],as.data.frame(miss),matrix(NA,nrow(miss),assays_variables))
          names(miss_segment)=names(partial_assays)
          partial_assays=rbind(partial_assays,miss_segment)
          # sort new partial assays information by from and to
          partial_assays=partial_assays[order(partial_assays$from, partial_assays$to),]
        }
        # make auxiliar vector to assign grade to length vector of partial drillhole
        partial_drillhole$length[1]=partial_drillhole$length[1]+0.01
        aux=as.numeric(cut(partial_drillhole$length,partial_assays$from))
        # join the variables to partial drillhole
        partial_drillhole=cbind(partial_drillhole,partial_assays[aux,4:ncol(partial_assays)])
        rownames(partial_drillhole)=c(1:nrow(partial_drillhole))
        total_drillhole[[ii]]=partial_drillhole
      }else{
        # join the missed variables to partial drillhole
        aux_assays=matrix(NA,nrow(partial_drillhole),ncol(partial_assays)-3)
        aux_assays=as.data.frame(aux_assays)
        names(aux_assays)=names(partial_assays)[4:ncol(partial_assays)]
        partial_drillhole=cbind(partial_drillhole,aux_assays)
        rownames(partial_drillhole)=c(1:nrow(partial_drillhole))
        total_drillhole[[ii]]=partial_drillhole
      }
    }
  }
  # import litho (+nvar column in output file)
  if(use_litho){
    for(ii in 1:length(total_drillhole)){
      # extract partial drillhole
      partial_drillhole=total_drillhole[[ii]]
      # extract partial litho information
      partial_litho=subset(litho,drillhole==partial_drillhole$drillhole[1])
      # if there are information in litho dataframe
      if(nrow(partial_litho)!=0){
        # sort partial litho information by from and to
        partial_litho=partial_litho[order(partial_litho$from, partial_litho$to),]
        # search and complete missing information
        miss=c()
        for(jj in 1:(nrow(partial_litho)-1)){
          if((partial_litho$to[jj]-partial_litho$from[jj+1])!=0){
            miss=rbind(miss,c(partial_litho$to[jj],partial_litho$from[jj+1]))
          }
        }
        if(!is.null(miss)){
          litho_variables=ncol(partial_litho)-3
          miss_segment=data.frame(partial_litho$drillhole[1],as.data.frame(miss),matrix(NA,nrow(miss),litho_variables))
          names(miss_segment)=names(partial_litho)
          partial_litho=rbind(partial_litho,miss_segment)
          # sort new partial litho information by from and to
          partial_litho=partial_litho[order(partial_litho$from, partial_litho$to),]
        }
        # make auxiliar vector to assign grade to length vector of partial drillhole
        partial_drillhole$length[1]=partial_drillhole$length[1]+0.01
        aux=as.numeric(cut(partial_drillhole$length,partial_litho$from))
        # join the variables to partial drillhole
        partial_drillhole=cbind(partial_drillhole,partial_litho[aux,4:ncol(partial_litho)])
        rownames(partial_drillhole)=c(1:nrow(partial_drillhole))
        total_drillhole[[ii]]=partial_drillhole
      }else{
        # join the missed variables to partial drillhole
        aux_litho=matrix(NA,nrow(partial_drillhole),ncol(partial_litho)-3)
        aux_litho=as.data.frame(aux_litho)
        names(aux_litho)=names(partial_litho)[4:ncol(partial_litho)]
        partial_drillhole=cbind(partial_drillhole,aux_litho)
        rownames(partial_drillhole)=c(1:nrow(partial_drillhole))
        total_drillhole[[ii]]=partial_drillhole
      }
    }
  }
  # join the drillhole
  aux=c()
  for(ii in 1:length(total_drillhole)){aux=rbind(aux,total_drillhole[[ii]])}
  # output
  return(list(drillhole=aux,by_drillhole=total_drillhole))
}
