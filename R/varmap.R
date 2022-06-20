#' @import rgl
#' @import grDevices
#' @import graphics
#' @import MBA
#' @title Calcular el mapa variografico
#' @description Esta funcion permite calcular el mapa variografico experimental direccional
#' @param drillhole corresponde a la base de datos con las coordenadas y la variable a utilizar
#' @param coordinates corresponde a los nombres de las coordenadas
#' @param variable corresponde al nombre de la variable a utilizar
#' @param lags es el numero de lags del mapa variografico
#' @param maxdist es la maxima distancia a la cual se calcula el mapa variografico
#' @details Esta funcion sirve para crear un mapa variografico 2D o 3D
#' @examples
#' @export
varmap=function(drillhole,coordinates,variable,lags,maxdist){
  cutoff=maxdist
  # funcion extra
  {
    image.scale <- function(z, zlim, col = heat.colors(12),
                            breaks, horiz=TRUE,
                            ylim=NULL, xlim=NULL, ...){
      if(!missing(breaks)){
        if(length(breaks) != (length(col)+1)){stop("must
                                                   have one more break than colour")}
      }
      if(missing(breaks) & !missing(zlim)){
        breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
      }
      if(missing(breaks) & missing(zlim)){
        zlim <- range(z, na.rm=TRUE)
        zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)
        zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
        breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
      }
      poly <- vector(mode="list", length(col))
      for(i in seq(poly)){
        poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
      }
      xaxt <- ifelse(horiz, "s", "n")
      yaxt <- ifelse(horiz, "n", "s")
      if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
      if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
      if(missing(xlim)) xlim=XLIM
      if(missing(ylim)) ylim=YLIM
      plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt,
           yaxt=yaxt, xaxs="i", yaxs="i", ...)
      for(i in seq(poly)){
        if(horiz){
          polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
        }
        if(!horiz){
          polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
        }
      }
    }
    calc_points=function(azi,dip,rango){
      z=sin(dip*pi/180)*rango
      rxy=cos(dip*pi/180)*rango
      if((0<=azi & azi<=90) | azi==360){
        x= sin(azi*pi/180)*rxy
        y= cos(azi*pi/180)*rxy
      }else if(90<azi & azi<=180){
        x= cos((azi-90)*pi/180)*rxy
        y=-sin((azi-90)*pi/180)*rxy
      }else if(180<azi & azi<=270){
        x=-sin((azi-180)*pi/180)*rxy
        y=-cos((azi-180)*pi/180)*rxy
      }else if(270<azi & azi<360){
        x=-cos((azi-270)*pi/180)*rxy
        y= sin((azi-270)*pi/180)*rxy
      }
      return(as.numeric(c(x,y,z)))
    }
    ellipse3d.axes=function (x, centre = c(0, 0, 0),
                             scale = c(1, 1, 1), level = 0.95, t =
                               sqrt(qchisq(level, 3)), which = 1:3, ...){
      stopifnot(is.matrix(x)) # should test for square, symmetric
      cov <- x[which, which]
      eigen <- eigen(cov)
      # coordinate axes, (-1, 1), in pairs
      axes <- matrix(
        c(0, 0, -1,   0, 0, 1,
          0, -1, 0,   0, 1, 0,
          -1, 0, 0,    1, 0, 0),  6, 3, byrow=TRUE)
      # transform to PC axes
      axes <- axes %*% sqrt(diag(eigen$values)) %*% t(eigen$vectors)
      # result <- scale3d(axes, t, t, t)
      # if (!missing(scale))
      #   if (length(scale) != 3) scale <- rep(scale, length.out=3)
      # result <- scale3d(result, scale[1], scale[2], scale[3])
      # if (!missing(centre))
      #   if (length(centre) != 3) scale <- rep(centre, length.out=3)
      # result <- translate3d(result, centre[1], centre[2], centre[3])
      # segments3d(result, ...)
      # invisible(result)
    }
    fit_ellipsoid=function(data){
      a0=max(data[,1])
      b0=max(data[,2])
      c0=max(data[,3])
      ang_x=runif(1,0,360)
      ang_y=runif(1,0,360)
      ang_z=runif(1,0,360)  #
      psi=15
      error=1000000000000
      for(ii in 1:10000){
        # candidatos
        a0_new=rnorm(1,a0,sd(data[,1]))
        b0_new=rnorm(1,b0,sd(data[,2]))
        c0_new=rnorm(1,c0,sd(data[,3]))
        ang_x_new=360*rbeta(1,ang_x/360*psi,
                            (1-ang_x/360)*psi)
        ang_y_new=360*rbeta(1,ang_y/360*psi,
                            (1-ang_y/360)*psi)
        ang_z_new=360*rbeta(1,ang_z/360*psi,
                            (1-ang_z/360)*psi)
        # error actual
        rot_x=matrix(c(1,0,0,0,cos(ang_x_new*pi/180),
                       sin(ang_x_new*pi/180),
                       0,-sin(ang_x_new*pi/180),
                       cos(ang_x_new*pi/180)),3,3)
        rot_y=matrix(c(cos(ang_y_new*pi/180),0,
                       -sin(ang_y_new*pi/180),0,1,0,
                       sin(ang_y_new*pi/180),0,
                       cos(ang_y_new*pi/180)),3,3)
        rot_z=matrix(c(cos(ang_z_new*pi/180),
                       sin(ang_z_new*pi/180),0,
                       -sin(ang_z_new*pi/180),
                       cos(ang_x_new*pi/180),0,0,0,1),3,3)
        data_rot=as.matrix(data)%*%rot_x%*%rot_y%*%rot_z
        error_new=sum(abs(((data_rot[,1]/a0_new)**2+
                             (data_rot[,2]/b0_new)**2+(data_rot[,3]/c0_new)**2)-1))
        if(error_new<error){
          error=error_new
          a0=a0_new
          b0=b0_new
          c0=c0_new
          ang_x=ang_x_new
          ang_y=ang_y_new
          ang_z=ang_z_new
        }
      }
      return(c(a0,b0,c0,ang_x,ang_y,ang_z,error))
    }
    calc_directions=function(puntos){
      azimuth=c()
      dip=c()
      for(jj in 1:3){
        x=puntos[jj,1]
        y=puntos[jj,2]
        z=puntos[jj,3]
        if(z<0){x=-x;y=-y;z=-z}
        # calcular azimuth
        if( x==0 & y>0 ){
          azi=0
        }else if( x>0 & y==0 ){
          azi=90
        }else if( x==0 & y<0 ){
          azi=180
        }else if( x<0 & y==0 ){
          azi=270
        }else if( x>0 & y>0 ){
          azi=atan(abs(x/y))*180/pi
        }else if ( x>0 & y<0 ){
          azi=90+atan(abs(y/x))*180/pi
        }else if ( x<0 & y<0 ){
          azi=180+atan(abs(x/y))*180/pi
        }else{
          azi=270+atan(abs(y/x))*180/pi
        }
        # calcular dip
        if( z>0 ){
          dd=atan(abs(z/sqrt(x**2+y**2)))*180/pi
        }else if( z<0 ){
          dd=-atan(abs(z/sqrt(x**2+y**2)))*180/pi
        }else{
          dd=0
        }
        azimuth=c(azimuth,azi)
        dip=c(dip,dd)
      }
      return(cbind(azimuth,dip))
    }
  }
  # 2d o 3d
  if(length(coordinates)==2){
    # particion de ventana grafica
    layout(cbind(cbind(0,rbind(0,matrix(1,6,6),0)),c(0,rep(2,6),0),0))
    # paleta de colores y display
    pal.1=colorRampPalette(c("midnightblue","blue",
                             "dodgerblue","cyan","green",
                             "yellow","orange","red",
                             "red4"), space="rgb")
    # calculo de direcciones
    azimuth=seq(0,165,by=15)
    lag=rep(lags,length(azimuth))
    maxd=rep(maxdist,length(azimuth))
    tol.lag=rep(0.5,length(azimuth))
    tol.dir=rep(5,length(azimuth))
    # calcular variogramas direccionales
    ve=expvar_dir(drillhole   = drillhole,
                  coordinates = coordinates,
                  variable    = variable,
                  azimuth     = azimuth,
                  lags        = lag,
                  maxdist     = maxd,
                  tol.lag     = tol.lag,
                  tol.dir     = tol.dir)
    # ajustar modelos teoricos
    # va=fit_var_d(ve$variograma,ve$lags,ve$pares,modelos=rep("Sph",length(ve$direcciones)),nug=FALSE)
    # escoger tipo de variograma
    # if(variograma=="Teo"){for(ii in 1:length(ve$direcciones)){ve$variograma[,ii]=va[[ii]]$teorico}}
    # determinar posici?n en grafico
    xcoord=c();ycoord=c()
    for(jj in 1:ncol(ve$variograma)){
      xcoord=cbind(xcoord,c(0,ve$lags[,jj])*
                     cos(ve$direcciones[jj]*pi/180))
      ycoord=cbind(ycoord,c(0,ve$lags[,jj])*
                     sin(ve$direcciones[jj]*pi/180))
    }
    dd=cbind(as.numeric(xcoord),as.numeric(ycoord),
             as.numeric(rbind(0,ve$variograma)))
    # replicar para simetria
    dd1=cbind(cbind(as.numeric(xcoord),
                    as.numeric(ycoord))%*%matrix(c(cos(pi),
                                                   sin(pi),-sin(pi),cos(pi)),2,2),
              as.numeric(rbind(0,ve$variograma)))
    dd2=rbind(dd,dd1)
    ll=1.25*max(abs(dd2[,1:2]))
    dd2=rbind(dd2,c(ll,ll,1),c(-ll,-ll,1))
    # interpolacion
    Interp=mba.surf(dd2, 1000, 1000, extend=TRUE)$xyz.est
    Mat=Interp[["z"]]
    minitics=seq(min(Interp[["x"]]),
                 max(Interp[["x"]]), length.out = 1000)
    # hacer NA lo que esta fuera
    eje_1=maxdist
    eje_2=cutoff
    eje_m=min(eje_1,eje_2)
    markNA <- matrix(minitics, ncol = 1000, nrow = 1000)
    # los que estan fuera de todo
    Mat[!sqrt( (markNA ^ 2)/(eje_m**2) +
                 (t(markNA) ^ 2)/(eje_m**2)  ) < 1.01] <- NA
    # los que estan entre la elipse y las direcciones
    Mat[sqrt( (markNA ^ 2)/(eje_m**2) +
                (t(markNA) ^ 2)/(eje_m**2)  ) < 0.99  &
          sqrt( (markNA ^ 2)/(eje_1**2) +
                  (t(markNA) ^ 2)/(eje_2**2)  ) > 1] <- NA
    # plotear
    breaks <- seq(min(Mat,na.rm = TRUE),
                  max(Mat,na.rm = T),length.out=100)
    par(mar=c(1,1,1,1))
    image(seq(dim(Mat)[1]), seq(dim(Mat)[2]), Mat,
          col=pal.1(length(breaks)-1), breaks=breaks,
          xaxt="n", yaxt="n", ylab="",
          xlab="",main=paste("Mapa variografico para ",variable,sep=""),
          axes=FALSE)
    # agregar texto
    text(950,500,"0")
    text(820,820,"45")
    text(500,950,"90")
    text(180,820,"135")
    text(50,500,"180")
    text(180,180,"225")
    text(500,50,"270")
    text(830,180,"315")
    text(950,620,"15")
    text(910,720,"30")
    text(60,620,"165")
    text(100,720,"150")
    text(60,370,"195")
    text(100,270,"210")
    text(950,370,"345")
    text(910,270,"330")
    text(630,60,"285")
    text(740,100,"300")
    text(730,890,"60")
    text(630,930,"75")
    text(370,930,"105")
    text(260,890,"120")
    text(380,60,"255")
    text(280,90,"240")
    # agregar escala
    par(mar=c(1,1,1,1))
    image.scale(Mat, col=pal.1(length(breaks)-1),
                breaks=breaks, horiz=FALSE,las=2)
    box()
    # regresar ventana grafica a la normalidad
    par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
  }else{
    # 3d
    azimuth=seq(0,345,by=15)
    dip=seq(0,90,by=15)
    direcciones=expand.grid(azimuth,dip)
    lag=rep(lags,nrow(direcciones))
    maxd=rep(maxdist,nrow(direcciones))
    tol.lag=rep(0.5,nrow(direcciones))
    tol.dir=rep(7.5,nrow(direcciones))
    ve=expvar_dir(drillhole  = drillhole,
                 coordinates = coordinates,
                 variable    = variable,
                 azimuth     = direcciones[,1],
                 dip         = direcciones[,2],
                 lags        = lag,
                 maxdist     = maxd,
                 tol.lag     = tol.lag,
                 tol.dir     = tol.dir)
    # ajustar modelos teoricos
    va=fitvar_dir(ve$variograma,ve$lags,ve$pares,modelos=rep("Exponencial",length(ve$direcciones)),nug=FALSE)
    rango=c();for(ii in 1:nrow(ve$direcciones)){rango[ii]=va[[ii]]$rango}
    # solucion a valores extremos o malos ajustes
    rango=ifelse(rango>=as.numeric(quantile(rango,0.8)),as.numeric(quantile(rango,0.8)),rango)
    # calcular puntos en la elipse
    puntos=c();for(ii in 1:nrow(direcciones)){puntos=rbind(puntos,calc_points(direcciones[ii,1],direcciones[ii,2],rango[ii]))}
    # completar elipsoide
    rot_z=matrix(c(cos(180*pi/180),
                   sin(180*pi/180),0,
                   -sin(180*pi/180),
                   cos(180*pi/180),0,0,0,1),3,3)
    points_elipse2=puntos
    for(a1 in 1:nrow(points_elipse2)){points_elipse2[a1,]=rot_z%*%matrix(points_elipse2[a1,],3,1);points_elipse2[a1,3]=-points_elipse2[a1,3]}
    points_eli=rbind(puntos,points_elipse2)
    # grafica
    v1=apply(points_eli,2,max)
    lines3d(matrix(c(-v1[1],v1[1],0,0,0,0),2,3),col=2,lwd=2)
    lines3d(matrix(c(0,0,-v1[2],v1[2],0,0),2,3),col=2,lwd=2)
    lines3d(matrix(c(0,0,0,0,-v1[3],v1[3]),2,3),col=2,lwd=2)
    plot3d( ellipse3d(cov(points_eli), centre = apply(points_eli,2,mean), level = 0.65),
            col = "green", alpha = 0.5, add = TRUE,type = "wire")
    uu=ellipse3d.axes(cov(points_eli), centre = c(0, 0, 0),
                      scale = c(1, 1, 1), level = 0.95, t =
                        sqrt(qchisq(0.95, 3)), which = 1:3)
    lines3d(uu[1:2,],col="blue",lwd=2)
    lines3d(uu[3:4,],col="blue",lwd=2)
    lines3d(uu[5:6,],col="blue",lwd=2)
    vv1=uu[c(2,4,6),]
    colnames(vv1)=c("vec1","vec2","vec3")
    print(cbind(calc_directions(uu[c(2,4,6),])))
  }
}
