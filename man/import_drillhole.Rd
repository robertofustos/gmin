% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_drillhole.R
\name{import_drillhole}
\alias{import_drillhole}
\title{Importar sondajes divididos por tablas}
\usage{
import_drillhole(header, survey, assays, litho)
}
\arguments{
\item{header}{contiene el nombre de los sondajes y sus coordenadas}

\item{survey}{contiene el nombre de los sondajes, sus largos y direcciones}

\item{assays}{contiene el nombre de los sondajes, sus largos, direcciones y las variables numericas}

\item{litho}{contiene el nombre de los sondajes, sus largos, direcciones y descripciones geologicas}
}
\description{
Esta funcion permite importar sondajes que se presentan
como tablas segmentadas. Este tipo de tablas se utilizan en diversos software mineros y
deben estar en formato data.frame. Estas tablas por lo general corresponden a:
}
\details{
Esta funcion sirve para importar sondajes
los nombres de las tablas deden ser
\itemize{
\item \code{header = [drillhole, easting, norhing, elevation]}
\item \code{survey = [drillhole, from, to, azimuth, dip]}
\item \code{assays = [drillhole, from, to, ...]}
\item \code{litho = [drillhole, from, to, ...]}
}
Usualmente, la base de datos llamada assays contiene informacion sobre variables
numericas, las que estan relacionadas con las leyes minerales. La base de datos
litho contiene la informacion geologica, la que corresponde a descripciones de
tipo categorico sobre el cuerpo mineralizado. La salida corresponde a un archivo en
formato data.frame con los sondajes compositados por defecto a 0.1 unidades de medida.
}
\examples{
data(header_example)
data(survey_example)
data(assays_example)
data(litho_example)
# importando solo la vista de los sondajes
sondajes=import_drillhole(header, survey)
# importando los sondajes con leyes minerales y descripcion geologica
sondajes=import_drillhole(header, survey, assays, litho)
}
