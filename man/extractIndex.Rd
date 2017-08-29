\name{Extract by index}

\docType{methods}


\alias{[,RasterStackBrickTS,Spatial,ANY-method}
\alias{[,RasterStackBrickTS,numeric,ANY-method}
\alias{[,RasterStackBrickTS,Extent,ANY-method}


\title{Exrtract values from raster time series}

\description{
This is a short-hand method that acts the same as \code{\link[rts]{extract}}. This method extracts values from a \code{Raster*TS} object for spatial locations which can be specified by spatial points, lines, polygons, or an Extent (rectangle) object or raster cell number(s). 

}

\section{Methods}{
\describe{

\code{x[i, j]}


Arguments


  \code{x}  is a raster time series (\code{Raster*TS}) object created by \code{\link[rts]{rts}} 
  
  \code{i}  is a \code{\link[sp]{SpatialPoints}*}, \code{\link[sp]{SpatialPolygons}*}, \code{\link[sp]{SpatialLines}}, \code{\link[raster]{Extent}} object, or a vector (representing cell numbers)
  
  \code{j}  is optional; the time index for which the values in raster should be extracted. It can be numeric, timeBased or ISO-8601 style (see details)

}}

\details{
This function uses the \pkg{raster} and \pkg{xts} packages to extract the values in space and subset them in time by specifying ISO-8601 compatible range strings. This allows for natural range-based time queries without requiring prior knowledge of the underlying time object used in construction.

When a raw character vector is used for \code{j}, it is processed as if it was ISO-8601 compliant.  This means that it is parsed from left to right, according to the following specification:

CCYYMMDD HH:MM:SS.ss+

A full description will be expanded from a left-specified truncated one.

Additionally, one may specify range-based queries by simply supplying two time descriptions seperated by a forward slash:

CCYYMMDD HH:MM:SS.ss+/CCYYMMDD HH:MM:SS.ss

The algorithm to parse the above is \code{.parseISO8601} from
the \pkg{xts} package.
}

\value{
\code{rts}.
}

\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[rts]{extract}} and \code{\link[xts]{[.xts}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

n1 <- ndvi[125] # extract the time series values at cell number 125 for all available times

n1

plot(n1)

n2 <- ndvi[125,"/20090101"] # extract the time series values at cell number 125 for 
# all times after 2009-01-01

n2

plot(n2)

n3 <- ndvi[125,"200901/"] # extract the time series values at cell number 125 for all
# times before 2009-01

n4 <- ndvi[10:20,"2008-05-01"] #extract the values at cell numbers of 10:20 for 
# the specified time

n4


}

}

\keyword{ utilities }