\name{plot}

\docType{methods}

\alias{plot}
\alias{plot,RasterStackBrickTS-method}
\alias{plot,rts-method}
\alias{plot,RasterStackBrickTS,ANY-method}
\alias{plot,rts,ANY-method}

\title{Plot raster time series}

\description{
Plot raster layers corresponding to specified times, or plot extracted time-series values at a location (cell) or a number of cells as a \code{rts} object. }

\usage{

\S4method{plot}{RasterStackBrickTS,ANY}(x, y, ...)
\S4method{plot}{rts,ANY}(x, y, ...)

}

\arguments{
  \item{x}{ an object of raster time series class (\code{Raster*TS}), or an object of \code{rts} class.}
  \item{y}{ optional. if \code{x} is a \code{Raster*TS} object, this item defines time range specifying which layers of raster time series should be plotted. if \code{x} is an \code{rts} object, this item specifies which column (corresponding to a cell) of time-series data should be plotted (default=1). \code{y='all'} indicates all series in \code{rts} object should be included in plot.  }
  \item{...}{ additional argument as in \code{plot} in \code{raster} package or in \code{graphics} package.}
  }
  
\details{
If \code{x} is a \code{Raster*TS} object:

This function, first, selects the layers corresponding to the time range specified in \code{y} and then call \code{plot} function in \code{raster} package to plot the selected raster layers. If \code{y} is not specified, all layers will be sent to \code{plot} function.

Same as in \code{\link[rts]{extract}} and \code{\link[rts]{subset}} functions, When a raw character vector is used for the \code{y}, it is processed as if it was ISO-8601 compliant.  This means that it is parsed from left to right, according to the following specification:

CCYYMMDD HH:MM:SS.ss+

A full description will be expanded from a left-specified truncated one.

Additionally, one may specify range-based queries by simply supplying two time descriptions seperated by a forward slash:

CCYYMMDD HH:MM:SS.ss+/CCYYMMDD HH:MM:SS.ss


\code{x} can be an \code{rts} object. \code{rts} is a subclass of \code{xts}, created by \code{\link[rts]{extract}} function.
}



\author{Babak Naimi 

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[raster]{plot}}, \code{\link[rts]{extract}}, \code{\link[rts]{subset}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

plot(ndvi,1:4) # plot the first 4 layers in the raster time series

plot(ndvi,"/20010101")

plot(ndvi,"20010101/20010501")

plot(ndvi,"2001-02-01/2001-05-01")

plot(ndvi,"2001-02-01")


n1 <- extract(ndvi,125)# extract the time series values at cell number 125 for all times


plot(n1)

n2 <- extract(ndvi,125,"/20090101") # extract the time series values at cell number 125
# for all times after 2009-01-01

n2

plot(n2)


n3 <- extract(ndvi,125:127,"200901/") # extract the time series values at the specified cell
# numbers for all times before 2009-01

n3

plot(n3) # plot the time-series values for the first series in n3 (i.e cell: 125)

plot(n3,y=1:3) # plot for the 3 series in n3 (i.e cells of 125:127)

plot(n3,y=1:3,col=1)

plot(n3,y=1:3,col=c(1,4,5))

plot(n3,y='all')
}

}

\keyword{methods}
\keyword{spatial}
