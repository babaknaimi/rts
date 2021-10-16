\name{cellFromXY}
\alias{cellFromRowCol}
\alias{cellFromXY}
\alias{cellFromRowCol,RasterStackBrickTS,numeric,numeric-method}
\alias{cellFromXY,RasterStackBrickTS-method}

\alias{cellFromRowCol,SpatRasterTS,numeric,numeric-method}
\alias{cellFromXY,SpatRasterTS-method}


\title{Get cell number from row, column or XY}

\description{
Get cell number(s) of a Raster*TS object from row and column numbers or X and Y coordinates.   
}


\usage{
\S4method{cellFromRowCol}{RasterStackBrickTS,numeric,numeric}(object, row, col)
\S4method{cellFromXY}{RasterStackBrickTS}(object, xy)

\S4method{cellFromRowCol}{SpatRasterTS,numeric,numeric}(object, row, col)
\S4method{cellFromXY}{SpatRasterTS}(object, xy)
}


\arguments{
  \item{object}{SpatRaster}
  \item{col}{integer. column number(s)}
  \item{row}{integer row number(s)}
  \item{xy}{matrix of x and y coordinates}
}
  


\details{
These functions are essentially a wrapper to \code{\link[raster]{cellFromRowCol}} and  \code{\link[raster]{cellFromXY}}
in \pkg{raster} package, work with \code{Raster*TS} objects.
}

\value{
row, column or cell number(s). cellFromLine and cellFromPolygon return a list.
}

\author{Babak Naimi
\email{naimi.b@gmail.com}
\url{http://r-gis.net}
}


\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

rt <- rts(file) # read the ndvi time series from the specified file
cellFromRowCol(rt,15,20)

cellFromRowCol(rt,c(16:20),c(11:15))

cellFromXY(rt,c(645000,57345000))
}

}

\keyword{spatial}