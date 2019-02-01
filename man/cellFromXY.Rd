\name{cellFromXY}
\alias{cellFromRowCol}
\alias{cellFromXY}
\alias{cellFromRowCol,RasterStackBrickTS,numeric,numeric-method}
\alias{cellFromXY,RasterStackBrickTS-method}


\title{Get cell number from row, column or XY}

\description{
Get cell number(s) of a Raster*TS object from row and column numbers or X and Y coordinates.   
}

\usage{
cellFromXY(object, xy)
cellFromRowCol(object, row, col,...)
}

\arguments{
  \item{object}{ a raster time series (\code{Raster*TS}) object created by \code{\link[rts]{rts}} }
  \item{col}{column number; or vector of column numbers}
  \item{row}{row number; or vector of row numbers}
  \item{xy}{matrix of x and y coordinates, or a SpatialPoints or SpatialPointsDataFrame object}
  \item{...}{additional arguments (none implemented)}
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

\keyword{ spatial }