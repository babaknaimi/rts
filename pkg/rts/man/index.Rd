\name{index}
\alias{index}
\alias{index<-}
\alias{index.RasterStackBrickTS}
\alias{index<-.RasterStackBrickTS}


\title{Extracting and replacing the index of raster time series}

\description{
\code{index} is a generic function for extracting the index of a raster time series (\code{Raster*TS}) object and replacing it.

  \code{index(x) <- value}, can be used to replace index with \code{value}, a vector of the same length as the number of raster layers in \code{Raster*TS} object .}


\usage{
\method{index}{RasterStackBrickTS}(x, ...)

}

\arguments{
  \item{x}{A \code{RasterStack} or \code{RasterBrick} object }
  \item{...}{further arguments passed to methods}

}


\author{Babak Naimi

\email{naimi.b@gmail.com}

\url{http://r-gis.net}
}

\seealso{ \code{\link[xts]{index.xts}}}

\examples{
\dontrun{
file <- system.file("external/ndvi", package="rts")

ndvi <- rts(file) # read the ndvi time series from the specified file

index(ndvi)

}

}

\keyword{ utilities }